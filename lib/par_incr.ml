type 'a var = {
  mutable value : 'a;
  mutable cutoff : 'a Types.cutoff;
  mutable to_string : 'a -> string;
  readers : Reader_list.t;
}

type ctx = Rsp.t
type comp_stat = Types.counter

type executor = Types.executor = {
  run : 'a. (unit -> 'a) -> 'a;
  par_do : 'a 'b. (unit -> 'a) -> (unit -> 'b) -> 'a * 'b;
}

type 'a t = ctx -> executor -> 'a var
type 'a computation = {var : 'a var; mutable root : Rsp.t; e : executor}

module RNode = Rsp.RNode

module Cutoff = struct
  type 'a t = 'a Types.cutoff =
    | Never
      (*Represented as 0, can be optimized to have no branch when we match on
        this and check equality. We check is_same in Var.set, which would have
        to be false if you don't want the computation to be cut-off. So we have
        to return false*)
    | Always
      (*Represented as 1, can be optimized to have no branch when we match on
        this and check equality. This means things are always equal and hence we
        should have is_same be true.*)
    | Phys_equal
    | Eq of ('a -> 'a -> bool)
    | F of (oldval:'a -> newval:'a -> bool)

  let[@inline] attach cutoff t ctx e =
    let tvar = t ctx e in
    tvar.cutoff <- cutoff;
    tvar
end

module Var = struct
  type 'a t = 'a var

  let null = Obj.magic (ref ())

  let[@inline] create ?(cutoff = Cutoff.Phys_equal) ?(to_s = Utils.undefined) x
      =
    let v =
      {value = x; cutoff; to_string = to_s; readers = Reader_list.empty ()}
    in
    v

  let[@inline] empty ~(cutoff : 'a Cutoff.t) ~(to_s : 'a -> string) () =
    {value = null; cutoff; to_string = to_s; readers = Reader_list.empty ()}

  let[@inline] set ({cutoff; value; readers; _} as t) x =
    if value == null then t.value <- x
    else
      let is_same =
        match cutoff with
        | Never -> false
        | Always ->
          true
          (*Compiler can optimize Always and Never cases by just returning it
            directly because of the way they are defined. Always will be
            represented as 1(same as true) and Never will be represented as
            0(same as false) *)
        | Phys_equal -> x == value
        | Eq f -> f value x
        | F f -> f ~oldval:value ~newval:x
      in
      if not is_same then (
        t.value <- x;
        Reader_list.iter readers Rsp.RNode.mark_dirty)

  let[@inline] value t =
    if t.value == null then
      failwith "Something is wrong, trying to access uninitialized value"
    else t.value

  let[@inline] cutoff {cutoff; _} = cutoff

  let[@inline] attach_cutoff (t : 'a t) cutoff =
    if t.cutoff != cutoff then t.cutoff <- cutoff

  let[@inline] attach_to_string (t : 'a t) f =
    if t.to_string != f then t.to_string <- f else ()

  let[@inline] add_reader {readers; _} r = Reader_list.add_reader readers r

  let[@inline] remove_reader {readers; _} r =
    Reader_list.remove_reader readers r

  let[@inline] num_readers {readers; _} = Reader_list.length readers
  let[@inline] to_s t = t |> value |> t.to_string
  let[@inline] get_to_string {to_string; _} = to_string
  let[@inline] watch x _ _ = x

  module Syntax = struct
    let ( := ) = set
    let ( ! ) = value
  end
end

let return x _ _ = Var.create x

let map ?(cutoff = Cutoff.Phys_equal) ~(fn : 'a -> 'b) (t : 'a t) (ctx : ctx)
    (e : executor) =
  let open Types in
  let left = Rsp.make_empty `S in
  let x = t left e in
  let y = Var.empty ~cutoff ~to_s:Utils.undefined () in
  let read_fn (type a) : a action -> a = function
    | Update -> Var.set y (Var.value x |> fn)
    | Remove self -> Var.remove_reader x self
    | Show -> "map: " ^ Var.to_s y
    | Count cntr -> cntr.map <- cntr.map + 1
  in

  let read_x = RNode.make ~fn:RNode.{fn = read_fn} in
  Var.add_reader x read_x;
  Rsp.set_exn ctx `Left (Rsp.prune left);
  Rsp.set_exn ctx `Right read_x;
  read_x.fn Update;
  y

let combine (a : 'a t) (b : 'b t) ctx e =
  let open Types in
  let ll = Rsp.make_empty `S in
  let lr = Rsp.make_empty `S in
  let x = a ll e in
  let y = b lr e in
  let xy =
    Var.empty ~cutoff:Phys_equal
      ~to_s:(Utils.combine_to_s (Var.get_to_string x) (Var.get_to_string y))
      ()
  in
  let read_fn_xy (type a) : a action -> a = function
    | Update -> Var.set xy (Var.value x, Var.value y)
    | Remove self ->
      Var.remove_reader x self;
      Var.remove_reader y self
    | Show -> "combine: " ^ Var.to_s xy
    | Count cntr -> cntr.combine <- cntr.combine + 1
  in
  let read_xy = RNode.make ~fn:RNode.{fn = read_fn_xy} in
  Var.add_reader x read_xy;
  Var.add_reader y read_xy;
  let left = Rsp.make_node `S ~l:(Rsp.prune ll) ~r:(Rsp.prune lr) in
  Rsp.set_exn ctx `Left left;
  Rsp.set_exn ctx `Right read_xy;
  read_xy.fn Update;
  xy

let par ~left ~right ctx e =
  let open Types in
  let (lres, ll), (rres, lr) =
    e.par_do
      (fun () ->
        let ll = Rsp.make_empty `S in
        let lres = left ll e in
        (lres, Rsp.prune ll))
      (fun () ->
        let lr = Rsp.make_empty `S in
        let rres = right lr e in
        (rres, Rsp.prune lr))
  in
  let lr_comb =
    Var.empty ~cutoff:Phys_equal
      ~to_s:
        (Utils.combine_to_s (Var.get_to_string lres) (Var.get_to_string rres))
      ()
  in
  let read_fn_lr_comb (type a) : a action -> a = function
    | Update -> Var.set lr_comb (Var.value lres, Var.value rres)
    | Remove self ->
      Var.remove_reader lres self;
      Var.remove_reader rres self
    | Show -> "par: " ^ Var.to_s lr_comb
    | Count cntr -> cntr.par_do <- cntr.par_do + 1
  in
  let read_lr = RNode.make ~fn:RNode.{fn = read_fn_lr_comb} in
  Var.add_reader lres read_lr;
  Var.add_reader rres read_lr;
  let left = Rsp.make_node `P ~l:ll ~r:lr in
  Rsp.set_exn ctx `Left left;
  Rsp.set_exn ctx `Right read_lr;
  read_lr.fn Update;
  lr_comb

let map2 ?(cutoff = Types.Phys_equal) ?(mode = `Seq) ~fn x y ctx e =
  let xy =
    match mode with `Seq -> combine x y | `Par -> par ~left:x ~right:y
  in
  map ~cutoff ~fn:(fun (x, y) -> fn x y) xy ctx e

let value comp = Var.value comp.var

let dump_tree file c =
  Out_channel.with_open_text file (fun oc -> Rsp.to_d2 oc c.root |> ignore)

let destroy_comp comp =
  let root = comp.root in
  comp.root <- Types.nil_tree;
  Rsp.destroy root

let[@inline] delay f c e = f () c e

module Debug = struct
  let attach ~fn t c e =
    let tvar = t c e in
    Var.attach_to_string tvar fn;
    tvar
end

let bind ~fn x ctx e =
  let open Types in
  let r = Rsp.set_and_get_exn ctx `Right (Rsp.make_empty `S) in
  let ll = Rsp.make_empty `S in
  let xvar = x ll e in
  let y = Var.empty ~cutoff:Phys_equal ~to_s:Utils.undefined () in
  let read_fn_xvar (type a) : a action -> a = function
    | Update -> begin
      let y' = fn (Var.value xvar) in
      let rl = Rsp.make_empty `S in
      let yvar' = y' rl e in
      let () = Rsp.set_exn r `Left (Rsp.prune rl) in
      let read_fn_yvar' (type a) : a action -> a = function
        | Update ->
          Var.set y (Var.value yvar');
          Var.attach_cutoff y (Var.cutoff yvar');
          Var.attach_to_string y (Var.get_to_string yvar')
        | Remove self -> Var.remove_reader yvar' self
        | Show -> "inner-bind: " ^ Var.to_s y
        | Count _ -> () (*Not going to count this*)
      in
      let read_yvar' = RNode.make ~fn:RNode.{fn = read_fn_yvar'} in
      Var.add_reader yvar' read_yvar';
      Rsp.set_exn r `Right read_yvar';
      read_yvar'.fn Update
    end
    | Remove self -> Var.remove_reader xvar self
    | Show -> "outer-bind: " ^ Var.to_s xvar
    | Count cntr -> cntr.bind <- cntr.bind + 1
  in
  let read_xvar = RNode.make ~fn:RNode.{fn = read_fn_xvar} in
  Var.add_reader xvar read_xvar;
  let l = Rsp.make_node `S ~l:(Rsp.prune ll) ~r:read_xvar in
  Rsp.set_exn ctx `Left l;
  read_xvar.fn Update;
  y

let run ~(executor : executor) f =
  let root = Rsp.make_root () in
  let run' = executor.run in
  let f' () = f root executor in
  let var = run' f' in
  {root; var; e = executor}

let propagate comp = Rsp.propagate_root comp.root comp.e
let get_stat comp = Rsp.get_stats comp.root

module Syntax = struct
  let ( let+ ) v f = map ~fn:f v
  let ( and+ ) x y = combine x y
  let ( let* ) v f = bind ~fn:f v
  let ( and* ) = ( and+ )
  let ( let& ) v f = map ~fn:f v
  let ( and& ) left right = par ~left ~right
end
