type 'a var = 'a Incr.t
type ctx = Rsp.t
type comp_stat = Types.counter

type executor = Types.executor = {
  run : 'a. (unit -> 'a) -> 'a;
  par_do : 'a 'b. (unit -> 'a) -> (unit -> 'b) -> 'a * 'b;
}

type 'a t = ctx -> executor -> 'a var
type 'a computation = {var : 'a var; mutable root : Rsp.t; e : executor}

module RNode = Rsp.RNode

module Var = struct
  type 'a t = 'a var

  let create ?(eq = ( == )) ?(to_s = Utils.undefined) x =
    Incr.create ~eq ~to_s x

  let set t x = Incr.set t x
  let value = Incr.value_exn
  let watch x _ _ = x
  let num_readers t = Incr.num_readers t

  module Syntax = struct
    let ( := ) = set
    let ( ! ) = value
  end
end

let return x _ _ = Var.create x

let map ?(eq = ( == )) ~(fn : 'a -> 'b) (t : 'a t) (ctx : ctx) (e : executor) =
  begin
    let open Types in
    let left = Rsp.make_empty `S in
    let x = t left e in
    let y = Incr.empty ~eq ~to_s:Utils.undefined () in
    let read_fn (type a) : a action -> a = function
      | Update -> Var.set y (Var.value x |> fn)
      | Remove self -> Incr.remove_reader x self
      | Show -> "map, val: " ^ Incr.to_s y
      | Count cntr -> cntr.map <- cntr.map + 1
    in

    let read_x = RNode.make ~fn:RNode.{fn = read_fn} in
    Incr.add_reader x read_x;
    Rsp.set_exn ctx `Left (Rsp.prune left);
    Rsp.set_exn ctx `Right (Rsp.make_rnode read_x);
    read_x.fn Update;
    y
  end

let combine (a : 'a t) (b : 'b t) ctx e =
  begin
    let open Types in
    let ll = Rsp.make_empty `S in
    let lr = Rsp.make_empty `S in
    let x = a ll e in
    let y = b lr e in
    let xy =
      Incr.empty
        ~eq:(Utils.combine_eq (Incr.eq x) (Incr.eq y))
        ~to_s:(Utils.combine_to_s (Incr.get_to_string x) (Incr.get_to_string y))
        ()
    in
    let read_fn_xy (type a) : a action -> a = function
      | Update -> Var.set xy (Var.value x, Var.value y)
      | Remove self ->
        Incr.remove_reader x self;
        Incr.remove_reader y self
      | Show -> "combined var: " ^ Incr.to_s xy
      | Count cntr -> cntr.combine <- cntr.combine + 1
    in
    let read_xy = RNode.make ~fn:RNode.{fn = read_fn_xy} in
    Incr.add_reader x read_xy;
    Incr.add_reader y read_xy;
    let left = Rsp.make_node `S ~l:(Rsp.prune ll) ~r:(Rsp.prune lr) in
    Rsp.set_exn ctx `Left left;
    Rsp.set_exn ctx `Right (Rsp.make_rnode read_xy);
    read_xy.fn Update;
    xy
  end

let par ~left ~right ctx e =
  begin
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
      Incr.empty
        ~eq:(Utils.combine_eq (Incr.eq lres) (Incr.eq rres))
        ~to_s:
          (Utils.combine_to_s (Incr.get_to_string lres)
             (Incr.get_to_string rres))
        ()
    in
    let read_fn_lr_comb (type a) : a action -> a = function
      | Update -> Var.set lr_comb (Var.value lres, Var.value rres)
      | Remove self ->
        Incr.remove_reader lres self;
        Incr.remove_reader rres self
      | Show -> "par var: " ^ Incr.to_s lr_comb
      | Count cntr -> cntr.par_do <- cntr.par_do + 1
    in
    let read_lr = RNode.make ~fn:RNode.{fn = read_fn_lr_comb} in
    Incr.add_reader lres read_lr;
    Incr.add_reader rres read_lr;
    let left = Rsp.make_node `P ~l:ll ~r:lr in
    Rsp.set_exn ctx `Left left;
    Rsp.set_exn ctx `Right (Rsp.make_rnode read_lr);
    read_lr.fn Update;
    lr_comb
  end

let map2 ?(eq = ( == )) ?(mode = `Seq) ~fn x y ctx e =
  let xy =
    match mode with `Seq -> combine x y | `Par -> par ~left:x ~right:y
  in
  map ~eq ~fn:(fun (x, y) -> fn x y) xy ctx e

let value comp = Incr.value_exn comp.var

let dump_tree file c =
  Out_channel.with_open_text file (fun oc -> Rsp.to_d2 oc c.root |> ignore)

let destroy_comp comp =
  Rsp.destroy comp.root;
  comp.root <- Types.Dummy

let delay f c e = f () c e [@@inline]

module Debug = struct
  let attach ~fn t c e =
    let tvar = t c e in
    Incr.attach_to_string tvar fn;
    tvar
end

module Eq = struct
  let attach ~fn t c e =
    let tvar = t c e in
    Incr.attach_eq tvar fn;
    tvar
end

let bind ~fn x ctx e =
  begin
    let open Types in
    let r = Rsp.set_and_get_exn ctx `Right (Rsp.make_empty `S) in
    let ll = Rsp.make_empty `S in
    let xvar = x ll e in
    let y = Incr.empty ~eq:( == ) ~to_s:Utils.undefined () in
    let read_fn_xvar (type a) : a action -> a = function
      | Update -> begin
        let y' = fn (Var.value xvar) in
        let rl = Rsp.make_empty `S in
        let yvar' = y' rl e in
        let () = Rsp.set_exn r `Left (Rsp.prune rl) in
        let read_fn_yvar' (type a) : a action -> a = function
          | Update ->
            Var.set y (Var.value yvar');
            Incr.attach_eq y (Incr.eq yvar');
            Incr.attach_to_string y (Incr.get_to_string yvar')
          | Remove self -> Incr.remove_reader yvar' self
          | Show -> "inner-bind, val: " ^ Incr.to_s y
          | Count _ -> () (*Not going to count this*)
        in
        let read_yvar' = RNode.make ~fn:RNode.{fn = read_fn_yvar'} in
        Incr.add_reader yvar' read_yvar';
        Rsp.set_exn r `Right (Rsp.make_rnode read_yvar');
        read_yvar'.fn Update
      end
      | Remove self -> Incr.remove_reader xvar self
      | Show -> "outer-bind, val: " ^ Incr.to_s xvar
      | Count cntr -> cntr.bind <- cntr.bind + 1
    in
    let read_xvar = RNode.make ~fn:RNode.{fn = read_fn_xvar} in
    Incr.add_reader xvar read_xvar;
    let l = Rsp.make_node `S ~l:(Rsp.prune ll) ~r:(Rsp.make_rnode read_xvar) in
    Rsp.set_exn ctx `Left l;
    read_xvar.fn Update;
    y
  end

let run ~(executor : executor) f =
  begin
    let root = Rsp.make_root () in
    let run' = executor.run in
    let f' () = f root executor in
    let var = run' f' in
    {root; var; e = executor}
  end

let propagate comp =
  begin
    Rsp.propagate_root comp.root comp.e
  end

let get_stat comp = Rsp.get_stats comp.root

module Syntax = struct
  let ( let+ ) v f = map ~fn:f v
  let ( and+ ) x y = combine x y
  let ( let* ) v f = bind ~fn:f v
  let ( and* ) = ( and+ )
  let ( let& ) v f = map ~fn:f v
  let ( and& ) left right = par ~left ~right
end
