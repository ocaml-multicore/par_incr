module RNode = struct
  open Types

  type t = rnode
  type show_fn_wrapper = {fn : 'a. 'a action -> 'a}

  let make ~fn:{fn} = {par = Dummy; has_pending_update = false; fn}

  let mark_dirty t =
    begin
      t.has_pending_update <- true;
      set_mark t.par
    end
end

open Types

type t = comp_tree

let empty = Dummy

let set_parent_exn ~c ~p ~c_par_fn =
  match c with
  | Root _ | Dummy -> failwith "Root/Dummy doesn't have parent"
  | Par nd ->
    c_par_fn nd.par;
    nd.par <- p
  | Seq nd ->
    c_par_fn nd.par;
    nd.par <- p
  | R rnd ->
    c_par_fn rnd.par;
    rnd.par <- p

let rec destroy t =
  match t with
  | Par _ | Seq _ | Root _ ->
    let () = set_exn' ~pre:destroy ~post:ignore t `Left Dummy in
    let () = set_exn' ~pre:destroy ~post:ignore t `Right Dummy in
    ()
  | R r -> r.fn (Remove r)
  | Dummy -> ()

and set_exn' ~pre ~post t dir child =
  if child != Dummy then set_parent_exn ~c_par_fn:ignore ~c:child ~p:t;
  match t with
  | Root r -> begin
    match dir with
    | `Left ->
      pre r.left;
      r.left <- child;
      post r.left
    | `Right ->
      pre r.right;
      r.right <- child;
      post r.right
  end
  | Seq nd -> begin
    match dir with
    | `Left ->
      pre nd.left;
      nd.left <- child;
      post nd.left
    | `Right ->
      pre nd.right;
      nd.right <- child;
      post nd.right
  end
  | Par nd -> begin
    match dir with
    | `Left ->
      pre nd.left;
      nd.left <- child;
      post nd.left
    | `Right ->
      pre nd.right;
      nd.right <- child;
      post nd.right
  end
  | R _ | Dummy -> failwith "R and Dummy nodes don't have left/right child"

(* Set's t's children that lies in dir direction to child *)
let set_exn ?(f = fun _ -> ()) t dir child =
  set_exn' ~post:f
    ~pre:(fun old_c -> if old_c != child then destroy old_c)
    t dir child

let get_sub_tree_exn t =
  match t with
  | Root nd -> (nd.left, nd.right)
  | Par nd -> (nd.left, nd.right)
  | Seq nd -> (nd.left, nd.right)
  | R _ | Dummy -> failwith "R and dummy don't have children"

let prune c =
  let l, r = get_sub_tree_exn c in
  match (l, r) with
  | Dummy, Dummy -> Dummy
  | Dummy, _ -> r
  | _, Dummy -> l
  | _ -> c

let make_root () = Root {left = empty; right = empty}

let make_empty typ =
  let nd =
    match typ with
    | `S -> Seq {par = Dummy; par_mark = false; left = Dummy; right = Dummy}
    | `P -> Par {par = Dummy; par_mark = false; left = Dummy; right = Dummy}
  in
  nd

let make_node ~l ~r typ =
  if l == Dummy then r
  else if r == Dummy then l
  else
    let nd =
      match typ with
      | `S -> Seq {par = Dummy; par_mark = false; left = l; right = r}
      | `P -> Par {par = Dummy; par_mark = false; left = l; right = r}
    in
    set_parent_exn ~c:l ~p:nd ~c_par_fn:ignore;
    set_parent_exn ~c:r ~p:nd ~c_par_fn:ignore;
    nd

let make_rnode rnd = R rnd

let rec propagate_exn comp e =
  match comp with
  | Root root ->
    failwith "propagate should never be called with Root variant of comp_tree"
  | Dummy -> ()
  | Seq snd -> begin
    if snd.par_mark then (
      propagate_exn snd.left e;
      propagate_exn snd.right e;
      snd.par_mark <- false)
    else ()
  end
  | Par pnd -> begin
    if pnd.par_mark then
      let _ =
        e.par_do
          (fun _ -> propagate_exn pnd.left e)
          (fun _ -> propagate_exn pnd.right e)
      in
      pnd.par_mark <- false
    else ()
  end
  | R rnd -> begin
    if rnd.has_pending_update then (
      rnd.fn Update;
      rnd.has_pending_update <- false)
    else ()
  end

let propagate_root comp e =
  match comp with
  | Root root -> begin
    e.run (fun () ->
        propagate_exn root.left e;
        propagate_exn root.right e)
  end
  | _ -> failwith "Cannot propagate destroyed/ill-formed computation"

let set_and_get_exn t dir child =
  set_exn t dir child;
  child

let to_d2 ?(cnt = ref 0) (oc : Out_channel.t) =
  (* let cnt = ref 0 in *)
  let incr_and_get cnt =
    incr cnt;
    !cnt
  in
  let rec to_d2' parent t =
    match t with
    | Dummy -> begin
      let n = incr_and_get cnt in
      Printf.fprintf oc "\n%d: Dummy" n;
      n
    end
    | R rnd -> begin
      assert (rnd.par == parent);
      let n = incr_and_get cnt in
      Printf.fprintf oc
        "\n%d: R {\nshape: sql_table\npending_mod: %s\ndetail: %s }" n
        (Bool.to_string rnd.has_pending_update)
        (rnd.fn Show);
      n
    end
    | _ ->
      let l, r = get_sub_tree_exn t in
      let leftid = to_d2' t l in
      let rightid = to_d2' t r in
      let typ, parent_marked =
        match t with
        | Root _ -> ("Root", "NA")
        | Seq nd ->
          assert (nd.par == parent);
          ("Seq", Bool.to_string nd.par_mark)
        | Par nd ->
          assert (nd.par == parent);
          ("Par", Bool.to_string nd.par_mark)
        | _ -> failwith "Invalid"
      in
      let n = incr_and_get cnt in
      Printf.fprintf oc
        "\n\
         %d: %s {\n\
         shape: sql_table\n\
         par_mark:%s\n\
         }\n\
        \ %d -> %d : %s \n\
        \ %d -> %d : %s " n typ parent_marked n leftid "Left" n rightid "Right";
      n
  in
  to_d2' Dummy

let get_stats t =
  let stats : counter =
    {
      bind = 0;
      map = 0;
      dirty = 0;
      combine = 0;
      par_do = 0;
      r = 0;
      s = 0;
      dummy = 0;
      p = 0;
    }
  in
  let rec f p = function
    | R rnd ->
      assert (rnd.par == p);
      rnd.fn (Count stats);
      stats.r <- stats.r + 1;
      if rnd.has_pending_update then stats.dirty <- stats.dirty + 1
    | (Root {left; right; _} | Par {left; right; _} | Seq {left; right; _}) as t
      -> begin
      f t left;
      f t right;
      let () =
        match t with
        | Par pnd ->
          stats.p <- stats.p + 1;
          if pnd.par_mark then stats.dirty <- stats.dirty + 1;
          assert (pnd.par == p)
        | Seq snd ->
          stats.s <- stats.s + 1;
          if snd.par_mark then stats.dirty <- stats.dirty + 1;
          assert (snd.par == p)
        | _ -> ()
      in
      ()
    end
    | Dummy -> stats.dummy <- stats.dummy + 1
  in
  f Dummy t;
  stats
