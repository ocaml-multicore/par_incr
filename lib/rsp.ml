module RNode = struct
  open Types

  type t = rnode
  type show_fn_wrapper = {fn : 'a. 'a action -> 'a} [@@unboxed]

  let[@inline] make ~fn:{fn} =
    {
      par = Types.nil_tree;
      flags = Utils.r_flag;
      left = Types.nil_tree;
      right = Types.nil_tree;
      fn;
    }

  let[@inline] mark_dirty ({flags; par; _} as t) =
    begin
      if not (Utils.is_marked flags) then (
        t.flags <- Utils.make_marked flags;
        set_mark par)
    end
end

open Types

type t = comp_tree

let empty = Types.nil_tree
let nil_tree = Types.nil_tree

let[@inline] set_parent_exn ~c ~p =
  if c != nil_tree && not (Utils.is_root c.flags) then c.par <- p
  else Utils.impossible ()

let rec destroy t =
  let flags = t.flags in
  if Utils.is_rnode flags then t.fn (Remove t)
  else
    let {left; right; _} = t in
    if left != nil_tree then (
      t.left <- nil_tree;
      destroy left);
    if right != nil_tree then (
      t.right <- nil_tree;
      destroy right)

let[@inline] set_exn t dir child =
  if child != nil_tree then set_parent_exn ~c:child ~p:t;
  let flag = Utils.masked t.flags in
  if flag = Utils.r_flag then failwith "R nodes don't have left/right child"
  else begin
    let {left; right; _} = t in
    match dir with
    | `Left -> begin
      if left != child then (
        t.left <- child;
        if left != nil_tree then destroy left)
    end
    | `Right -> begin
      if right != child then (
        t.right <- child;
        if right != nil_tree then destroy right)
    end
  end

let[@inline] prune c =
  if c == nil_tree then nil_tree
  else begin
    let flag = Utils.masked c.flags in
    if flag = Utils.r_flag then c
    else begin
      let {left; right; _} = c in
      if left == nil_tree && right == nil_tree then nil_tree
      else if left == nil_tree then right
      else if right == nil_tree then left
      else c
    end
  end

let[@inline] make_root () =
  {
    right = empty;
    left = empty;
    flags = Utils.root_flag;
    par = nil_tree;
    fn = Types.default_action;
  }

let[@inline] make_empty typ =
  let flag = match typ with `S -> Utils.s_flag | `P -> Utils.p_flag in
  {
    right = empty;
    left = empty;
    flags = flag;
    par = nil_tree;
    fn = Types.default_action;
  }

let[@inline] make_node ~l ~r typ =
  if l == nil_tree then r
  else if r == nil_tree then l
  else
    let flag = match typ with `S -> Utils.s_flag | `P -> Utils.p_flag in
    let nd =
      {
        right = r;
        left = l;
        flags = flag;
        par = nil_tree;
        fn = Types.default_action;
      }
    in
    set_parent_exn ~c:l ~p:nd;
    set_parent_exn ~c:r ~p:nd;
    nd

(* let make_rnode rnd = R rnd *)

let[@inline] is_marked c = c != nil_tree && Utils.is_marked c.flags

let rec propagate_exn comp e =
  (* if comp == nil_tree then () *)
  (* else  *)
  begin
    let {left; right; fn; flags; _} = comp in
    let masked_flag = Utils.masked flags in
    (* if Utils.is_marked flag then begin *)
    if masked_flag = Utils.r_flag then fn Update
    else if masked_flag = Utils.p_flag && is_marked left && is_marked right then
      let _ =
        e.par_do
          (fun () -> propagate_exn left e)
          (fun () -> propagate_exn right e)
      in
      ()
    else begin
      (* Root is impossible case *)
      if masked_flag = Utils.root_flag then Utils.impossible ();
      if is_marked left then propagate_exn left e;
      if is_marked right then propagate_exn right e
    end;
    (* end; *)
    comp.flags <- masked_flag
  end

let propagate_root comp e =
  if comp == nil_tree then
    failwith "Cannot propagate destroyed/ill-formed computation"
  else begin
    let {left; right; flags; par; _} = comp in
    assert (Utils.is_root flags);
    assert (par == nil_tree);
    if Utils.is_marked comp.flags then
      e.run (fun () ->
          if is_marked left then propagate_exn left e;
          if is_marked right then propagate_exn right e)
  end

let[@inline] set_and_get_exn t dir child =
  (set_exn [@inlined]) t dir child;
  child

let to_d2 ?(cnt = ref 0) (oc : Out_channel.t) =
  (* let cnt = ref 0 in *)
  let[@inline] incr_and_get cnt =
    incr cnt;
    !cnt
  in
  let rec to_d2' parent t =
    if t == nil_tree then (
      let n = incr_and_get cnt in
      Printf.fprintf oc "\n%d: Nil" n;
      n)
    else begin
      let flag = t.flags in
      let nd_type = Utils.masked flag in
      let marked = Utils.is_marked flag in
      let nd_type_as_string = Utils.typeflag_to_string nd_type in
      if nd_type = Utils.r_flag then begin
        assert (t.par == parent);
        let n = incr_and_get cnt in
        let content = t.fn Show in
        Printf.fprintf oc
          "\n%d: R {\nshape: sql_table\ndirty: %s\ndetail: %s\n}" n
          (Bool.to_string marked) content;
        n
      end
      else begin
        let {left; right; par; _} = t in
        assert (par == parent);
        let leftid = to_d2' t left in
        let rightid = to_d2' t right in
        let n = incr_and_get cnt in
        Printf.fprintf oc
          "\n\
           %d: %s {\n\
           shape: sql_table\n\
           dirty:%s\n\
           }\n\
          \ %d -> %d : %s \n\
          \ %d -> %d : %s " n nd_type_as_string (Bool.to_string marked) n leftid
          "Left" n rightid "Right";
        n
      end
    end
  in
  to_d2' nil_tree

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
  let rec f p t =
    if t == nil_tree then stats.dummy <- stats.dummy + 1
    else begin
      let flag = t.flags in
      if Utils.is_marked flag then stats.dirty <- stats.dirty + 1;
      let nd_type = Utils.masked flag in
      if nd_type = Utils.r_flag then (
        assert (t.par == p);
        t.fn (Count stats);
        stats.r <- stats.r + 1)
      else
        let {left; right; par; _} = t in
        assert (par == p);
        if nd_type = Utils.s_flag || nd_type = Utils.root_flag then
          stats.s <- stats.s + 1
        else if nd_type = Utils.p_flag then stats.p <- stats.p + 1
        else Utils.impossible ();
        f t left;
        f t right
    end
  in
  f nil_tree t;
  stats
