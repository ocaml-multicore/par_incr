(* In a better looking version of this decl, we'd define some common *)
(* type and have Seq of <type> | Par of <type> but doing that would  *)
(* we'd be chasing pointers a lot and will incur more allocation, we *)
(* want to avoid that. *)
type comp_tree =
  | Dummy
  | Root of {mutable left : comp_tree; mutable right : comp_tree}
  | Seq of {
      mutable par : comp_tree;
      mutable par_mark : bool;
      mutable left : comp_tree;
      mutable right : comp_tree;
    }
  | Par of {
      mutable par : comp_tree;
      mutable par_mark : bool;
      mutable left : comp_tree;
      mutable right : comp_tree;
    }
  | R of rnode

and counter = {
  mutable map : int;
  mutable combine : int;
  mutable bind : int;
  mutable par_do : int;
  mutable r : int;
  mutable s : int;
  mutable p : int;
  mutable dummy : int;
  mutable dirty : int;
}

and _ action =
  | Update : unit action
  | Remove : rnode -> unit action
  | Show : string action
  | Count : counter -> unit action

and rnode = {
  mutable par : comp_tree;
  mutable has_pending_update : bool;
  fn : 'a. 'a action -> 'a;
}

type executor = {
  run : 'a. (unit -> 'a) -> 'a;
  par_do : 'a 'b. (unit -> 'a) -> (unit -> 'b) -> 'a * 'b;
}

let rec set_mark t =
  match t with
  | Root _ -> ()
  | Seq nd ->
    if not nd.par_mark then begin
      nd.par_mark <- true;
      set_mark nd.par
    end
  | Par nd ->
    if not nd.par_mark then begin
      nd.par_mark <- true;
      set_mark nd.par
    end
  | R _ | Dummy -> Utils.impossible ()
