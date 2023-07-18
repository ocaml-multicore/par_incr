type 'a cutoff =
  | Never
  | Always
  | Phys_equal
  | Eq of ('a -> 'a -> bool)
  | F of (oldval:'a -> newval:'a -> bool)

type comp_tree = {
  mutable par : comp_tree;
  mutable flags : int;
  mutable left : comp_tree;
  mutable right : comp_tree;
  fn : 'a. 'a action -> 'a;
}

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

and rnode = comp_tree

let default_action : type a. a action -> a = function
  | Update -> ()
  | Remove _ -> ()
  | Show -> ""
  | Count _ -> ()

let nil_tree : comp_tree = Obj.magic ()

type executor = {
  run : 'a. (unit -> 'a) -> 'a;
  par_do : 'a 'b. (unit -> 'a) -> (unit -> 'b) -> 'a * 'b;
}

let rec set_mark (t : comp_tree) =
  let open Utils in
  if t != nil_tree && not (is_marked t.flags) then (
    t.flags <- make_marked t.flags;
    set_mark t.par)
