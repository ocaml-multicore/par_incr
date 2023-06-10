type t = Rsp.RNode.t list Atomic.t

let empty () = Atomic.make []

let[@tail_mod_cons] rec remove_first x' = function
  | [] -> []
  | x :: xs -> if x == x' then xs else x :: remove_first x' xs

let rec remove_reader t node =
  let nodes = Atomic.get t in
  if not (Atomic.compare_and_set t nodes (remove_first node nodes)) then
    remove_reader t node

let rec add_reader t node =
  let nodes = Atomic.get t in
  if not (Atomic.compare_and_set t nodes (node :: nodes)) then add_reader t node

let[@inline] for_all t f =
  begin
    let t' = Atomic.get t in
    let rec g = function
      | [] -> ()
      | x :: xs ->
        f x;
        g xs
    in
    g t'
    (* List.iter f t' *)
  end

let len t = List.length (Atomic.get t)
