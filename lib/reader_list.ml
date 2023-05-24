type 'a t = 'a list Atomic.t

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

let for_all t f =
  begin
    List.iter f (Atomic.get t)
  end

let len t = List.length (Atomic.get t)
