type 'a t =
  | Root of {mutable size : int; mutable next : 'a t; mutable prev : 'a t}
  | Entry of {value : 'a; mutable next : 'a t; mutable prev : 'a t}

let empty () =
  let rec root = Root {size = 0; next = root; prev = root} in
  root

let change_size op t =
  match t with
  | Root r -> (
    match op with
    | `Incr -> r.size <- r.size + 1
    | `Decr -> r.size <- r.size - 1)
  | _ -> failwith "impossible"

let[@inline] value = function
  | Root _ -> failwith "impossible"
  | Entry e -> e.value

let[@inline] size = function Root r -> r.size | _ -> failwith "impossible"
let[@inline] next t = match t with Root r -> r.next | Entry e -> e.next
let[@inline] prev t = match t with Root r -> r.prev | Entry e -> e.prev

let[@inline] set_next t n =
  match t with Root r -> r.next <- n | Entry e -> e.next <- n

let[@inline] set_prev t p =
  match t with Root r -> r.prev <- p | Entry e -> e.prev <- p

let[@inline] push_back root x =
  let entry = Entry {value = x; next = root; prev = prev root} in
  set_next (prev root) entry;
  set_prev root entry;
  change_size `Incr root

let[@inline] push_front root x =
  let entry = Entry {value = x; next = next root; prev = root} in
  set_next root entry;
  set_prev (next entry) entry;
  change_size `Incr root

let[@inline] pop_front root =
  assert (size root != 0);
  let next_node = next root in
  let next_next = next next_node in
  set_next root next_next;
  set_prev next_next root;
  change_size `Decr root;
  value next_node

let[@inline] pop_back root =
  assert (size root != 0);
  let prev_node = prev root in
  let prev_prev = prev prev_node in
  set_next prev_prev root;
  set_prev root prev_prev;
  change_size `Decr root;
  value prev_node

let to_list root =
  let rec f cur acc =
    if cur == root then acc else f (next cur) (value cur :: acc)
  in
  List.rev (f (next root) [])
