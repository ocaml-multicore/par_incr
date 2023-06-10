type 'a t

val empty : unit -> 'a t
val push_back : 'a t -> 'a -> unit
val push_front : 'a t -> 'a -> unit
val pop_back : 'a t -> 'a
val pop_front : 'a t -> 'a
val size : 'a t -> int
val to_list : 'a t -> 'a list
