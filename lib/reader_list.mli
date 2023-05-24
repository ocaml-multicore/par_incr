type 'a t

val empty : unit -> 'a t
val add_reader : 'a t -> 'a -> unit
val remove_reader : 'a t -> 'a -> unit
val for_all : 'a t -> ('a -> unit) -> unit
val len : 'a t -> int
