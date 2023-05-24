type 'a t

val empty : eq:('a -> 'a -> bool) -> to_s:('a -> string) -> unit -> 'a t
val create : eq:('a -> 'a -> bool) -> to_s:('a -> string) -> 'a -> 'a t
val set : 'a t -> 'a -> unit
val value_exn : 'a t -> 'a
val add_reader : 'a t -> Rsp.RNode.t -> unit
val remove_reader : 'a t -> Rsp.RNode.t -> unit
val num_readers : 'a t -> int
val notify_readers : 'a t -> unit
val eq : 'a t -> 'a -> 'a -> bool
val to_s : 'a t -> string
val get_to_string : 'a t -> 'a -> string
val attach_eq : 'a t -> ('a -> 'a -> bool) -> unit
val attach_to_string : 'a t -> ('a -> string) -> unit
