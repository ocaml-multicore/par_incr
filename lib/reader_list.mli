type t

val empty : unit -> t
val add_reader : t -> Rsp.RNode.t -> unit
val remove_reader : t -> Rsp.RNode.t -> unit
val for_all : t -> (Rsp.RNode.t -> unit) -> unit
val len : t -> int
