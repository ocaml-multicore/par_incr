type t

val empty : unit -> t
val add_reader : t -> Rsp.RNode.t -> unit
val remove_reader : t -> Rsp.RNode.t -> unit
val iter : t -> (Rsp.RNode.t -> unit) -> unit
val length : t -> int
