type 'a t
type 'a incremental := 'a t
type comp_stat = Types.counter

type executor = Types.executor = {
  run : 'a. (unit -> 'a) -> 'a;
  par_do : 'a 'b. (unit -> 'a) -> (unit -> 'b) -> 'a * 'b;
}

module Var : sig
  type 'a t

  val create : ?eq:('a -> 'a -> bool) -> ?to_s:('a -> string) -> 'a -> 'a t
  val set : 'a t -> 'a -> unit
  val value : 'a t -> 'a
  val watch : 'a t -> 'a incremental
  val num_readers : 'a t -> int

  module Syntax : sig
    val ( := ) : 'a t -> 'a -> unit
    val ( ! ) : 'a t -> 'a
  end
end

type 'a computation

val return : 'a -> 'a t
val map : ?eq:('b -> 'b -> bool) -> fn:('a -> 'b) -> 'a t -> 'b t

val map2 :
  ?eq:('c -> 'c -> bool) ->
  ?mode:[< `Par | `Seq > `Seq] ->
  fn:('a -> 'b -> 'c) ->
  'a t ->
  'b t ->
  'c t

val combine : 'a t -> 'b t -> ('a * 'b) t
val value : 'a computation -> 'a
val dump_tree : string -> 'a computation -> unit
val destroy_comp : 'a computation -> unit
val delay : (unit -> 'a t) -> 'a t
val bind : fn:('a -> 'b t) -> 'a t -> 'b t
val par : left:'a t -> right:'b t -> ('a * 'b) t
val run : executor:executor -> 'a t -> 'a computation
val propagate : 'a computation -> unit
val get_stat : 'a computation -> comp_stat

module Debug : sig
  val attach : fn:('a -> string) -> 'a t -> 'a t
end

module Eq : sig
  val attach : fn:('a -> 'a -> bool) -> 'a t -> 'a t
end

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let& ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and& ) : 'a t -> 'b t -> ('a * 'b) t
end
