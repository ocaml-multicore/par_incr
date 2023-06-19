(** A simple library for parallel incremental computations.
    Based on {{: https://drive.google.com/file/d/130-sCY1YPzo4j3YAJ7EL9-MflK0l8RmJ/view?pli=1 }"Efficient Parallel Self-Adjusting Computation"}

    {2 How it works}

    - Define [Var.t] with certain values.
    - Perform [watch] operation on [Var.t] and change it to [incremental]
    - Every [incremental] signifies a computation in itself
    - Use different combinators provided by the library on the [incremental]s
     and make even bigger [incremental]s
    - Obtain value of a certain [incremental] by running it(A [run] operation is provided
     by the library)
    - Running an ['a incremental] returns an ['a computatation]
    - When we change some Var.t(done with [Var.set] operation), it marks all dependent
     computations dirty.
    - Running [propagation] operation on dirty [computation] updates it's value efficiently
    - Destroy(with [destroy_comp] operation) [computation] when it's no more required


 *)

(** {2 Par_incr Module Documentation} *)

type 'a t
(** An ['a t] holds a value of type 'a. This type is opaque and we don't expose
    any mechanism to change this or modify it. Computations happens by chaining
    together various combinators provided by the library, all of which operate
    on['a t]
*)

type 'a incremental := 'a t
(** type ['a incremental] is an alias for ['a t]*)

type 'a computation
(** An ['a computation] holds everything necessary to store a computation *)

(**/**)

type comp_stat = Types.counter
(** A [comp_stat] holds the statistics about the computation *)

(**/**)

type executor = Types.executor = {
  run : 'a. (unit -> 'a) -> 'a;
  par_do : 'a 'b. (unit -> 'a) -> (unit -> 'b) -> 'a * 'b;
}
(** An [executor] is something that has to be passed while initially running the
    the computation. The reason behind this is, we want to support multiple ways
    to run computations parallely and library users can use the one that's best
    for them.

    Suppose you have your own scheduler built on top of [Domains] and you don't
    want to use the standard Domainslib for running tasks. You can very well use
    it as long as you provide these two [run] and [par_do] functions.

    If however you want to use [Domainslib], you would have an [executor] which
    would look roughly like this:
    {[
    module T = Domainslib.Task
    let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count () / 2) ()
    let par_runner f = T.run pool f
    let par_do l r =
      let lres = T.async pool l in
      let rres = r () in
      (T.await pool lres, rres)
    let executor = {run = par_runner; par_do}
    ]}

*)

(** Defines the type and various operations for modifiable values*)
module Var : sig
  type 'a t
  (** An ['a Var.t] will hold a value of type 'a. Unlike ['a t], this
      can be read and changed. This is what makes it possible to change
      inputs of a computation and the change to be propapagated efficiently.
      This is our handle to values that can be mutated and the computations
      can be propagated efficiently.
      *)

  val create : ?eq:('a -> 'a -> bool) -> ?to_s:('a -> string) -> 'a -> 'a t
  (** [Var.create x] creates a Var.t with value [x]. Optionally you can specify
      [eq] and [to_s] parameters as well. Default for [eq] is [(==)] and it's
      recommended to pass [eq] wherever it the default doesn't work.
     *)

  val set : 'a t -> 'a -> unit
  (** [Var.set t x] sets the value of [t] to [x]. In case [x] is same as the old
      value, it won't do anything. If [x] is not equal to the old value, it'll
      mark all the computations that depend on [t] to be ready for propagation.
     *)

  val value : 'a t -> 'a
  (** [Var.value t] returns the value stored inside t *)

  val watch : 'a t -> 'a incremental
  (** [Var.value t] converts ['a Var.t] to ['a incremental]. This is what lets us apply many
      combinators made available by the library and write bigger incremental programs.
      *)

  (**/**)

  val num_readers : 'a t -> int
  (** [Var.num_readers t] returns the number of readers/dependants that [t] has*)

  (**/**)

  (** Syntax module introduces some convenient operators for [Var.set] and [Var.value] operations*)
  module Syntax : sig
    val ( := ) : 'a t -> 'a -> unit
    (** Shorthand for Var.set
    {[
      let x = Var.create 2
      .
      .
      .
      let () = x:= 3
     ]}
    *)

    val ( ! ) : 'a t -> 'a
    (** Shorthand for Var.value
    {[
      let x = Var.create 2
      let () = assert( !x = 2)
     ]}
    *)
  end
end

val return : 'a -> 'a t
(** [return x] returns an instance of ['a incremental] from [x] of type ['a] *)

val map : ?eq:('b -> 'b -> bool) -> fn:('a -> 'b) -> 'a t -> 'b t
(** [map ~fn a] maps the internal value of [a] to [fn]. Default [eq] is [(==)] *)

val map2 :
  ?eq:('c -> 'c -> bool) ->
  ?mode:[< `Par | `Seq > `Seq] ->
  fn:('a -> 'b -> 'c) ->
  'a t ->
  'b t ->
  'c t
(** [map2 ~fn ?(mode=`Seq) a b] is a convenient function to [map] over two [incremental]s.
    If [mode] is [`Seq], it computes [a] and [b] sequentially, but if it is [`Par], computing
    [a] and [b] happens parallely(it's ran with [executor]'s [par_do] function)
    Default [eq] is [(==)].*)

val combine : 'a t -> 'b t -> ('a * 'b) t
(** [combine a b] is useful function to combine two [incremental]'s into one*)

val bind : fn:('a -> 'b t) -> 'a t -> 'b t
(** [bind ~fn a] calls [fn] with the value of [a] and returns that. This lets us build
    computations that are more dynamic in nature. This is the monadic bind operation for
    [incremental]s.
*)

val par : left:'a t -> right:'b t -> ('a * 'b) t
(** [par ~left:a ~right:b] computes [a] and [b] parallely and gives us the result as a new [incremental]
    with both values stored as tuple. This uses [executor]'s [par_do] function to run [left] and [right]
    parallely.
*)

val delay : (unit -> 'a t) -> 'a t
(** [delay f] lets us have [incremental]s that are lazily evaluated*)

val value : 'a computation -> 'a
(** [value c] returns the value/result associated with the computation [c]*)

val run : executor:executor -> 'a t -> 'a computation
(** [run ~executor t] evaluates [t] with the provided [executor]. This stores the
    result of the computation as well as all the data structures associated with it.
*)

val propagate : 'a computation -> unit
(** [propagate c] will propagate the changes to all the [Var.t] that the computation
    [c] dependended on. If there's no any changes, it will not do any extra work and
    return back right away.
*)

val dump_tree : string -> 'a computation -> unit
(** [dump_tree file c] will dump the computation tree associated with [c] into
    [file]. It dumps the tree in D2 format. See instructions at https://github.com/terrastruct/d2
    for viewing the files.
*)

val destroy_comp : 'a computation -> unit
(** [destroy_comp c] will destroy the computation associated with [c].After destroying, calling
    [propagate] on [c] will result in an exception. It's necessary to destroy computations that
    are no longer needed. ['a computation] will be taking up memory and doing unnecessary work if not
    destroyed.
*)

(**/**)

val get_stat : 'a computation -> comp_stat

(**/**)

module Debug : sig
  val attach : fn:('a -> string) -> 'a t -> 'a t
  (** Attach [to_string] function to an incremental. This is especially useful when we
      want to print the computation tree with [dump_tree]. With a specialized to_string
      attached to an [incremental], we can get better tree dumps.
      *)
end

module Eq : sig
  val attach : fn:('a -> 'a -> bool) -> 'a t -> 'a t
  (** [Eq.attach ~fn:my_custom_eq_fn incr] attaches [my_custom_eq_fn] as [eq] function
      to the incremental [incr]. This is useful in cases where you want somewhat different
      cutoff conditions. Take floating point related computations for example, you may
      choose to ignore difference in values within some delta. For such cases, you can use
      this.
  *)
end

(** Introduces some convenient operators for [map], [bind], [combine] and [par] operations*)
module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Syntactic sugar for [map]. Note: You can't provide your own [eq] function when using [Syntax]
      module. You may use Eq.attach to add it to the resultant [incremental], but there's no way to
      pass it as parameter initially like with [map].
  *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** Syntactic sugar for [combine]. Recommended to be used together with [let+]*)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntactic sugar for [bind]*)

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  (** Syntactic sugar for [combine]. Recommended to be used together with [let*]*)

  val ( let& ) : 'a t -> ('a -> 'b) -> 'b t
  (** Syntactic sugar for [map]. Behaves similarly to [let+] *)

  val ( and& ) : 'a t -> 'b t -> ('a * 'b) t
  (** Syntactic sugar for [par]. Recommended to be used together with [let&]*)
end
