# Par_incr - Parallel Self Adjusting Computations

A simple library for parallel incremental computations. Based on
[Efficient Parallel Self-Adjusting Computation](https://drive.google.com/file/d/130-sCY1YPzo4j3YAJ7EL9-MflK0l8RmJ/view?pli=1).
The documentation exists
[here](https://ocaml-multicore.github.io/par_incr/par_incr/Par_incr/index.html)

Similar libraries:

- [current_incr](https://github.com/ocurrent/current_incr)
- [Jane Street's incremental](https://github.com/janestreet/incremental)

> NOTE: The libraries mentioned above do not have support for parallelism.

## How it works

> NOTE: `incremental` is just an alias for `Par_incr.t`

- Define `Var.t` with certain values.
- Perform `Var.watch` operation on `Var.t` and change it to `incremental`.
- Every `incremental` signifies a computation in itself.
- Use different combinators provided by the library on the `incremental`s and
  make even bigger `incremental`s.
- Obtain value of a certain `incremental` by running it (a `run` operation is
  provided by the library).
- To run an `incremental`, we must pass an `executor` to the `run` function. The
  executor is the thing that allows for parallelism. `executor` is a record with
  two fields: `run` and `par_do`. More on this can be found in the
  [documentation](https://ocaml-multicore.github.io/par_incr/par_incr/Par_incr/index.html#type-executor)
- Running an `'a incremental` returns a `'a computation`.
- When we change some `Var.t` (done with `Var.set` operation), it marks all
  dependent computations dirty.
- Running `propagate` operation on a dirty `computation` updates its value
  efficiently.
- Destroy (with `destroy_comp` operation) `computation` when its no more
  required.

## Examples

### Add 1

This is a simple incremental computation which increments value by 1. It can be
easily done by using the `map` function provided by the library.

```ocaml
# #require "par_incr"

# open Par_incr
```

We'll start off creating a `Var.t` with value 10

```ocaml
# let x =
    Var.create 10
val x : int Var.t = <abstr>
```

We can use `map` to make a computation depending on `x`, which basically returns
`x+1`.

```ocaml
# let x_plus_1 = Par_incr.map
                 ~fn:(fun x -> x+1)
                 (Var.watch x)
val x_plus_1 : int t = <abstr>
```

As mentioned in [How It Works](#how-it-works) at the start, we need to create an
`executor` to pass to the `run` function. Here we are creating a simple executor
which doesn't do anything parallely(hence named seq_executor). You would
implement this `executor` differently if you wanted parallelism.

```ocaml
# let seq_executor = {
        run = (fun f -> f());
        par_do=(fun l r ->
            let lres = l() in (lres, r())
        )}
val seq_executor : executor = {run = <fun>; par_do = <fun>}
```

Now we `run` `x_plus_1` to get it's value and record all the computations
involved in getting the value as well.

```ocaml
# let x_plus_1_comp = Par_incr.run
                    ~executor:seq_executor
                    x_plus_1
val x_plus_1_comp : int computation = <abstr>
```

To get the value(type `'a`) out of `'a computation`, we just call `value` on it.

```ocaml
# Par_incr.value x_plus_1_comp
- : int = 11
```

We'll change the input value(in our case `x`) and see what happens. On changing
the input to any computation, we must run `propagate` to update the output.
`propagate` is clever enough to not do any work if there's no need to(i.e in
case some inputs change but the final output doesn't change, `propagate` will
make sure to not do any extra work and stop as soon as possible).

```ocaml
# Var.set x 20 (* Change value of x to 20*)
- : unit = ()

# Par_incr.propagate
    x_plus_1_comp (* Propagate changes*)
- : unit = ()

# Par_incr.value x_plus_1_comp
- : int = 21

# Par_incr.propagate
        x_plus_1_comp (* Propagating when
        there's no changes will do nothing*)
- : unit = ()

# Par_incr.value x_plus_1_comp
- : int = 21
```

Since we are done with the computation now, we should destroy it (with
`destroy_comp`)

```ocaml
# Par_incr.destroy_comp x_plus_1_comp
- : unit = ()
```

Running propagate on a destroyed computation will raise an exception

```ocaml
# Par_incr.propagate
            x_plus_1_comp
Exception: Failure "Cannot propagate destroyed/ill-formed computation".
```

### Adding 3 numbers

This example demonstrates `map2` and shows the usage of some of the convenient
syntax/operators exposed by the `Syntax` and `Var.Syntax` module of the library.

> NOTE: `map2` is just an abstraction over `combine` followed by a `map`.

```ocaml
# let a = Var.create 10
val a : int Var.t = <abstr>

# let b = Var.create 20
val b : int Var.t = <abstr>

# let c = Var.create 30
val c : int Var.t = <abstr>
```

We can use `map2` provided by the library to add `a` `b` `c` together. There's a
convenient syntax to achieve the same thing which can be used by opening the
`Syntax` module. You can see that they give the same results.

```ocaml
# let abc_sum = Par_incr.map2
                ~fn:(Int.add)
                (Var.watch c)
                (Par_incr.map2
                    ~fn:(Int.add)
                    (Var.watch a)
                    (Var.watch b))
val abc_sum : int t = <abstr>

# let abc_sum' =(*If we open Syntax module, we can write
                                    this more cleanly*)
    let open Par_incr.Syntax in
    let+ a = Var.watch a
    and+ b = Var.watch b
    and+ c = Var.watch c in
    a + b + c
val abc_sum' : int t = <abstr>

# let abc_sum_comp = Par_incr.run
        ~executor:seq_executor abc_sum
val abc_sum_comp : int computation = <abstr>

# let abc_sum'_comp = Par_incr.run
        ~executor:seq_executor abc_sum'
val abc_sum'_comp : int computation = <abstr>

# assert (Par_incr.value abc_sum_comp =
          Par_incr.value abc_sum'_comp)
- : unit = ()
```

On changing the inputs, we can see that the output is updated accordingly. The
below code snippet shows off the operators exposed by `Var.Syntax` module as
well.

```ocaml
# Var.set a 40
- : unit = ()

# Par_incr.propagate abc_sum'_comp
- : unit = ()

# Par_incr.value abc_sum'_comp
- : int = 90

# let () =
    (*Var.Syntax module provides some convenient operators*)
    let open Var.Syntax in
    (*Equivalent to Var.set b (Var.value b + Var.value b) *)
    b := (!b) + (!b);

# Par_incr.propagate abc_sum'_comp;
  Par_incr.propagate abc_sum_comp
- : unit = ()

# Par_incr.value abc_sum'_comp
- : int = 110

# assert (Par_incr.value abc_sum'_comp =
          Par_incr.value abc_sum_comp )
- : unit = ()

# Par_incr.destroy_comp abc_sum_comp;
  Par_incr.destroy_comp abc_sum'_comp
- : unit = ()
```

### Exploiting Parallelism

This examples shows usage of
[Domainslib](https://github.com/ocaml-multicore/domainslib) to parallelize
incremental computations. In this example as well, there's use of the nice
syntax/operators that the `Syntax` module provides.

```ocaml
# #require "domainslib"

# module T = Domainslib.Task
module T = Domainslib.Task
```

We start off by creating a parallel `executor` since we want to actually run
things in parallel this time. This does require some `domainslib` knowledge but
other than that this is pretty easy to understand.

```ocaml
# let get_par_executor ~num_domains () = (* A useful
             function to give us a parallel executor*)
    let pool = T.setup_pool ~num_domains () in
    let par_runner f = T.run pool f in
    let par_do l r =
        let lres = T.async pool l in
        let rres = r () in
        (T.await pool lres, rres)
    in
    (pool, {run = par_runner; par_do})
val get_par_executor : num_domains:int -> unit -> T.pool * executor = <fun>

# let pool, par_executor = get_par_executor ~num_domains:4 ()
val pool : T.pool = <abstr>
val par_executor : executor = {run = <fun>; par_do = <fun>}
```

In the `sum_range` function, we're dividing the array in half at each level and
computing the sum of both halves in parallel. There's multiple ways to write
this function and alternative ways are shown as part of the comments.

```ocaml
# let rec sum_range ~lo ~hi xs =
    Par_incr.delay @@ fun () ->
    if hi - lo = 1 then begin
        xs.(lo)
    end
    else
        let mid = lo + ((hi - lo) asr 1) in
        let open Par_incr.Syntax in
        (*Using let+ and and+ instead of let& and
        and& would make this sequential*)
        let& lhalf = sum_range ~lo ~hi:mid xs
        and& rhalf = sum_range ~lo:mid ~hi xs in
        lhalf + rhalf
        (*Can be written alternatively as:
        Par_incr.map2 ~mode:`Par ~fn:(Int.add)
            (sum_range ~lo ~hi:mid xs)
            (sum_range ~lo:mid ~hi xs)

        or

        let res = par ~left:(sum_range ~lo ~hi:mid xs)
                      ~right:(sum_range ~lo:mid ~hi xs)
        in
        map ~fn:(fun (x,y) -> Int.add x y)

        *)
val sum_range : lo:int -> hi:int -> int t array -> int t = <fun>
```

We'll define the array we want to sum here and run the computation with
`par_executor` defined above. The example demonstrates input change and
propagation as well.

```ocaml
# let arr = Array.map Var.create
                [|1;2;3;4;5;6;7;8;9;10|]
val arr : int Var.t array =
  [|<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>|]

# let t_arr = Array.map Var.watch arr
val t_arr : int t array =
  [|<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>|]

# let arr_sum = sum_range ~lo:0
                ~hi:(Array.length t_arr) t_arr
val arr_sum : int t = <abstr>

# let arr_sum_comp = run ~executor:par_executor
                                        arr_sum
val arr_sum_comp : int computation = <abstr>

# Par_incr.value arr_sum_comp
- : int = 55

# Var.set arr.(0) 11
- : unit = ()

# Par_incr.propagate arr_sum_comp;
  Par_incr.value arr_sum_comp
- : int = 65

# Par_incr.destroy_comp arr_sum_comp
- : unit = ()
```

### Filtering a Cons List

Although this example is very inefficient, it helps us realize why we need the
`bind` operation. All other operations only builds static computation trees, but
for writing complex computations, like an incremental filter which adapts to
changes in the list, we need dynamism. We get that with `bind`. In the example,
we use `let*` operator which is just a syntactic sugar for `bind` provided by
`Syntax` module.

> You can combine multiple `bind`s together with `and*` operator provided by
> `Syntax` module. `and*` is again just a syntactic sugar for `combine`
> operation provided by the library.

First off, we define some helper functions.

```ocaml
# let rec to_var_list xs = (*Helper function*)
    Var.create (
    match xs with
      | [] -> `Nil
      | x :: xs -> `Cons (x, to_var_list xs)
    )
val to_var_list : 'a list -> ([> `Cons of 'a * 'b | `Nil ] Var.t as 'b) =
  <fun>

# let rec to_incr_list xs = (*Helper function*)
    let open Par_incr.Syntax in
    let+ l  = Var.watch xs in (*map operation*)
    match l with
    | `Nil -> `Nil
    | `Cons(x,xs) -> `Cons(x, to_incr_list xs)
val to_incr_list :
  ([< `Cons of 'b * 'a | `Nil ] Var.t as 'a) ->
  ([> `Cons of 'b * 'c | `Nil ] t as 'c) = <fun>
```

The `filter` function is then defined. It binds `xs` and returns another
incremental based on what `xs` was. The reason we need to use `bind` here
instead of something like `map` is because the tail of the list is also fully
dynamic and we want the tail to be computed incrementally as well. If we're
using map, the `~fn` passed to map is expected to be pure (pure, in our case,
can be defined to be something doesn't use anything that is `incremental` in its
body). The behaviour is not well-defined if `~fn` is not pure.

```ocaml
# let rec filter predicate xs =
    let open Par_incr.Syntax in
    let* l= xs in (*Syntactic sugar for bind*)
    match l with
    | `Nil -> Par_incr.return []
    | `Cons (x, xs) ->
        let xs = filter predicate xs in
        if predicate x
        then (let* xs = xs in
            Par_incr.return (x::xs))
        (*Can also be written as:
        bind xs (fun xs -> return (x::xs))
        *)
        else xs
val filter :
  ('a -> bool) -> ([< `Cons of 'a * 'b | `Nil ] t as 'b) -> 'a list t = <fun>
```

We can see `filter` does indeed work as expected.

```ocaml
# let var_list = to_var_list [2;3;5]
val var_list : _[> `Cons of int * 'a | `Nil ] Var.t as 'a = <abstr>

# let incr_list = to_incr_list var_list
val incr_list : _[> `Cons of int * 'a | `Nil ] t as 'a = <abstr>

# let res_list = filter (fun x -> x mod 2 = 1)
                 incr_list
val res_list : int list t = <abstr>

# let filter_comp = Par_incr.run ~executor:seq_executor
                    res_list
val filter_comp : int list computation = <abstr>

# Par_incr.value filter_comp
- : int list = [3; 5]

# let () =
    let open Var.Syntax in
    (*Let's change first element to 5 and see what happens*)
    match !var_list with
    | `Nil -> failwith "Impossible"
    | `Cons(x,xs) -> var_list:= `Cons(5, xs)

# Par_incr.propagate filter_comp;
  Par_incr.value filter_comp
- : int list = [5; 3; 5]

# Par_incr.destroy_comp filter_comp
- : unit = ()

# T.teardown_pool pool (*Teardown the domainslib
                    Task pool we created before*)
- : unit = ()
```
