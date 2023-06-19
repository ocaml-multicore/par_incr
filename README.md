# Par_incr - Parallel Self Adjusting Computations

A simple library for parallel incremental computations. Based on [Efficient Parallel Self-Adjusting Computation](https://drive.google.com/file/d/130-sCY1YPzo4j3YAJ7EL9-MflK0l8RmJ/view?pli=1)

## Example

### Add 1

```ocaml
# #require "par_incr";;

# open Par_incr;;

# let x = Var.create 10;; (*Create Var.t with value 10 *)
val x : int Var.t = <abstr>

# let x_plus_1 = Par_incr.map
                 ~fn:(fun x -> x+1)
                 (Var.watch x);; (*A simple add 1 computation*)
val x_plus_1 : int t = <abstr>

# let seq_executor ={ (*A simple executor that runs computations sequentially(for demo purpose)*)
                    run = (fun f -> f());
                    par_do=(fun l r -> let lres = l() in (lres, r()))
                   }
val seq_executor : executor = {run = <fun>; par_do = <fun>}

# let x_plus_1_comp = Par_incr.run ~executor:seq_executor x_plus_1 ;;
val x_plus_1_comp : int computation = <abstr>

# Par_incr.value x_plus_1_comp;;
- : int = 11

# Var.set x 20;; (* Change value of x to 20*)
- : unit = ()

# Par_incr.propagate x_plus_1_comp;; (* Propagate changes*)
- : unit = ()

# Par_incr.value x_plus_1_comp;;
- : int = 21

# Par_incr.propagate x_plus_1_comp;; (* Propagating when there's no changes will do nothing*)
- : unit = ()

# Par_incr.value x_plus_1_comp;;
- : int = 21

# Par_incr.destroy_comp x_plus_1_comp;;
- : unit = ()

# Par_incr.propagate x_plus_1_comp (* Running propagate on
                destroyed computation raises exception*);;
Exception: Failure "Cannot propagate destroyed/ill-formed computation".
```

### Adding 3 numbers

```ocaml
# let a = Var.create 10;;
val a : int Var.t = <abstr>

# let b = Var.create 20;;
val b : int Var.t = <abstr>

# let c = Var.create 30;;
val c : int Var.t = <abstr>

# let abc_sum = Par_incr.map2
                ~fn:(Int.add)
                (Var.watch c)
                (Par_incr.map2
                    ~fn:(Int.add)
                    (Var.watch a)
                    (Var.watch b));;
val abc_sum : int t = <abstr>

# let abc_sum' = (*If we open Syntax module, we can write this more cleanly*)
    let open Par_incr.Syntax in
    let+ a = Var.watch a
    and+ b = Var.watch b
    and+ c = Var.watch c in
    a + b + c
    ;;
val abc_sum' : int t = <abstr>

# let abc_sum_comp = Par_incr.run ~executor:seq_executor abc_sum;;
val abc_sum_comp : int computation = <abstr>

# let abc_sum'_comp = Par_incr.run ~executor:seq_executor abc_sum';;
val abc_sum'_comp : int computation = <abstr>

# assert (Par_incr.value abc_sum_comp = Par_incr.value abc_sum'_comp);;
- : unit = ()

# Var.set a 40;;
- : unit = ()

# Par_incr.propagate abc_sum'_comp;;
- : unit = ()

# Par_incr.value abc_sum'_comp;;
- : int = 90

# let () =(*Var.Syntax module provides some convenient operators*)
    let open Var.Syntax in
    (*Equivalent to Var.set b (Var.value b + Var.value b) *)
    b := (!b) + (!b);

# Par_incr.propagate abc_sum'_comp; Par_incr.propagate abc_sum_comp;;
- : unit = ()

# Par_incr.value abc_sum'_comp;;
- : int = 110

# assert (Par_incr.value abc_sum'_comp = Par_incr.value abc_sum_comp );;
- : unit = ()

# Par_incr.destroy_comp abc_sum_comp ; Par_incr.destroy_comp abc_sum'_comp;;
- : unit = ()
```

### Exploiting Parallelism

```ocaml
# #require "domainslib";;

# module T = Domainslib.Task;;
module T = Domainslib.Task

# let get_par_executor ~num_domains () = (*Let's define a useful function to give us parallel executor*)
    let module T = Domainslib.Task in
    let pool = T.setup_pool ~num_domains () in
    let par_runner f = T.run pool f in
    let par_do l r =
        let lres = T.async pool l in
        let rres = r () in
        (T.await pool lres, rres)
    in
    (pool, {run = par_runner; par_do})
val get_par_executor : num_domains:int -> unit -> T.pool * executor = <fun>

# let pool, par_executor = get_par_executor ~num_domains:4 () ;;
val pool : T.pool = <abstr>
val par_executor : executor = {run = <fun>; par_do = <fun>}

# let rec sum_range ~lo ~hi xs =
    Par_incr.delay @@ fun () ->
    if hi - lo = 1 then begin
        xs.(lo)
    end
    else
        let mid = lo + ((hi - lo) asr 1) in
        let open Par_incr.Syntax in
        (*Using let+ and and+ instead of let& and and& would make this sequential*)
        let& lhalf = sum_range ~lo ~hi:mid xs
        and& rhalf = sum_range ~lo:mid ~hi xs in
        lhalf + rhalf
        (*Can be written alternatively as:
        Par_incr.map2 ~mode:`Par ~fn:(Int.add) (sum_range ~lo ~hi:mid xs)
            (sum_range ~lo:mid ~hi xs)
        *)
val sum_range : lo:int -> hi:int -> int t array -> int t = <fun>

# let arr = Array.map Var.create [|1;2;3;4;5;6;7;8;9;10|];;
val arr : int Var.t array =
  [|<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>|]

# let t_arr = Array.map Var.watch arr;;
val t_arr : int t array =
  [|<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>|]

# let arr_sum = sum_range ~lo:0 ~hi:(Array.length t_arr) t_arr;;
val arr_sum : int t = <abstr>

# let arr_sum_comp = run ~executor:par_executor arr_sum;;
val arr_sum_comp : int computation = <abstr>

# Par_incr.value arr_sum_comp;;
- : int = 55

# Var.set arr.(0) 11;;
- : unit = ()

# Par_incr.propagate arr_sum_comp; Par_incr.value arr_sum_comp;;
- : int = 65

# Par_incr.destroy_comp arr_sum_comp;;
- : unit = ()
```

### Filtering a Cons List

Although this example is very inefficient, it helps us realize why we need the `bind` operation.
All other operations only builds static computation trees, but for writing complex computations
,like an incremental filter which adapts to changes in the list, we need dynamism. We get that
with `bind`.

```ocaml
# let rec to_var_list xs = (*Helper function*)
    Var.create (match xs with [] -> `Nil | x :: xs -> `Cons (x, to_var_list xs))
val to_var_list : 'a list -> ([> `Cons of 'a * 'b | `Nil ] Var.t as 'b) =
  <fun>

# let rec to_incr_list xs = (*Helper function*)
    let open Par_incr.Syntax in
    let+ l  = Var.watch xs in
    match l with
    | `Nil -> `Nil
    | `Cons(x,xs) -> `Cons(x, to_incr_list xs)
val to_incr_list :
  ([< `Cons of 'b * 'a | `Nil ] Var.t as 'a) ->
  ([> `Cons of 'b * 'c | `Nil ] t as 'c) = <fun>

# let rec filter predicate xs =
    let open Par_incr.Syntax in
    let* l= xs in (*Syntactic sugar for bind*)
    match l with
    | `Nil -> Par_incr.return []
    | `Cons (x, xs) -> let xs = filter predicate xs in
        if predicate x then (let* xs = xs in Par_incr.return (x::xs)) else xs
val filter :
  ('a -> bool) -> ([< `Cons of 'a * 'b | `Nil ] t as 'b) -> 'a list t = <fun>

# let var_list = to_var_list [2;3;5];;
val var_list : _[> `Cons of int * 'a | `Nil ] Var.t as 'a = <abstr>

# let incr_list = to_incr_list var_list;;
val incr_list : _[> `Cons of int * 'a | `Nil ] t as 'a = <abstr>

# let res_list = filter (fun x -> x mod 2 = 1) incr_list;;
val res_list : int list t = <abstr>

# let filter_comp = Par_incr.run ~executor:seq_executor res_list;;
val filter_comp : int list computation = <abstr>

# Par_incr.value filter_comp;;
- : int list = [3; 5]

# let () =
    let open Var.Syntax in
    (*Let's change first element to 5 and see what happens*)
    var_list := ( match !var_list with
        | `Nil -> failwith "Impossible"
        | `Cons(x,xs) -> `Cons(5, xs)
    )

# Par_incr.propagate filter_comp; Par_incr.value filter_comp;;
- : int list = [5; 3; 5]

# Par_incr.destroy_comp filter_comp;;
- : unit = ()

# T.teardown_pool pool;; (*Teardown the domainslib Task pool we created before*)
- : unit = ()
```
