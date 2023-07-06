open Par_incr

let seq_executor = {run = (fun f -> f ()); par_do = (fun l r -> (l (), r ()))}
let output_check = "Output check"
let no_readers_check = "No readers check"
let reader_check = "No of Readers check"
let no_leak_check = "No leaks or extra allocations check"

let live_words () =
  Gc.full_major ();
  Gc.full_major ();
  (Gc.stat ()).live_words

let does_raise f = match f () with exception _ -> true | _ -> false

let simple_test () =
  let x = Var.create 10 in
  let y = run ~executor:seq_executor (Var.watch x) in
  (*This is because there's no need for reader node in this case*)
  Alcotest.(check int) reader_check 0 (Var.num_readers x);
  Alcotest.(check int) output_check 10 (value y);
  let live_words_before_loop = live_words () in

  for i = 1 to 10 do
    let () = Var.set x i in
    let () = propagate y in
    Alcotest.(check int) output_check i (value y);
    Alcotest.(check bool)
      no_leak_check true
      (live_words () <= live_words_before_loop)
  done;

  let () = destroy_comp y in
  Alcotest.(check int) no_readers_check 0 (Var.num_readers x)

(*
Make sure destroy works properly. Behaviour is:

- Destroys the entire computation. This means the readers count should be
  changed accordingly.

- Trying to run propagate on destroyed computation raises exception.
*)
let destroy_test () =
  let x = Var.create 10 in
  let y = run ~executor:seq_executor (map ~fn:(fun y -> 2 * y) (Var.watch x)) in

  Alcotest.(check int) no_readers_check 1 (Var.num_readers x);
  let () =
    Alcotest.(check bool)
      "propagate doesn't raise before destroying" false
      (does_raise (fun () -> propagate y))
  in

  let () = destroy_comp y in

  Alcotest.check_raises "propagate raises after destroying"
    (Failure "Cannot propagate destroyed/ill-formed computation") (fun () ->
      propagate y);
  Alcotest.(check int) no_readers_check 0 (Var.num_readers x);

  (*'a computation = {var;root;e}, so root is 1st field(starting from 0)*)
  (* Dummy is the only empty constructor for Rsp.t, meaning it's value is 0  *)
  Alcotest.(check int)
    "root is changed to dummy node" 0
    (Obj.magic (Obj.field (Obj.repr y) 1))

(*
We'll test the following properties of map:
- The output should be updated on changing input(and calling propagate)
- Shouldn't cause leaks
*)
let map_test () =
  let map_fn x = 2 * x in
  let x = Var.create 10 in
  let y = run ~executor:seq_executor (map ~fn:map_fn (Var.watch x)) in
  Alcotest.(check int) reader_check 1 (Var.num_readers x);
  Alcotest.(check int) output_check (map_fn 10) (value y);

  let live_words_before_loop = live_words () in
  for i = 1 to 100 do
    let () = Var.set x i in
    let () = propagate y in
    Alcotest.(check int) output_check (map_fn i) (value y);
    Alcotest.(check bool)
      no_leak_check true
      (live_words () <= live_words_before_loop)
  done;
  let () = destroy_comp y in
  Alcotest.(check int) reader_check 0 (Var.num_readers x)

(* Testing change propagation for combine *)
let combine_test () =
  let tup_eq (a', b') (a'', b'') = a' = a'' && String.equal b' b'' in

  let x = Var.create 10 in
  let y = Var.create "Ten" in
  let live_words_before_running_comp = live_words () in
  let z = run ~executor:seq_executor (combine (Var.watch x) (Var.watch y)) in
  Alcotest.(check int) reader_check 1 (Var.num_readers x);
  Alcotest.(check int) reader_check 1 (Var.num_readers y);
  Alcotest.(check bool) output_check true (tup_eq (10, "Ten") (value z));

  let () = Var.set x 20 in
  let () = propagate z in
  Alcotest.(check bool) output_check true (tup_eq (20, "Ten") (value z));

  let () = Var.set y "Twenty" in
  let () = propagate z in
  Alcotest.(check bool) output_check true (tup_eq (20, "Twenty") (value z));

  let () = Var.set x 30 in
  let () = Var.set y "Thirty" in
  let () = propagate z in
  Alcotest.(check bool) output_check true (tup_eq (30, "Thirty") (value z));

  let () = destroy_comp z in
  Alcotest.(check bool)
    no_leak_check true
    (live_words () <= live_words_before_running_comp);
  Alcotest.(check int) reader_check 0 (Var.num_readers x);
  Alcotest.(check int) reader_check 0 (Var.num_readers y)

(*Making sure things actually run parallely(atleast making surepar_do gets
  called appropriately). Checking for leaks in this test will give weird results
  since we have domains running. So, the other test `par_test_for_gc` will make
  sure we don't have leaks. This test makes sure of the change propagation and
  when par_do operation gets called. This is sort of testing the implementation
  of propagate as well in one way. *)
let par_test () =
  let open Syntax in
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:2 () in
  let test () =
    let par_do_call_count = Atomic.make 0 in
    let run f = T.run pool f in

    let par_do l r =
      Atomic.incr par_do_call_count;
      (*We'll use atomic incr here since we are doing parallel operations. Our
        operation is rather simple(there's just only one invocation of par_do),
        so it would work for this particular case to use ref incr too. But we'll
        use Atomic incr because that is what you should use when you have things
        that can run parallely. *)
      let lres = T.async pool l in
      let rres = r () in
      (T.await pool lres, rres)
    in
    let executor = {run; par_do} in
    let x = Var.create 10 in
    let y = Var.create 20 in
    let dbl_sum =
      Par_incr.run ~executor
      @@ let& x = map ~fn:(fun x -> 2 * x) (Var.watch x)
         and& y = map ~fn:(fun y -> 2 * y) (Var.watch y) in
         x + y
    in
    let par_do_call_check = "par_do call check" in
    Alcotest.(check int) output_check 60 (value dbl_sum);
    Alcotest.(check int) reader_check 1 (Var.num_readers x);
    Alcotest.(check int) reader_check 1 (Var.num_readers y);
    Alcotest.(check int) par_do_call_check 1 (Atomic.get par_do_call_count);

    let () = Var.set x 30 in
    let () = propagate dbl_sum in
    Alcotest.(check int) output_check 100 (value dbl_sum);
    (*Wouldn't call par_do since only one side was affected*)
    Alcotest.(check int) par_do_call_check 1 (Atomic.get par_do_call_count);

    let expected_par_call_cnt = ref 1 in

    for i = 1 to 10 do
      incr expected_par_call_cnt;
      for j = 1 to 10 do
        let () = Var.set x i in
        let () = Var.set y j in
        let () = propagate dbl_sum in

        Alcotest.(check int) output_check (2 * (i + j)) (value dbl_sum);
        Alcotest.(check int)
          par_do_call_check !expected_par_call_cnt
          (Atomic.get par_do_call_count)
      done
    done;

    T.teardown_pool pool;
    (*propagating with torn down pool raises exception, hence not legal
      behaviour*)
    Alcotest.check_raises "running propagate with torn down pool raises exn"
      (Invalid_argument "pool already torn down") (fun () -> propagate dbl_sum);
    destroy_comp dbl_sum;
    ()
  in
  Fun.protect
  (*Make sure pool is torn down, else this might interfere somehow with other
    tests that check gc stats*)
    ~finally:(fun () ->
      (*Handle exn in case teardown ran twice*)
      try T.teardown_pool pool with _ -> ())
    test

(* I have to check for leaks in sequential version of par because no. of live
   words with domains becomes weird. This test makes sure we don't have leaks in
   `par` operation.*)
let par_test_for_gc () =
  let open Syntax in
  let par_do_call_count = ref 0 in
  let run = seq_executor.run in
  let par_do l r =
    incr par_do_call_count;
    (*We don't need atomic incr here because we're
      anyway doing things sequentially*)
    seq_executor.par_do l r
  in
  let executor = {run; par_do} in
  let x = Var.create 10 in
  let y = Var.create 20 in
  let dbl_sum =
    Par_incr.run ~executor
    @@ let& x = map ~fn:(fun x -> 2 * x) (Var.watch x)
       and& y = map ~fn:(fun y -> 2 * y) (Var.watch y) in
       x + y
  in
  let live_words_at_start = live_words () in
  let par_do_call_check = "par_do call check" in

  let () = Var.set x 30 in
  let () = propagate dbl_sum in
  Alcotest.(check int) output_check 100 (value dbl_sum);
  (*Wouldn't call par_do since only one side was affected*)
  Alcotest.(check int) par_do_call_check 1 !par_do_call_count;
  Alcotest.(check bool) no_leak_check true (live_words () <= live_words_at_start);

  let expected_par_call_cnt = ref 1 in

  for i = 1 to 10 do
    incr expected_par_call_cnt;
    for j = 1 to 10 do
      let () = Var.set x i in
      let () = Var.set y j in
      let () = propagate dbl_sum in

      Alcotest.(check int) output_check (2 * (i + j)) (value dbl_sum);
      Alcotest.(check int)
        par_do_call_check !expected_par_call_cnt !par_do_call_count;
      Alcotest.(check bool)
        no_leak_check true
        (live_words () <= live_words_at_start)
    done
  done;

  destroy_comp dbl_sum;
  ()

(*We'll check the change propagation, and make sure there's no leak. The test is
  designed in a way such that on changing the condition, the computation tree
  that gets built is different. One has a few extra node than the other one. We
  ensure that on changing state from false to true and then back to false will
  have no leaks. We've tested the dynamic nature of bind too by checking the
  no. of readers being changed for different Var.t that are used. *)
let bind_test () =
  let cond = Var.create false in
  let true_const = Var.create true in
  let weird_not_computation =
    run ~executor:seq_executor
    @@ bind
         ~fn:(fun c ->
           if c then map ~fn:not (Var.watch cond) else Var.watch true_const)
         (Var.watch cond)
  in
  let gc_stat1 = live_words () in

  (*Else branch means just one reader for the cond variable and one for false_var*)
  Alcotest.(check int) reader_check 1 (Var.num_readers cond);
  Alcotest.(check int) reader_check 1 (Var.num_readers true_const);
  Alcotest.(check bool) output_check true (value weird_not_computation);

  let () = Var.set cond true in
  let () = propagate weird_not_computation in

  (*if then branch means just two reader for the cond variable*)
  Alcotest.(check int) reader_check 2 (Var.num_readers cond);
  Alcotest.(check int) reader_check 0 (Var.num_readers true_const);
  Alcotest.(check bool) output_check false (value weird_not_computation);

  let () = Var.set cond false in
  let () = propagate weird_not_computation in

  Alcotest.(check int) reader_check 1 (Var.num_readers cond);
  Alcotest.(check int) reader_check 1 (Var.num_readers true_const);
  Alcotest.(check bool) output_check true (value weird_not_computation);

  (*Making sure there's no leaks with bind*)
  Alcotest.(check bool) "live words remain same" true (live_words () <= gc_stat1);
  destroy_comp weird_not_computation

(*This is a test one some larger computation. All the test above have been very
  simple. In this one,we'll test propagation, cutoffs and the structure of the
  computation as well in a way.*)
let internals_test () =
  let ops = ref 0 in
  let reset_ops () = ops := 0 in
  let rec sum_range ~lo ~hi xs =
    delay @@ fun () ->
    if hi - lo <= 1 then begin
      xs.(lo)
    end
    else
      let mid = lo + ((hi - lo) asr 1) in
      map2
        ~fn:(fun x y ->
          incr ops;
          (*Fine to use ref incr since we're not doing anything
            parallely*)
          Int.add x y)
        (sum_range ~lo ~hi:mid xs) (sum_range ~lo:mid ~hi xs)
  in
  let arr = Array.init 10 (( + ) 1) in
  let var_arr = Array.map Var.create arr in
  let t_arr = Array.map Var.watch var_arr in
  let sum = sum_range ~lo:0 ~hi:(Array.length t_arr) t_arr in
  let run f = f () in
  let par_do l r =
    let lres = l () in
    (lres, r ())
  in
  let executor = {run; par_do} in
  let sum_c = Par_incr.run ~executor sum in

  Alcotest.(check int) output_check 55 (value sum_c);

  (*
    Following the execution of sum_range function, we can figure out the number of
      addition operations

       Roughly looks like the following:
                                    +-----+
                                    |0,10 |
                                    ---+---
                                       |
                         +-------------v-------------+
                         |                           |
                       +-+----+                   +--+-+
                       |  0,5 |                   |5,10|
                       +-+----+                   +--+-+
                         |                           |
                         |                           |
             +-----------+-------+           +-------+------------+
             |                   |           |                    |
            ++---+             +-+---+     +-+--+               +-+--+
            |0,2 |             |  2,5|     | 5,7|               |7,10|
            +-+--+             +--+--+     +--+-+               +-+--+
              |                   |           |                   |
              |                   |           |                   |
    +---------+-----+     +-------+--+    +---+----+         +----+-------+
    |               |     |          |    |        |         |            |
+---+--+        +---+--+ ++----+ +---+-+ ++---+ +--+---+  +--+--+      +--+--+
| 0,1  |        |  1,2 | | 2,3 | | 3,5 | | 5,6| | 6,7  |  | 7,8 |      |8,10 |
+------+        +------+ +-----+ +--+--+ +----+ +------+  +-----+      +---+-+
                                    |                                      |
                              +-----+----+                           +-----+----+
                              v          v                        +------+    +-----+
                           +--+--+     +-+---+                    | 8,9  |    | 9,10|
                           | 3,4 |     | 4,5 |                    +------+    +-----+
                           +-----+     +-----+

    Diagram credits: https://asciiflow.com/legacy/
    *)
  Alcotest.(check int) "No of add operations executed" 9 !ops;

  reset_ops ();

  Var.Syntax.(
    (*This computation should cutoff early since the value of the sum of these two won't change*)
    var_arr.(0) := 0;
    var_arr.(1) := 3);

  propagate sum_c;
  Alcotest.(check int) "No of add operations executed" 1 !ops;

  reset_ops ();

  Var.Syntax.(var_arr.(2) := !(var_arr.(2)) + 1);

  propagate sum_c;
  Alcotest.(check int) output_check 56 (value sum_c);
  Alcotest.(check int) "No of add operations executed" 3 !ops;

  destroy_comp sum_c

(* Again a large example that uses bind(but not really in a very
      natural way). We'll again be testing cutoff,output, no. of calls
   to different functions,etc. *)
let bind_internals_test () =
  let sum' xs = List.fold_left ( + ) 0 xs in
  let product xs = List.fold_left ( * ) 1 xs in
  let reduce ~zero ~one ~plus xs =
    let n = Array.length xs in
    if n = 0 then return zero
    else
      let rec reduce' lo hi =
        delay @@ fun () ->
        let delta = hi - lo in
        if delta = 1 then map ~fn:one xs.(lo)
        else
          let mid = lo + (delta asr 1) in
          map2 ~fn:plus (reduce' lo mid) (reduce' mid hi)
      in
      reduce' 0 n
  in
  let if' c t e =
    let open Syntax in
    let* c = c in
    if c then t else e
  in
  let add_cnt = ref 0 in
  let mul_cnt = ref 0 in
  let add x y =
    incr add_cnt;
    (*Nothing is parallel so ref incr is fine*)
    x + y
  in
  let mul x y =
    incr mul_cnt;
    (*Nothing is parallel so ref incr is fine*)
    x * y
  in
  let reset x = x := 0 in

  let arr = Array.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let var_arr = Array.map Var.create arr in
  let t_arr = Array.map Var.watch var_arr in
  let sum = reduce ~zero:0 ~one:Fun.id ~plus:add t_arr in
  let prod = reduce ~zero:1 ~one:Fun.id ~plus:mul t_arr in
  let assert_n_reader n =
    Array.iter
      (fun x -> Alcotest.(check int) reader_check n (Var.num_readers x))
      var_arr
  in
  assert_n_reader 0;

  (*
       Call stack tree for sum and prod

       Roughly looks like the following:

                                    +-----+
                                    |0,10 |
                                    ---+---
                                       |
                         +-------------v-------------+
                         |                           |
                       +-+----+                   +--+-+
                       |  0,5 |                   |5,10|
                       +-+----+                   +--+-+
                         |                           |
                         |                           |
             +-----------+-------+           +-------+------------+
             |                   |           |                    |
            ++---+             +-+---+     +-+--+               +-+--+
            |0,2 |             |  2,5|     | 5,7|               |7,10|
            +-+--+             +--+--+     +--+-+               +-+--+
              |                   |           |                   |
              |                   |           |                   |
    +---------+-----+     +-------+--+    +---+----+         +----+-------+
    |               |     |          |    |        |         |            |
+---+--+        +---+--+ ++----+ +---+-+ ++---+ +--+---+  +--+--+      +--+--+
| 0,1  |        |  1,2 | | 2,3 | | 3,5 | | 5,6| | 6,7  |  | 7,8 |      |8,10 |
+------+        +------+ +-----+ +--+--+ +----+ +------+  +-----+      +---+-+
                                    |                                      |
                              +-----+----+                           +-----+----+
                              v          v                        +------+    +-----+
                           +--+--+     +-+---+                    | 8,9  |    | 9,10|
                           | 3,4 |     | 4,5 |                    +------+    +-----+
                           +-----+     +-----+
    *)
  let pred = Var.create true in
  let comp = run ~executor:seq_executor @@ if' (Var.watch pred) sum prod in

  Alcotest.(check int)
    output_check
    (sum' [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])
    (value comp);
  assert_n_reader 1;
  Alcotest.(check int) "Check executed function" 9 !add_cnt;
  (*The prod branch is detached from the rsp tree*)
  Alcotest.(check int) "Check executed function" 0 !mul_cnt;

  Var.set var_arr.(3) 5;
  reset add_cnt;
  propagate comp;
  Alcotest.(check int) "Check executed function" 4 !add_cnt;
  Alcotest.(check int) "Check executed function" 0 !mul_cnt;
  Alcotest.(check int)
    output_check
    (sum' [1; 2; 3; 5; 5; 6; 7; 8; 9; 10])
    (value comp);
  assert_n_reader 1;

  Var.set pred false;
  reset add_cnt;
  propagate comp;

  Alcotest.(check int)
    output_check
    (product [1; 2; 3; 5; 5; 6; 7; 8; 9; 10])
    (value comp);
  Alcotest.(check int) "Check executed function" 0 !add_cnt;
  Alcotest.(check int) "Check executed function" 9 !mul_cnt;
  assert_n_reader 1;

  Var.set var_arr.(0) 10;
  reset mul_cnt;
  propagate comp;

  Alcotest.(check int)
    output_check
    (product [10; 2; 3; 5; 5; 6; 7; 8; 9; 10])
    (value comp);
  Alcotest.(check int) "Check executed function" 0 !add_cnt;
  Alcotest.(check int) "Check executed function" 3 !mul_cnt;
  assert_n_reader 1;

  Var.set pred true;
  reset mul_cnt;
  propagate comp;

  Alcotest.(check int)
    output_check
    (sum' [10; 2; 3; 5; 5; 6; 7; 8; 9; 10])
    (value comp);

  (*The old add computation was fully destroyed when it was detached so
    it recomputes everything*)
  Alcotest.(check int) "Check executed function" 9 !add_cnt;
  Alcotest.(check int) "Check executed function" 0 !mul_cnt;
  assert_n_reader 1;

  destroy_comp comp

let () =
  Alcotest.run "par_incr"
    [
      ( "basic_tests",
        [
          Alcotest.test_case "simple" `Quick simple_test;
          Alcotest.test_case "destroy" `Quick destroy_test;
          Alcotest.test_case "map" `Quick map_test;
          Alcotest.test_case "combine" `Quick combine_test;
          Alcotest.test_case "par" `Quick par_test;
          Alcotest.test_case "par_test_for_gc" `Quick par_test_for_gc;
          Alcotest.test_case "bind" `Quick bind_test;
        ] );
      ( "internals_test",
        [
          Alcotest.test_case "internals" `Quick internals_test;
          Alcotest.test_case "bind_internals" `Quick bind_internals_test;
        ] );
    ]
