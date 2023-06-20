open Par_incr

let seq_executor = {run = (fun f -> f ()); par_do = (fun l r -> (l (), r ()))}
let output_check = "Output check"
let no_readers_check = "No readers check"
let reader_check = "No of Readers check"
let no_leak_check = "No leaks or extra allocations check"

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let does_raise f = match f () with exception _ -> true | _ -> false

let simple_test () =
  begin
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
      Alcotest.(check int) no_leak_check live_words_before_loop (live_words ())
    done;

    let () = destroy_comp y in
    Alcotest.(check int) no_readers_check 0 (Var.num_readers x)
  end

let destroy_test () =
  begin
    let x = Var.create 10 in
    let y =
      run ~executor:seq_executor (map ~fn:(fun y -> 2 * y) (Var.watch x))
    in

    Alcotest.(check int) no_readers_check 1 (Var.num_readers x);
    let () =
      Alcotest.(check bool)
        "propagate doesn't raise before destroying" false
        (does_raise (fun () -> propagate y))
    in

    let () = destroy_comp y in
    Alcotest.(check bool)
      "propagate raises after destroying" true
      (does_raise (fun () -> propagate y));
    Alcotest.(check int) no_readers_check 0 (Var.num_readers x);

    (*'a computation = {var;root;e}, so root is 1st field(starting from 0)*)
    (* Dummy is the only empty constructor for Rsp.t, meaning it's value is 0  *)
    Alcotest.(check int)
      "root is changed to dummy node" 0
      (Obj.magic (Obj.field (Obj.repr y) 1))
  end

let map_test () =
  begin
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
      Alcotest.(check int) no_leak_check live_words_before_loop (live_words ())
    done;
    let () = destroy_comp y in
    Alcotest.(check int) reader_check 0 (Var.num_readers x)
  end

let combine_test () =
  begin
    let tup_eq (a', b') (a'', b'') = a' = a'' && String.equal b' b'' in

    let x = Var.create 10 in
    let y = Var.create "Ten" in
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
    Alcotest.(check int) reader_check 0 (Var.num_readers x);
    Alcotest.(check int) reader_check 0 (Var.num_readers y)
  end

let par_test () =
  begin
    let open Syntax in
    let module T = Domainslib.Task in
    let pool = T.setup_pool ~num_domains:2 () in
    let test () =
      begin
        let par_do_call_count = ref 0 in
        let par_runner f = T.run pool f in

        let par_do l r =
          incr par_do_call_count;
          let lres = T.async pool l in
          let rres = r () in
          (T.await pool lres, rres)
        in
        let executor = {run = par_runner; par_do} in
        let x = Var.create 10 in
        let y = Var.create 20 in
        let dbl_sum =
          run ~executor
          @@ let& x = map ~fn:(fun x -> 2 * x) (Var.watch x)
             and& y = map ~fn:(fun y -> 2 * y) (Var.watch y) in
             x + y
        in
        let par_do_call_check = "par_do call check" in
        Alcotest.(check int) output_check 60 (value dbl_sum);
        Alcotest.(check int) reader_check 1 (Var.num_readers x);
        Alcotest.(check int) reader_check 1 (Var.num_readers y);
        Alcotest.(check int) par_do_call_check 1 !par_do_call_count;

        let () = Var.set x 30 in
        let () = propagate dbl_sum in
        Alcotest.(check int) output_check 100 (value dbl_sum);
        (*Wouldn't call par_do since only one side was affected*)
        Alcotest.(check int) par_do_call_check 1 !par_do_call_count;

        let expected_par_call_cnt = ref 1 in

        for i = 1 to 10 do
          incr expected_par_call_cnt;
          for j = 1 to 10 do
            let () = Var.set x i in
            let () = Var.set y j in
            let () = propagate dbl_sum in

            Alcotest.(check int) output_check (2 * (i + j)) (value dbl_sum);
            Alcotest.(check int)
              par_do_call_check !expected_par_call_cnt !par_do_call_count
          done
        done;

        T.teardown_pool pool;
        (*propagating with torn down pool raises exception, hence not legal behaviour*)
        Alcotest.(check bool)
          "running propagate with torn down pool raises exn" true
          (does_raise (fun () -> propagate dbl_sum));

        destroy_comp dbl_sum;
        ()
      end
    in
    Fun.protect
    (*Make sure pool is torn down, else this might interfere somehow with other tests that check gc stats*)
      ~finally:(fun () ->
        (*Handle exn in case teardown ran twice*)
        try
          begin
            T.teardown_pool pool
          end
        with _ -> ())
      test
  end

(*I have to test leaks in sequential version of par because no. of live words with domains becomes weird*)
let par_test_for_gc () =
  begin
    let open Syntax in
    let par_do_call_count = ref 0 in
    let executor =
      {
        run = seq_executor.run;
        par_do =
          (fun l r ->
            incr par_do_call_count;
            seq_executor.par_do l r);
      }
    in
    let x = Var.create 10 in
    let y = Var.create 20 in
    let dbl_sum =
      run ~executor
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
    Alcotest.(check int) no_leak_check live_words_at_start (live_words ());

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
        Alcotest.(check int) no_leak_check live_words_at_start (live_words ())
      done
    done;

    destroy_comp dbl_sum;
    ()
  end

let bind_test () =
  begin
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
    Alcotest.(check int) "live words remain same" gc_stat1 (live_words ());
    destroy_comp weird_not_computation
  end

let internals_test () =
  begin
    let ops = Atomic.make 0 in
    let reset_ops () = Atomic.set ops 0 in
    let rec sum_range ~mode ~lo ~hi xs =
      delay @@ fun () ->
      if hi - lo <= 1 then begin
        xs.(lo)
      end
      else
        let mid = lo + ((hi - lo) asr 1) in
        map2 ~mode
          ~fn:(fun x y ->
            Atomic.incr ops;
            Int.add x y)
          (sum_range ~mode ~lo ~hi:mid xs)
          (sum_range ~mode ~lo:mid ~hi xs)
    in
    let arr = Array.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
    let var_arr = Array.map (Var.create ~to_s:Int.to_string) arr in
    let t_arr = Array.map Var.watch var_arr in
    let sum = sum_range ~mode:`Seq ~lo:0 ~hi:(Array.length t_arr) t_arr in
    let executor =
      {
        run = (fun f -> f ());
        par_do =
          (fun l r ->
            let lres = l () in
            (lres, r ()));
      }
    in
    let sum_c = run ~executor sum in

    Alcotest.(check int) output_check 55 (value sum_c);

    (*
    Following the execution of sum_range function, we can figure out the number of
      addition operations

       Roughly looks like the following:

                             0,10
                 0,5          |      5,10
            0,2  |  2-5            5,7 | 7,10
        0,1|1,2   2,3 | 3-5     5,6|6-7  7,8 | 8,10
                        3-4|4-5                8,9|9,10
    *)
    Alcotest.(check int) "No of add operations executed" 9 (Atomic.get ops);

    reset_ops ();

    let open Var.Syntax in
    (*This computation should cutoff early since the value of the sum of these two wont change*)
    var_arr.(0) := 0;
    var_arr.(1) := 3;

    propagate sum_c;
    Alcotest.(check int) "No of add operations executed" 1 (Atomic.get ops);

    reset_ops ();

    var_arr.(2) := !(var_arr.(2)) + 1;

    propagate sum_c;
    Alcotest.(check int) output_check 56 (value sum_c);
    Alcotest.(check int) "No of add operations executed" 3 (Atomic.get ops);

    destroy_comp sum_c
  end

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
          Alcotest.test_case "internals" `Quick internals_test;
        ] );
    ]
