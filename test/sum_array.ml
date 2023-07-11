module Incr = Par_incr
open Incr
open Incr.Syntax
module T = Domainslib.Task
module Js_incr = Incremental.Make ()

let usage_msg = "sum_array [-n <int>] [-r <int>] [-c <int>]"
let no_of_entries = ref 100000
let runs = ref 10
let no_of_input_changes = ref 500

let speclist =
  [
    ( "-n",
      Arg.Set_int no_of_entries,
      "No. of elements in the array to be summed(Default:100000)" );
    ("-r", Arg.Set_int runs, "No. of runs for benchmarking(Default:10)");
    ( "-c",
      Arg.Set_int no_of_input_changes,
      "No. of changes to make to input before propagating(Default:500)" );
  ]

let () = Arg.parse speclist ignore usage_msg
let pool, par_executor = Utils.get_par_executor ~num_domains:4 ()

let rec sum_range ~lo ~hi xs =
  delay @@ fun () ->
  if hi - lo <= 1 then begin
    xs.(lo)
  end
  else
    let mid = lo + ((hi - lo) asr 1) in
    let+ lhalf = sum_range ~lo ~hi:mid xs
    and+ rhalf = sum_range ~lo:mid ~hi xs in
    lhalf + rhalf

let rec sum_range_par ~lo ~hi xs =
  Incr.delay @@ fun () ->
  if hi - lo <= 32 then begin
    sum_range ~lo ~hi xs
  end
  else
    let mid = lo + ((hi - lo) asr 1) in
    let& lhalf = sum_range_par ~lo ~hi:mid xs
    and& rhalf = sum_range_par ~lo:mid ~hi xs in
    lhalf + rhalf

let rec sum_range' ~lo ~hi xs =
  let open Current_incr in
  let delta = hi - lo in
  if delta = 1 then xs.(lo)
  else
    let mid = lo + (delta asr 1) in
    let lhalf = sum_range' ~lo ~hi:mid xs in
    let rhalf = sum_range' ~lo:mid ~hi xs in
    of_cc
    @@ read lhalf (fun l -> Current_incr.read rhalf (fun r -> write (l + r)))

let rec sum_range'' ~lo ~hi xs =
  let delta = hi - lo in
  if delta = 1 then xs.(lo)
  else
    let mid = lo + (delta asr 1) in
    let lhalf = sum_range'' ~lo ~hi:mid xs in
    let rhalf = sum_range'' ~lo:mid ~hi xs in
    Js_incr.map2 lhalf rhalf ~f:(fun x y -> x + y)

let () = Random.self_init ()
let arr = Array.init !no_of_entries (fun _ -> !no_of_entries |> Random.int)
let var_arr = Array.map Var.create arr
let t_arr = Array.map Var.watch var_arr
let ci_var_arr = Array.map Current_incr.var arr
let ci_t_arr = Array.map Current_incr.of_var ci_var_arr
let js_var_arr = Array.map Js_incr.Var.create arr
let js_t_arr = Array.map Js_incr.Var.watch js_var_arr
let sum_par = sum_range_par ~lo:0 ~hi:!no_of_entries t_arr
let sum_seq = sum_range ~lo:0 ~hi:!no_of_entries t_arr

let change_inputs ~for' () =
  let n = !no_of_input_changes in
  for _ = 1 to n do
    let index = !no_of_entries |> Random.int in
    let new_val = Random.int !no_of_entries in
    match for' with
    | `Current_incr -> Current_incr.change ci_var_arr.(index) new_val
    | `Par_incr -> Var.set var_arr.(index) new_val
    | `Js_incr -> Js_incr.Var.set js_var_arr.(index) new_val
  done

let runs = !runs

(* We'll use the par_executor for everything *)
let run_incr = Incr.run ~executor:par_executor

let () =
  Printf.printf
    "# Sum of int array of size: %d | %d elements changed in propagation \
     benchmarks\n"
    !no_of_entries !no_of_input_changes;
  let sum_result = ref 0 in
  let static_seq_arr_sum =
    Bench.run ~name:"static-seq-arr-sum" ~runs
      ~f:(fun () -> Array.fold_left (fun acc x -> acc + x) 0 arr)
      ~post:(fun x -> sum_result := x)
      ()
  in
  let incr_seq_arr_sum_initial_cons =
    Bench.run ~name:"incr-seq-arr-sum-initial-cons" ~runs
      ~f:(fun () -> run_incr sum_seq)
      ~post:(fun c ->
        assert (Incr.value c = !sum_result);
        destroy_comp c)
      ()
  in
  let incr_par_arr_sum_initial_cons =
    Bench.run ~name:"incr-par-arr-sum-initial-cons" ~runs
      ~f:(fun () -> run_incr sum_par)
      ~post:(fun c ->
        assert (Incr.value c = !sum_result);
        destroy_comp c)
      ()
  in

  let current_incr_arr_sum_initial_cons =
    Bench.run ~name:"current-incr-arr-sum-initial-cons" ~runs
      ~f:(fun () -> sum_range' ~lo:0 ~hi:!no_of_entries ci_t_arr)
      ~post:(fun t ->
        assert (Current_incr.observe t = !sum_result);
        Gc.full_major ())
      ()
  in

  let js_incr_arr_sum_initial_cons =
    Bench.run ~name:"js-incr-arr-sum-initial-cons" ~runs
      ~f:(fun () ->
        let t = sum_range'' ~lo:0 ~hi:!no_of_entries js_t_arr in
        let obs = Js_incr.observe t in
        Js_incr.stabilize ();
        obs)
      ~post:(fun t ->
        assert (Js_incr.Observer.value_exn t = !sum_result);
        Js_incr.Observer.disallow_future_use t;
        Gc.full_major ())
      ()
  in

  let seq_comp = run_incr sum_seq in
  let incr_seq_arr_sum_prop =
    Bench.run ~name:"incr-seq-arr-sum-prop"
      ~pre:(fun () ->
        change_inputs ~for':`Par_incr ();
        sum_result := Array.fold_left (fun acc x -> acc + Var.value x) 0 var_arr)
      ~runs
      ~f:(fun () -> propagate seq_comp)
      ~post:(fun _ -> assert (value seq_comp = !sum_result))
      ()
  in
  destroy_comp seq_comp;

  let par_comp = run_incr sum_par in
  let incr_par_arr_sum_prop =
    Bench.run ~name:"incr-par-arr-sum-prop"
      ~pre:(fun () ->
        change_inputs ~for':`Par_incr ();
        sum_result := Array.fold_left (fun acc x -> acc + Var.value x) 0 var_arr)
      ~runs
      ~f:(fun () -> propagate par_comp)
      ~post:(fun _ -> assert (value par_comp = !sum_result))
      ()
  in
  destroy_comp par_comp;
  let js_incr_comp =
    Js_incr.observe (sum_range'' ~lo:0 ~hi:!no_of_entries js_t_arr)
  in
  Js_incr.stabilize ();
  let js_incr_arr_sum_prop =
    Bench.run ~name:"js_incr-arr-sum-prop" ~runs
      ~pre:(fun () ->
        change_inputs ~for':`Js_incr ();
        sum_result :=
          Array.fold_left
            (fun acc x -> acc + (x |> Js_incr.Var.value))
            0 js_var_arr)
      ~f:(fun () -> Js_incr.stabilize ())
      ~post:(fun _ ->
        assert (Js_incr.Observer.value_exn js_incr_comp = !sum_result))
      ()
  in
  Js_incr.Observer.disallow_future_use js_incr_comp;
  Gc.full_major ();

  let ci_comp = sum_range' ~lo:0 ~hi:!no_of_entries ci_t_arr in
  let current_incr_arr_sum_prop =
    Bench.run ~name:"current-incr-arr-sum-prop" ~runs
      ~pre:(fun () ->
        change_inputs ~for':`Current_incr ();
        sum_result :=
          Array.fold_left
            (fun acc x ->
              acc + (x |> Current_incr.of_var |> Current_incr.observe))
            0 ci_var_arr)
      ~f:(fun () -> Current_incr.propagate ())
      ~post:(fun _ -> assert (Current_incr.observe ci_comp = !sum_result))
      ()
  in

  print_endline "## Initial computation";
  Bench.report
    [
      static_seq_arr_sum;
      incr_seq_arr_sum_initial_cons;
      incr_par_arr_sum_initial_cons;
      current_incr_arr_sum_initial_cons;
      js_incr_arr_sum_initial_cons;
    ];
  print_endline "## Change propagation";
  Bench.report
    [
      incr_seq_arr_sum_prop;
      incr_par_arr_sum_prop;
      current_incr_arr_sum_prop;
      js_incr_arr_sum_prop;
    ]

let () = T.teardown_pool pool
