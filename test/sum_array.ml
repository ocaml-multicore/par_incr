module Incr = Par_incr
open Incr
open Incr.Syntax
module T = Domainslib.Task

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

let () = Random.self_init ()
let arr = Array.init !no_of_entries (fun _ -> !no_of_entries |> Random.int)
let var_arr = Array.map Var.create arr
let t_arr = Array.map Var.watch var_arr
let sum_par = sum_range_par ~lo:0 ~hi:!no_of_entries t_arr
let sum_seq = sum_range ~lo:0 ~hi:!no_of_entries t_arr

let change_inputs () =
  let n = !no_of_input_changes in
  for _ = 1 to n do
    let index = !no_of_entries |> Random.int in
    Var.set var_arr.(index) (Random.int !no_of_entries)
  done

let runs = !runs

(* We'll use the par_executor for everything *)
let run_incr = Incr.run ~executor:par_executor

let () =
  Printf.printf
    "Sum of int array of size: %d | %d elements changed in propagation \
     benchmarks\n"
    !no_of_entries !no_of_input_changes;
  let sum_result = ref 0 in
  let sum_wo_lib =
    Bench.run ~name:"Summing array w/o library" ~runs
      ~f:(fun () -> Array.fold_left (fun acc x -> acc + x) 0 arr)
      ~post:(fun x -> sum_result := x)
      ()
  in
  let sum_seq_lib =
    Bench.run ~name:"Initial computation(seq)" ~runs
      ~f:(fun () -> run_incr sum_seq)
      ~post:(fun c ->
        assert (Incr.value c = !sum_result);
        destroy_comp c)
      ()
  in
  let sum_par_lib =
    Bench.run ~name:"Initial computation(par)" ~runs
      ~f:(fun () -> run_incr sum_par)
      ~post:(fun c ->
        assert (Incr.value c = !sum_result);
        destroy_comp c)
      ()
  in
  let seq_comp = run_incr sum_seq in
  let prop_seq =
    Bench.run ~name:"Propagation(seq)"
      ~pre:(fun () ->
        change_inputs ();
        sum_result := Array.fold_left (fun acc x -> acc + Var.value x) 0 var_arr)
      ~runs
      ~f:(fun () -> propagate seq_comp)
      ~post:(fun _ -> assert (value seq_comp = !sum_result))
      ()
  in
  destroy_comp seq_comp;

  let par_comp = run_incr sum_par in
  let prop_par =
    Bench.run ~name:"Propagation(par)"
      ~pre:(fun () ->
        change_inputs ();
        sum_result := Array.fold_left (fun acc x -> acc + Var.value x) 0 var_arr)
      ~runs
      ~f:(fun () -> propagate par_comp)
      ~post:(fun _ -> assert (value par_comp = !sum_result))
      ()
  in
  destroy_comp par_comp;
  Bench.report [sum_wo_lib; sum_seq_lib; sum_par_lib; prop_seq; prop_par]

let () = T.teardown_pool pool
