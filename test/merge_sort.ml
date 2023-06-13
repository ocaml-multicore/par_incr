module Incr = Par_incr
open Incr
module T = Domainslib.Task

let usage_msg = "merge_sort [-n <int>] [-r <int>] [-c <int>]"
let no_of_entries = ref 100000
let runs = ref 10
let no_of_input_changes = ref 500

let speclist =
  [
    ( "-n",
      Arg.Set_int no_of_entries,
      "No. of elements in the array(Default:100000)" );
    ("-r", Arg.Set_int runs, "No. of runs for benchmarking(Default:10)");
    ( "-c",
      Arg.Set_int no_of_input_changes,
      "No. of changes to make to input before propagating(Default:500)" );
  ]

let () = Arg.parse speclist ignore usage_msg
let pool, par_executor = Utils.get_par_executor ~num_domains:4 ()

(*merge_sort credits: @polytypic*)
let[@tail_mod_cons] rec merge xs ys =
  match (xs, ys) with
  | [], ys -> ys
  | xs, [] -> xs
  | (x :: xs as xxs), (y :: ys as yys) ->
    if x <= y then x :: merge xs yys else y :: merge xxs ys

let rec merge_pairs = function
  | [] -> []
  | [x] -> [x]
  | x1 :: x2 :: xs -> merge x1 x2 :: merge_pairs xs

let msort_list xs =
  let rec loop = function [xs] -> xs | xss -> merge_pairs xss |> loop in
  xs |> List.map (fun x -> [x]) |> loop

let () = assert (msort_list [3; 1; 4; 1; 5; 9; 2] = [1; 1; 2; 3; 4; 5; 9])

let reduce ~mode zero one plus xs =
  let n = Array.length xs in
  if n = 0 then Incr.return zero
  else
    let rec reduce lo hi =
      Incr.delay @@ fun () ->
      let delta = hi - lo in
      if delta = 1 then Incr.map ~fn:one xs.(lo)
      else
        let mid = lo + (delta asr 1) in
        Incr.map2 ~mode ~fn:plus (reduce lo mid) (reduce mid hi)
    in
    reduce 0 n

let msort ~mode xss = reduce ~mode [] msort_list merge xss

let is_sorted = function
  | [] -> true
  | [_] -> true
  | x :: xs ->
    let res, _ =
      List.fold_left
        (fun (sorted_till_now, last) x -> (sorted_till_now && last <= x, x))
        (true, x) xs
    in
    res

let () = Random.self_init ()
let arr = Array.init !no_of_entries (fun _ -> !no_of_entries |> Random.int)
let var_arr = Array.map (fun x -> Var.create [x]) arr
let t_arr = Array.map Var.watch var_arr
let runs = !runs

(*Saving keystrokes *)
let run_incr = Incr.run ~executor:par_executor

let change_inputs () =
  let n = !no_of_input_changes in
  for _ = 1 to n do
    let index = !no_of_entries |> Random.int in
    Var.set var_arr.(index) [Random.int !no_of_entries]
  done

let () =
  Printf.printf
    "Sorting array of size: %d | %d elements changed in propagation\n"
    !no_of_entries !no_of_input_changes;
  let tmp_arr = ref [||] in
  let sorting_wo_lib =
    Bench.run ~name:"Sorting array w/o library" ~runs
      ~pre:(fun () -> tmp_arr := Array.map Fun.id arr)
      ~f:(fun () -> Array.fast_sort Int.compare !tmp_arr)
      ~post:(fun _ -> tmp_arr := [||])
      ()
  in
  let lst = Array.to_list arr in
  let sorting_w_msort_list =
    Bench.run ~name:"Sorting array with our merge_sort" ~runs
      ~f:(fun () -> msort_list lst)
      ~post:ignore ()
  in
  let sort_initial_seq =
    Bench.run ~name:"Initial sorting(seq)" ~runs
      ~f:(fun () -> run_incr (msort ~mode:`Seq t_arr))
      ~post:(fun c ->
        assert (is_sorted (value c));
        destroy_comp c)
      ()
  in
  let sort_initial_par =
    Bench.run ~name:"Initial sorting(par)" ~runs
      ~f:(fun () -> run_incr (msort ~mode:`Par t_arr))
      ~post:(fun c ->
        assert (is_sorted (value c));
        destroy_comp c)
      ()
  in
  let msort_seq_comp = run_incr (msort ~mode:`Seq t_arr) in
  let prop_seq_msort =
    Bench.run ~name:"Propagation(seq)" ~pre:change_inputs ~runs
      ~f:(fun () -> propagate msort_seq_comp)
      ~post:(fun _ -> assert (msort_seq_comp |> value |> is_sorted))
      ()
  in
  let append_element_prop_seq =
    Bench.run ~name:"Append Propagation(seq)"
      ~pre:(fun () ->
        (* Append to the last element *)
        let n = !no_of_entries in
        Var.set var_arr.(n - 1) (Random.int n :: Var.value var_arr.(n - 1)))
      ~runs
      ~f:(fun () -> propagate msort_seq_comp)
      ~post:(fun _ ->
        assert (msort_seq_comp |> value |> is_sorted);
        (*Undo the change*)
        let n = !no_of_entries in
        Var.set var_arr.(n - 1) (Var.value var_arr.(n - 1) |> List.tl))
      ()
  in

  destroy_comp msort_seq_comp;

  let msort_par_comp = run_incr (msort ~mode:`Par t_arr) in
  let prop_par_msort =
    Bench.run ~name:"Propagation(par)" ~pre:change_inputs ~runs
      ~f:(fun () -> propagate msort_par_comp)
      ~post:(fun _ -> assert (msort_par_comp |> value |> is_sorted))
      ()
  in
  let append_element_prop_par =
    Bench.run ~name:"Append Propagation(par)"
      ~pre:(fun () ->
        (* Append to the last element *)
        let n = !no_of_entries in
        Var.set var_arr.(n - 1) (Random.int n :: Var.value var_arr.(n - 1)))
      ~runs
      ~f:(fun () -> propagate msort_par_comp)
      ~post:(fun _ ->
        assert (msort_par_comp |> value |> is_sorted);
        (*Undo the change*)
        let n = !no_of_entries in
        Var.set var_arr.(n - 1) (Var.value var_arr.(n - 1) |> List.tl))
      ()
  in
  destroy_comp msort_par_comp;
  Bench.report
    [
      sorting_wo_lib;
      sorting_w_msort_list;
      sort_initial_seq;
      sort_initial_par;
      prop_seq_msort;
      prop_par_msort;
      append_element_prop_seq;
      append_element_prop_par;
    ]

let () = T.teardown_pool pool
