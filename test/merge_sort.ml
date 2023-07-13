module Incr = Par_incr
open Incr
module T = Domainslib.Task
module Js_incr = Incremental.Make ()
module M = Utils.M (Js_incr)

let usage_msg = "merge_sort [-n <int>] [-r <int>] [-c <int>]"
let no_of_entries = ref 1000
let runs = ref 10
let no_of_input_changes = ref 50

let speclist =
  [
    ( "-n",
      Arg.Set_int no_of_entries,
      "No. of elements in the array (Default: 100000)" );
    ("-r", Arg.Set_int runs, "No. of runs for benchmarking (Default: 10)");
    ( "-c",
      Arg.Set_int no_of_input_changes,
      "No. of changes to make to input before propagating (Default: 500)" );
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
let msort ~mode xss = Utils.reduce_arr ~mode [] msort_list merge xss

let current_incr_msort xss =
  M.reduce_arr ~incr_type:M.Current_incr [] msort_list merge xss

let js_incr_msort xss =
  M.reduce_arr ~incr_type:M.Js_incr [] msort_list merge xss

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
let ci_var_arr = Array.map (fun x -> Current_incr.var [x]) arr
let ci_t_arr = Array.map Current_incr.of_var ci_var_arr
let js_var_arr = Array.map (fun x -> Js_incr.Var.create [x]) arr
let js_t_arr = Array.map Js_incr.Var.watch js_var_arr
let runs = !runs

(*Saving keystrokes *)
let run_incr = Incr.run ~executor:par_executor

let change_inputs ~for' () =
  let n = !no_of_input_changes in
  for _ = 1 to n do
    let index = !no_of_entries |> Random.int in
    let rand_num = Random.int !no_of_entries in
    match for' with
    | `Par_incr -> Var.set var_arr.(index) [rand_num]
    | `Current_incr -> Current_incr.change ci_var_arr.(index) [rand_num]
    | `Js_incr -> Js_incr.Var.set js_var_arr.(index) [rand_num]
  done

let () =
  Printf.printf
    "# Sorting array of size: %d | %d elements changed in propagation\n"
    !no_of_entries !no_of_input_changes;
  let tmp_arr = ref [||] in
  let static_seq_stdlib_sort =
    Bench.run ~name:"static-seq-stdlib-sort" ~runs
      ~pre:(fun () -> tmp_arr := Array.map Fun.id arr)
      ~f:(fun () -> Array.fast_sort Int.compare !tmp_arr)
      ~post:(fun _ -> tmp_arr := [||])
      ()
  in
  let lst = Array.to_list arr in
  let static_seq_custom_msort =
    Bench.run ~name:"static-seq-custom-msort" ~runs
      ~f:(fun () -> msort_list lst)
      ~post:ignore ()
  in
  let incr_seq_msort_initial_cons =
    Bench.run ~name:"incr-seq-msort-initial-cons" ~runs
      ~f:(fun () -> run_incr (msort ~mode:`Seq t_arr))
      ~post:(fun c ->
        assert (is_sorted (value c));
        destroy_comp c)
      ()
  in
  let incr_par_msort_initial_cons =
    Bench.run ~name:"incr-par-msort-initial-cons" ~runs
      ~f:(fun () -> run_incr (msort ~mode:`Par t_arr))
      ~post:(fun c ->
        assert (is_sorted (value c));
        destroy_comp c)
      ()
  in
  let ci_msort_initial_cons =
    Bench.run ~name:"current-incr-msort-initial-cons" ~runs
      ~f:(fun () -> current_incr_msort ci_t_arr)
      ~post:(fun c ->
        assert (is_sorted (Current_incr.observe c));
        for i = 0 to Array.length arr - 1 do
          ci_var_arr.(i) <-
            ci_t_arr.(i) |> Current_incr.observe |> Current_incr.var;
          ci_t_arr.(i) <- Current_incr.of_var ci_var_arr.(i)
        done)
      ()
  in

  let js_msort_initial_cons =
    Bench.run ~name:"js-incr-msort-initial-cons" ~runs
      ~f:(fun () ->
        let t = js_incr_msort js_t_arr |> Js_incr.observe in
        Js_incr.stabilize ();
        t)
      ~post:(fun t ->
        assert (is_sorted (Js_incr.Observer.value_exn t));
        Js_incr.Observer.disallow_future_use t)
      ()
  in

  let msort_seq_comp = run_incr (msort ~mode:`Seq t_arr) in
  let incr_seq_msort_change_prop =
    Bench.run ~name:"incr-seq-msort-change-prop"
      ~pre:(change_inputs ~for':`Par_incr)
      ~runs
      ~f:(fun () -> propagate msort_seq_comp)
      ~post:(fun _ -> assert (msort_seq_comp |> value |> is_sorted))
      ()
  in
  let incr_seq_msort_append_prop =
    Bench.run ~name:"incr-seq-msort-append-prop"
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
  let incr_par_msort_change_prop =
    Bench.run ~name:"incr-par-msort-change-prop"
      ~pre:(change_inputs ~for':`Par_incr)
      ~runs
      ~f:(fun () -> propagate msort_par_comp)
      ~post:(fun _ -> assert (msort_par_comp |> value |> is_sorted))
      ()
  in
  let incr_par_msort_append_prop =
    Bench.run ~name:"incr-par-msort-append-prop"
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

  let msort_ci_comp = current_incr_msort ci_t_arr in
  let ci_msort_change_prop =
    Bench.run ~name:"current-incr-msort-change-prop"
      ~pre:(change_inputs ~for':`Current_incr)
      ~runs
      ~f:(fun () -> Current_incr.propagate ())
      ~post:(fun _ ->
        assert (msort_ci_comp |> Current_incr.observe |> is_sorted))
      ()
  in
  let ci_msort_append_prop =
    Bench.run ~name:"current-incr-msort-append-prop"
      ~pre:(fun () ->
        (* Append to the last element *)
        let n = !no_of_entries in
        Current_incr.change
          ci_var_arr.(n - 1)
          (Random.int n :: Current_incr.observe ci_t_arr.(n - 1)))
      ~runs
      ~f:(fun () -> Current_incr.propagate ())
      ~post:(fun _ ->
        assert (msort_ci_comp |> Current_incr.observe |> is_sorted);
        (*Undo the change*)
        let n = !no_of_entries in
        Current_incr.change
          ci_var_arr.(n - 1)
          (Current_incr.observe ci_t_arr.(n - 1) |> List.tl))
      ()
  in
  Gc.full_major ();

  let js_comp =
    let t = js_incr_msort js_t_arr |> Js_incr.observe in
    Js_incr.stabilize ();
    t
  in

  let js_msort_change_prop =
    Bench.run ~name:"js-incr-msort-change-prop"
      ~pre:(change_inputs ~for':`Js_incr)
      ~runs
      ~f:(fun () -> Js_incr.stabilize ())
      ~post:(fun _ ->
        assert (js_comp |> Js_incr.Observer.value_exn |> is_sorted))
      ()
  in

  let js_msort_append_prop =
    Bench.run ~name:"js-incr-msort-append-prop"
      ~pre:(fun () ->
        (* Append to the last element *)
        let n = !no_of_entries in
        Js_incr.Var.set
          js_var_arr.(n - 1)
          (Random.int n :: Js_incr.Var.value js_var_arr.(n - 1)))
      ~runs
      ~f:(fun () -> Js_incr.stabilize ())
      ~post:(fun _ ->
        assert (js_comp |> Js_incr.Observer.value_exn |> is_sorted);
        (*Undo the change*)
        let n = !no_of_entries in
        Js_incr.Var.set
          js_var_arr.(n - 1)
          (Js_incr.Var.value js_var_arr.(n - 1) |> List.tl))
      ()
  in

  print_endline "## Initial computation";
  Bench.report
    [
      static_seq_stdlib_sort;
      static_seq_custom_msort;
      incr_seq_msort_initial_cons;
      incr_par_msort_initial_cons;
      ci_msort_initial_cons;
      js_msort_initial_cons;
    ];

  print_endline "## Change propagation";
  Bench.report
    [
      incr_seq_msort_change_prop;
      incr_par_msort_change_prop;
      js_msort_change_prop;
      ci_msort_change_prop;
    ];
  print_endline "## Append propagation";
  Bench.report
    [
      incr_seq_msort_append_prop;
      incr_par_msort_append_prop;
      js_msort_append_prop;
      ci_msort_append_prop;
    ]

let () = T.teardown_pool pool
