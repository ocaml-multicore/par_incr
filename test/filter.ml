module Incr = Par_incr
open Incr
module T = Domainslib.Task
module Js_incr = Incremental.Make ()
module M = Utils.M (Js_incr)

let () = Random.self_init ()
let usage_msg = "filter [-n <int>] [-r <int>] [-c <int>]"
let no_of_entries = ref 1000
let runs = ref 10
let no_of_input_changes = ref 5

let speclist =
  [
    ("-n", Arg.Set_int no_of_entries, "No. of elements in list (Default: 1000)");
    ("-r", Arg.Set_int runs, "No. of runs for benchmarking (Default: 10)");
    ( "-c",
      Arg.Set_int no_of_input_changes,
      "No. of changes to make to input before propagating (Default: 5)" );
  ]

let () = Arg.parse speclist ignore usage_msg
let pool, par_executor = Utils.get_par_executor ~num_domains:4 ()

type 'a lst = Nil | Cons of {value : 'a; mutable rest : 'a lst}

let[@tail_mod_cons] rec lst_to_list l =
  match l with Nil -> [] | Cons {value; rest} -> value :: lst_to_list rest

let lst_eq a b =
  let x, _ = a in
  let y, _ = b in
  match (x, y) with Nil, Nil -> true | _ -> false

let set_rest lst rest = match lst with Nil -> () | Cons l -> l.rest <- rest

let one_fn fn x =
  if fn x then
    let res = Cons {value = x; rest = Nil} in
    (res, res)
  else (Nil, Nil)

let combine_fn (x, xlast) (y, ylast) =
  match (x, y) with
  | Nil, _ -> (y, ylast)
  | Cons _, Nil ->
    set_rest xlast y;
    (x, xlast)
  | _ ->
    set_rest xlast y;
    (x, ylast)

let filter ~mode ~fn (xs : int Incr.t list) =
  let f : 'a -> (int -> 'a) -> ('a -> 'a -> 'a) -> int Incr.t list -> 'a t =
    Utils.reduce_lst ~eq:lst_eq ~mode
  in
  f (Nil, Nil) (one_fn fn) combine_fn xs

let current_incr_filter ~fn (xs : int Current_incr.t list) =
  let f = M.reduce_lst ~incr_type:M.Current_incr ~eq:lst_eq in
  f (Nil, Nil) (one_fn fn) combine_fn xs

let js_incr_filter ~fn (xs : int Js_incr.t list) =
  let f = M.reduce_lst ~incr_type:M.Js_incr ~eq:lst_eq in
  f (Nil, Nil) (one_fn fn) combine_fn xs

let () = Random.self_init ()
let lst = List.init !no_of_entries (fun _ -> !no_of_entries |> Random.int)
let var_lst = List.map Var.create lst
let t_lst = List.map Var.watch var_lst
let ci_var_lst = List.map Current_incr.var lst
let ci_t_lst = List.map Current_incr.of_var ci_var_lst
let js_var_lst = List.map Js_incr.Var.create lst
let js_t_lst = List.map Js_incr.Var.watch js_var_lst
let runs = !runs

(*Saving keystrokes *)
let run_incr = Incr.run ~executor:par_executor

let change_inputs ~for' () =
  for _ = 1 to !no_of_input_changes do
    let index = Random.int !no_of_entries in
    let new_val = Random.int !no_of_entries in
    match for' with
    | `Par_incr -> Var.set (List.nth var_lst index) new_val
    | `Current_incr -> Current_incr.change (List.nth ci_var_lst index) new_val
    | `Js_incr -> Js_incr.Var.set (List.nth js_var_lst index) new_val
  done

let () =
  Printf.printf
    "# Filtering list of size: %d | %d elements changed in propagation\n"
    !no_of_entries !no_of_input_changes;
  let expected_res = ref [] in
  let static_filter =
    Bench.run ~name:"static-filter" ~runs
      ~f:(fun () -> List.filter (fun x -> x mod 2 = 0) lst)
      ~post:(fun r -> expected_res := r)
      ()
  in
  let incr_seq_filter_initial_cons =
    Bench.run ~name:"incr-seq-filter-initial-cons"
      ~f:(fun () ->
        run_incr (filter ~mode:`Seq ~fn:(fun x -> x mod 2 = 0) t_lst))
      ~post:(fun c ->
        let lst, _ = Incr.value c in

        assert (lst_to_list lst = !expected_res);
        destroy_comp c)
      ()
  in
  let incr_par_filter_initial_cons =
    Bench.run ~name:"incr-par-filter-initial-cons"
      ~f:(fun () ->
        run_incr (filter ~mode:`Par ~fn:(fun x -> x mod 2 = 0) t_lst))
      ~post:(fun c ->
        let lst, _ = Incr.value c in

        assert (lst_to_list lst = !expected_res);
        destroy_comp c)
      ()
  in

  let ci_filter_initial_cons =
    Bench.run ~runs ~name:"current-incr-filter-init-cons"
      ~f:(fun () -> current_incr_filter ~fn:(fun x -> x mod 2 = 0) ci_t_lst)
      ~post:(fun c ->
        let lst, _ = Current_incr.observe c in

        assert (lst_to_list lst = !expected_res))
      ()
  in

  let js_filter_initial_cons =
    Bench.run ~runs ~name:"js-incr-filter-init-cons"
      ~f:(fun () ->
        let t =
          js_incr_filter ~fn:(fun x -> x mod 2 = 0) js_t_lst |> Js_incr.observe
        in
        Js_incr.stabilize ();
        t)
      ~post:(fun c ->
        let lst, _ = Js_incr.Observer.value_exn c in

        assert (lst_to_list lst = !expected_res);
        Js_incr.Observer.disallow_future_use c)
      ()
  in

  let filter_seq_comp =
    run_incr (filter ~mode:`Seq ~fn:(fun x -> x mod 2 = 0) t_lst)
  in
  let incr_seq_filter_change_prop =
    Bench.run ~name:"incr-seq-filter-change-prop"
      ~pre:(change_inputs ~for':`Par_incr)
      ~runs
      ~f:(fun () -> propagate filter_seq_comp)
      ~post:(fun _ ->
        let lst, _ = Incr.value filter_seq_comp in

        let res_list = lst_to_list lst in
        let exp_list =
          var_lst |> List.map Var.value |> List.filter (fun x -> x mod 2 = 0)
        in

        assert (res_list = exp_list))
      ()
  in
  destroy_comp filter_seq_comp;

  let filter_par_comp =
    run_incr (filter ~mode:`Par ~fn:(fun x -> x mod 2 = 0) t_lst)
  in
  let incr_par_filter_change_prop =
    Bench.run ~name:"incr-par-filter-change-prop"
      ~pre:(change_inputs ~for':`Par_incr)
      ~runs
      ~f:(fun () -> propagate filter_par_comp)
      ~post:(fun _ ->
        let lst, _ = Incr.value filter_par_comp in

        let res_list = lst_to_list lst in
        let exp_list =
          var_lst |> List.map Var.value |> List.filter (fun x -> x mod 2 = 0)
        in

        assert (res_list = exp_list))
      ()
  in
  destroy_comp filter_par_comp;

  let js_filter =
    js_incr_filter ~fn:(fun x -> x mod 2 = 0) js_t_lst |> Js_incr.observe
  in
  Js_incr.stabilize ();
  let js_filter_change_prop =
    Bench.run ~name:"js-incr-filter-change-prop"
      ~pre:(change_inputs ~for':`Js_incr)
      ~runs
      ~f:(fun () -> Js_incr.stabilize ())
      ~post:(fun _ ->
        let lst, _ = Js_incr.Observer.value_exn js_filter in
        let res_list = lst_to_list lst in
        let exp_list =
          js_var_lst |> List.map Js_incr.Var.value
          |> List.filter (fun x -> x mod 2 = 0)
        in

        assert (res_list = exp_list))
      ()
  in
  Gc.full_major ();

  let ci_filter = current_incr_filter ~fn:(fun x -> x mod 2 = 0) ci_t_lst in
  let ci_filter_change_prop =
    Bench.run ~name:"current-incr-filter-change-prop"
      ~pre:(change_inputs ~for':`Current_incr)
      ~runs
      ~f:(fun () -> Current_incr.propagate ())
      ~post:(fun _ ->
        let lst, _ = Current_incr.observe ci_filter in

        let res_list = lst_to_list lst in
        let exp_list =
          ci_t_lst
          |> List.map Current_incr.observe
          |> List.filter (fun x -> x mod 2 = 0)
        in

        assert (res_list = exp_list))
      ()
  in

  print_endline "## Initial computation";
  Bench.report
    [
      static_filter;
      incr_seq_filter_initial_cons;
      incr_par_filter_initial_cons;
      ci_filter_initial_cons;
      js_filter_initial_cons;
    ];

  print_endline "## Change propagation";
  Bench.report
    [
      incr_seq_filter_change_prop;
      incr_par_filter_change_prop;
      js_filter_change_prop;
      ci_filter_change_prop;
    ];
  T.teardown_pool pool
