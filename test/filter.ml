module Incr = Par_incr
open Incr
module T = Domainslib.Task

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

type 'a lst = Nil | Cons of 'a * 'a lst ref

let[@tail_mod_cons] rec lst_to_list l =
  match l with Nil -> [] | Cons (x, xs) -> x :: lst_to_list !xs

let lst_eq a b =
  let x, _ = a in
  let y, _ = b in
  match (x, y) with Nil, Nil -> true | _ -> false

let filter ~mode ~fn (xs : int Incr.t list) =
  let f : 'a -> (int -> 'a) -> ('a -> 'a -> 'a) -> int Incr.t list -> 'a t =
    Utils.reduce_lst ~eq:lst_eq ~mode
  in
  f
    (Nil, Obj.magic ())
    (fun x ->
      if fn x then begin
        let last_el = ref Nil in
        (Cons (x, last_el), last_el)
      end
      else (Nil, Obj.magic ()))
    (fun (x, xlast) (y, ylast) ->
      match (x, y) with
      | Nil, Nil -> (Nil, Obj.magic ())
      | Nil, Cons (_, _) -> (y, ylast)
      | Cons (_, _), Nil ->
        xlast := y;
        (x, xlast)
      | _ ->
        xlast := y;
        (x, ylast))
    xs

let () = Random.self_init ()
let lst = List.init !no_of_entries (fun _ -> !no_of_entries |> Random.int)
let arr = Array.of_list lst
let var_lst = List.map Var.create lst
let t_lst = List.map Var.watch var_lst
let runs = !runs

(*Saving keystrokes *)
let run_incr = Incr.run ~executor:par_executor

let change_inputs () =
  for _ = 1 to !no_of_input_changes do
    let index = Random.int !no_of_entries in
    let var = List.nth var_lst index in
    let new_val = Random.int !no_of_entries in
    Var.set var new_val;
    arr.(index) <- new_val
  done

let () =
  Printf.printf
    "Filtering list of size: %d | %d elements changed in propagation\n"
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
  let filter_seq_comp =
    run_incr (filter ~mode:`Seq ~fn:(fun x -> x mod 2 = 0) t_lst)
  in
  let incr_seq_filter_change_prop =
    Bench.run ~name:"incr-seq-filter-change-prop" ~pre:change_inputs ~runs
      ~f:(fun () -> propagate filter_seq_comp)
      ~post:(fun _ ->
        let lst, _ = Incr.value filter_seq_comp in

        let res_list = lst_to_list lst in
        let exp_list =
          arr |> Array.to_list |> List.filter (fun x -> x mod 2 = 0)
        in

        assert (res_list = exp_list))
      ()
  in
  destroy_comp filter_seq_comp;

  let filter_par_comp =
    run_incr (filter ~mode:`Par ~fn:(fun x -> x mod 2 = 0) t_lst)
  in
  let incr_par_filter_change_prop =
    Bench.run ~name:"incr-par-filter-change-prop" ~pre:change_inputs ~runs
      ~f:(fun () -> propagate filter_par_comp)
      ~post:(fun _ ->
        let lst, _ = Incr.value filter_par_comp in

        let res_list = lst_to_list lst in
        let exp_list =
          arr |> Array.to_list |> List.filter (fun x -> x mod 2 = 0)
        in

        assert (res_list = exp_list))
      ()
  in
  destroy_comp filter_par_comp;
  Bench.report
    [
      static_filter;
      incr_seq_filter_initial_cons;
      incr_par_filter_initial_cons;
      incr_seq_filter_change_prop;
      incr_par_filter_change_prop;
    ];
  T.teardown_pool pool
