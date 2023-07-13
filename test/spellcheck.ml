module Incr = Par_incr
open Incr
module T = Domainslib.Task
module Js_incr = Incremental.Make ()

let () = Random.self_init ()
let usage_msg = "spellcheck [-n <int>] [-r <int>] [-c <int>]"
let no_of_entries = ref 10
let runs = ref 10
let no_of_input_changes = ref 2

let speclist =
  [
    ("-n", Arg.Set_int no_of_entries, "No. of words(Default:10)");
    ("-r", Arg.Set_int runs, "No. of runs for benchmarking(Default:10)");
    ( "-c",
      Arg.Set_int no_of_input_changes,
      "No. of changes to make to input before propagating(Default:2)" );
  ]

let () = Arg.parse speclist ignore usage_msg
let pool, par_executor = Utils.get_par_executor ~num_domains:4 ()

let edit_distance (a : bytes) (b : bytes) =
  let n = Bytes.length a in
  let m = Bytes.length b in
  let dp =
    Array.init (n + 1) (fun i ->
        Array.init (m + 1) (fun j ->
            if j == 0 then i else if i == 0 then j else 0))
  in
  for i = 1 to n do
    for j = 1 to m do
      if Bytes.get a (i - 1) = Bytes.get b (j - 1) then
        dp.(i).(j) <- dp.(i - 1).(j - 1)
      else
        let mn = min (min dp.(i - 1).(j - 1) dp.(i - 1).(j)) dp.(i).(j - 1) in
        dp.(i).(j) <- mn + 1
    done
  done;
  dp.(n).(m)

let get_random_string len = Bytes.init len (fun _ -> Random.int 126 |> Char.chr)
let inf = Int.max_int

let min_distance tgt words ~mode =
  Utils.reduce_arr ~mode inf (fun word -> edit_distance tgt word) min words

let min_distance' tgt xs =
  let open Current_incr in
  let rec f lo hi =
    let delta = hi - lo in
    if delta = 1 then
      of_cc @@ read xs.(lo) (fun word -> write (edit_distance tgt word))
    else
      let mid = lo + (delta asr 1) in
      let lhalf = f lo mid in
      let rhalf = f mid hi in
      of_cc
      @@ read lhalf (fun l ->
             Current_incr.read rhalf (fun r -> write (min l r)))
  in
  f 0 (Array.length xs)

let min_distance'' tgt xs =
  let rec f lo hi =
    let delta = hi - lo in
    if delta = 1 then Js_incr.map xs.(lo) ~f:(edit_distance tgt)
    else
      let mid = lo + (delta asr 1) in
      let lhalf = f lo mid in
      let rhalf = f mid hi in
      Js_incr.map2 lhalf rhalf ~f:(fun x y -> min x y)
  in
  f 0 (Array.length xs)

let par_static_min_edit_distance tgt words =
  par_executor.run (fun () ->
      T.parallel_for_reduce ~start:0
        ~finish:(Array.length words - 1)
        ~body:(fun i -> edit_distance tgt words.(i))
        pool min inf)

let word_len = 80
let runs = !runs
let target_word = get_random_string word_len
let rand_words = Array.init !no_of_entries (fun _ -> get_random_string word_len)
let rand_var_words = Array.map Var.create rand_words
let rand_t_words = Array.map Var.watch rand_var_words
let rand_ci_var_words = Array.map Current_incr.var rand_words
let rand_ci_t_words = Array.map Current_incr.of_var rand_ci_var_words
let rand_js_var_words = Array.map Js_incr.Var.create rand_words
let rand_js_t_words = Array.map Js_incr.Var.watch rand_js_var_words

(* Saving keystrokes *)
let run_incr = Incr.run ~executor:par_executor

let change_some_words ~for' () =
  for _ = 0 to !no_of_input_changes do
    let new_str = get_random_string word_len in
    let ind = Random.int !no_of_entries in
    match for' with
    | `Current_incr -> Current_incr.change rand_ci_var_words.(ind) new_str
    | `Par_incr -> Var.set rand_var_words.(ind) new_str
    | `Js_incr -> Js_incr.Var.set rand_js_var_words.(ind) new_str
  done

let () =
  Printf.printf
    "# Min edit distance with %d random words | %d words changed during \
     propagation\n"
    !no_of_entries !no_of_input_changes;
  let static_seq_res = ref 0 in
  let static_par_res = ref 0 in
  let static_seq =
    Bench.run ~runs ~name:"static-seq-spellcheck"
      ~f:(fun () ->
        Array.fold_left
          (fun acc x -> min (edit_distance target_word x) acc)
          inf rand_words)
      ~post:(fun x -> static_seq_res := x)
      ()
  in
  let static_par =
    Bench.run ~runs ~name:"static-par-spellcheck"
      ~f:(fun () -> par_static_min_edit_distance target_word rand_words)
      ~post:(fun x -> static_par_res := x)
      ()
  in
  assert (!static_seq_res = !static_par_res);

  let incr_seq_init =
    Bench.run ~runs ~name:"incr-seq-spellcheck-initial-cons"
      ~f:(fun () -> run_incr (min_distance ~mode:`Seq target_word rand_t_words))
      ~post:(fun c ->
        assert (Incr.value c = !static_par_res);
        destroy_comp c)
      ()
  in
  let incr_par_init =
    Bench.run ~runs ~name:"incr-par-spellcheck-initial-cons"
      ~f:(fun () -> run_incr (min_distance ~mode:`Par target_word rand_t_words))
      ~post:(fun c ->
        assert (Incr.value c = !static_par_res);
        destroy_comp c)
      ()
  in

  let ci_init =
    Bench.run ~runs ~name:"current-incr-spellcheck-initial-cons"
      ~f:(fun () -> min_distance' target_word rand_ci_t_words)
      ~post:(fun t ->
        assert (Current_incr.observe t = !static_par_res);
        for i = 0 to Array.length rand_words - 1 do
          rand_ci_var_words.(i) <-
            rand_ci_t_words.(i) |> Current_incr.observe |> Current_incr.var;
          rand_ci_t_words.(i) <- Current_incr.of_var rand_ci_var_words.(i)
        done)
      ()
  in

  let js_init =
    Bench.run ~runs ~name:"js-incr-spellcheck-initial-cons"
      ~f:(fun () ->
        let t = Js_incr.observe (min_distance'' target_word rand_js_t_words) in
        Js_incr.stabilize ();
        t)
      ~post:(fun t ->
        assert (Js_incr.Observer.value_exn t = !static_par_res);
        Js_incr.Observer.disallow_future_use t)
      ()
  in

  let incr_seq_spellcheck_comp =
    run_incr (min_distance ~mode:`Seq target_word rand_t_words)
  in
  let incr_seq_spellcheck_prop =
    Bench.run ~runs ~name:"incr-seq-spellcheck-change-prop"
      ~pre:(change_some_words ~for':`Par_incr)
      ~f:(fun () -> Incr.propagate incr_seq_spellcheck_comp)
      ~post:(fun _ ->
        assert (
          Incr.value incr_seq_spellcheck_comp
          = par_static_min_edit_distance target_word
              (rand_var_words |> Array.map Var.value)))
      ()
  in
  destroy_comp incr_seq_spellcheck_comp;

  let incr_par_spellcheck_comp =
    run_incr (min_distance ~mode:`Par target_word rand_t_words)
  in
  let incr_par_spellcheck_prop =
    Bench.run ~runs ~name:"incr-par-spellcheck-change-prop"
      ~pre:(change_some_words ~for':`Par_incr)
      ~f:(fun () -> Incr.propagate incr_par_spellcheck_comp)
      ~post:(fun _ ->
        assert (
          Incr.value incr_par_spellcheck_comp
          = par_static_min_edit_distance target_word
              (rand_var_words |> Array.map Var.value)))
      ()
  in
  destroy_comp incr_par_spellcheck_comp;

  let js_comp =
    let t = Js_incr.observe (min_distance'' target_word rand_js_t_words) in
    Js_incr.stabilize ();
    t
  in
  let js_spellcheck_prop =
    Bench.run ~runs ~name:"js-incr-spellcheck-change-prop"
      ~pre:(change_some_words ~for':`Js_incr)
      ~f:(fun () -> Js_incr.stabilize ())
      ~post:(fun _ ->
        assert (
          Js_incr.Observer.value_exn js_comp
          = par_static_min_edit_distance target_word
              (Array.map Js_incr.Var.value rand_js_var_words)))
      ()
  in
  Js_incr.Observer.disallow_future_use js_comp;
  Gc.full_major ();

  let ci_comp = min_distance' target_word rand_ci_t_words in
  let ci_spellcheck_prop =
    Bench.run ~runs ~name:"current-incr-spellcheck-change-prop"
      ~pre:(change_some_words ~for':`Current_incr)
      ~f:(fun () -> Current_incr.propagate ())
      ~post:(fun _ ->
        assert (
          Current_incr.observe ci_comp
          = par_static_min_edit_distance target_word
              (Array.map Current_incr.observe rand_ci_t_words)))
      ()
  in

  print_endline "## Initial computation";
  Bench.report
    [static_seq; static_par; incr_seq_init; incr_par_init; ci_init; js_init];
  print_endline "## Change propagation";
  Bench.report
    [
      incr_seq_spellcheck_prop;
      incr_par_spellcheck_prop;
      js_spellcheck_prop;
      ci_spellcheck_prop;
    ];
  T.teardown_pool pool
