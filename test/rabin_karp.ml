module Incr = Par_incr
open Incr
module T = Domainslib.Task

let pool, par_executor = Utils.get_par_executor ~num_domains:4 ()
let chunk_size = 64
let prime_mod = 1000000007
let b = 128
let ( *% ) x y = x * y mod prime_mod
let ( +% ) x y = (x + y) mod prime_mod

module Hash = struct
  type t = {mutable result : int; mutable acc : int}

  let hash_chunk chnk =
    let t = {result = 0; acc = 1} in
    Bytes.iter
      (fun c ->
        let {result; acc} = t in
        let c' = Char.code c in
        t.result <- (result *% b) +% c';
        t.acc <- acc *% b)
      chnk;
    t

  let merge l r =
    {result = (l.result *% r.acc) +% r.result; acc = l.acc *% r.acc}
end

let usage_msg = "rabin_karp [-n <int>] [-r <int>] [-c <int>]"
let no_of_char = ref 100000
let runs = ref 10
let no_of_input_changes = ref 500

let speclist =
  [
    ("-n", Arg.Set_int no_of_char, "No. of char (Default: 100000)");
    ("-r", Arg.Set_int runs, "No. of runs for benchmarking (Default: 10)");
    ( "-c",
      Arg.Set_int no_of_input_changes,
      "No. of changes to make to input before propagating (Default: 500)" );
  ]

let () = Arg.parse speclist ignore usage_msg
let () = Random.self_init ()

let random_chunk size () =
  Bytes.init size (fun _ ->
      let del = Random.int 26 in
      'a' |> Char.code |> ( + ) del |> Char.chr)

let no_of_chunks = (!no_of_char + chunk_size) / chunk_size

let chunks =
  Array.init no_of_chunks (fun i ->
      random_chunk
        (if i = no_of_chunks - 1 then !no_of_char - (i * chunk_size)
         else chunk_size)
        ())

let chunks_var = Array.map Var.create chunks
let chunks_incr = Array.map Var.watch chunks_var

let rabin_karp_static_par chunks =
  let rec f l r =
    let delta = r - l in
    if delta = 1 then Hash.hash_chunk chunks.(l)
    else
      let mid = l + (delta asr 1) in
      let lhash, rhash =
        par_executor.par_do (fun () -> f l mid) (fun () -> f mid r)
      in
      Hash.merge lhash rhash
  in
  par_executor.run (fun () -> f 0 (Array.length chunks))

let rabin_karp_incr ~mode chunks =
  Utils.reduce_arr ~mode
    Hash.{result = 0; acc = 1}
    (fun x -> Hash.hash_chunk x)
    Hash.merge chunks

let change_inputs () =
  for _ = 1 to !no_of_input_changes do
    let index = Random.int no_of_chunks in
    let chunk = chunks.(index) in
    let chunk' = random_chunk (Bytes.length chunk) () in
    chunks.(index) <- chunk';
    Var.set chunks_var.(index) chunk'
  done

let run_incr = Incr.run ~executor:par_executor

let () =
  Printf.printf
    "Rabin Karp Hash | No. of char: %d, Chunk size <= %d, Changes during \
     propagation:%d\n"
    !no_of_char chunk_size !no_of_input_changes;
  let hash_result = ref Hash.{result = 0; acc = 1} in
  let static_par =
    Bench.run ~name:"static-par-rk"
      ~f:(fun () -> rabin_karp_static_par chunks)
      ~post:(fun res -> hash_result := res)
      ()
  in
  let incr_seq_initial_cons =
    Bench.run ~name:"incr-seq-rk-initial-cons"
      ~f:(fun () -> run_incr (rabin_karp_incr ~mode:`Seq chunks_incr))
      ~post:(fun c ->
        assert (Incr.value c = !hash_result);
        Incr.destroy_comp c)
      ()
  in
  let incr_par_initial_cons =
    Bench.run ~name:"incr-par-rk-initial-cons"
      ~f:(fun () -> run_incr (rabin_karp_incr ~mode:`Par chunks_incr))
      ~post:(fun c ->
        assert (Incr.value c = !hash_result);
        Incr.destroy_comp c)
      ()
  in
  let incr_seq_comp = run_incr (rabin_karp_incr ~mode:`Seq chunks_incr) in
  let incr_seq_prop =
    Bench.run ~name:"incr-seq-rk-prop" ~pre:change_inputs
      ~f:(fun () -> Incr.propagate incr_seq_comp)
      ~post:(fun _ ->
        assert (Incr.value incr_seq_comp = rabin_karp_static_par chunks))
      ()
  in
  destroy_comp incr_seq_comp;

  let incr_par_comp = run_incr (rabin_karp_incr ~mode:`Par chunks_incr) in
  let incr_par_prop =
    Bench.run ~name:"incr-par-rk-prop" ~pre:change_inputs
      ~f:(fun () -> Incr.propagate incr_par_comp)
      ~post:(fun _ ->
        assert (Incr.value incr_par_comp = rabin_karp_static_par chunks))
      ()
  in
  destroy_comp incr_par_comp;

  Bench.report
    [
      static_par;
      incr_seq_initial_cons;
      incr_par_initial_cons;
      incr_seq_prop;
      incr_par_prop;
    ];
  T.teardown_pool pool
