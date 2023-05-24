module Incr = Par_incr
open Incr
module T = Domainslib.Task

(* let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count () / 2) () *)
(* let par_runner f = T.run pool f *)
(* let async f = T.async pool f *)
(* let await fut = T.await pool fut *)

(* let par_do l r = *)
(*   let lres = async l in *)
(*   let rres = r () in *)
(*   (await lres, rres) *)

let par_runner f = f () [@@inline]

let par_do l r =
  let lres = l () in
  let rres = r () in
  (lres, rres)
  [@@inline]

let print_stats (stat : Par_incr.comp_stat) =
  Printf.printf
    "map:%d, combine:%d, bind:%d, par:%d, rnode:%d, snode:%d, pnode:%d, \
     dummy:%d, dirty:%d\n"
    stat.map stat.combine stat.bind stat.par_do stat.r stat.s stat.p stat.dummy
    stat.dirty

let time_fn ?fn_name ~f () =
  let t = Unix.gettimeofday () in
  let res = f () in
  let () =
    match fn_name with
    | None -> Printf.printf "Execution time: %fs\n" (Unix.gettimeofday () -. t)
    | Some name ->
      Printf.printf "Execution time for %s: %fs\n" name
        (Unix.gettimeofday () -. t)
  in
  res

let rec sum_range ~lo ~hi xs =
  Incr.delay @@ fun () ->
  if hi - lo <= 1 then begin
    xs.(lo)
  end
  else
    let mid = lo + ((hi - lo) asr 1) in
    Incr.Syntax.(
      let+ lhalf = sum_range ~lo ~hi:mid xs
      and+ rhalf = sum_range ~lo:mid ~hi xs in
      lhalf + rhalf)

let rec sum_range_par ~lo ~hi xs =
  Incr.delay @@ fun () ->
  if hi - lo <= 32 then begin
    sum_range ~lo ~hi xs
  end
  else
    let mid = lo + ((hi - lo) asr 1) in
    Incr.Syntax.(
      let& lhalf = sum_range_par ~lo ~hi:mid xs
      and& rhalf = sum_range_par ~lo:mid ~hi xs in
      lhalf + rhalf)

let _add (v : int Var.t) (x : int) =
  let open Incr.Var.Syntax in
  v := !v + x

let _array_sum_dnc n () =
  begin
    let xs =
      Array.init n (fun i -> Var.create ~eq:Int.equal ~to_s:Int.to_string i)
    in
    let xs' = Array.map Var.watch xs in
    let _sum xs = sum_range ~lo:0 ~hi:(Array.length xs) xs in
    let _sum_par xs = sum_range_par ~lo:0 ~hi:(Array.length xs) xs in
    let _result_seq () =
      time_fn
        ~f:(fun () ->
          Incr.run
            ~executor:
              {run = (fun f -> f ()); par_do = (fun l r -> (l (), r ()))}
            (Incr.map
               ~fn:(fun x ->
                 Printf.printf "Array Sum Result(seq): %d\t" x;
                 x)
               ~eq:Int.equal (_sum xs')))
        ()
    in
    let _result_par () =
      time_fn
        ~f:(fun () ->
          Incr.run ~executor:{run = par_runner; par_do}
            (Incr.map
               ~fn:(fun x ->
                 Printf.printf "Array Sum Result(par): %d\t" x;
                 x)
               ~eq:Int.equal (_sum_par xs')))
        ()
    in
    let tpar = _result_par () in
    let tseq = _result_seq () in

    Array.iteri (fun i x -> if i == 1000 then _add x (Var.value x)) xs;
    time_fn ~f:(fun () -> Incr.propagate tpar) ();
    time_fn ~f:(fun () -> Incr.propagate tseq) ();
    assert (value tseq = value tpar);

    (* Array.iter (fun x -> assert (Var.num_readers x = 2)) xs; *)
    destroy_comp tpar;
    (* Array.iter (fun x -> assert (Var.num_readers x = 1)) xs; *)
    destroy_comp tseq
    (* Array.iter (fun x -> assert (Var.num_readers x = 0)) xs *)
  end

let _list_sum n () =
  let open Examples.Operations.Incr_list in
  let lst = List.init n (fun i -> Var.create i) in
  let lst' = List.map (fun x -> Var.watch x) lst in
  let sum = reduce_exn lst' ~fn:( + ) in
  let sum_comp =
    time_fn ~fn_name:"sum(seq)"
      ~f:(fun () -> run ~executor:{run = par_runner; par_do} sum)
      ()
  in
  Printf.printf "Value(seq): %d\n" (value sum_comp);
  let () = List.iter (fun v -> Var.set v (2 * Var.value v)) lst in
  let () =
    time_fn ~fn_name:"propagate_sum(seq)" ~f:(fun () -> propagate sum_comp) ()
  in
  ()

let _array_sumv2 n () =
  begin
    let open Examples.Operations.Incr_array in
    let xs =
      Array.init n (fun i -> Var.create ~eq:Int.equal ~to_s:Int.to_string i)
    in
    let xs' = Array.map Var.watch xs in
    let sum = reduce_range_exn ~fn:Int.add ~lo:0 ~hi:(Array.length xs') xs' in
    let sum_par =
      reduce_range_par_exn ~fn:Int.add ~lo:0 ~hi:(Array.length xs') xs'
    in
    let seq =
      time_fn ~fn_name:"ArraySum(seq)"
        ~f:(fun () -> run ~executor:{run = par_runner; par_do} sum)
        ()
    in
    let par =
      time_fn ~fn_name:"ArraySum(par)"
        ~f:(fun () -> run ~executor:{run = par_runner; par_do} sum_par)
        ()
    in
    assert (value seq = value par);
    Array.iter (fun x -> _add x (Var.value x)) xs;
    let () =
      time_fn ~fn_name:"propagate_sum(seq)" ~f:(fun () -> propagate seq) ()
    in
    let () =
      time_fn ~fn_name:"propagate_sum(par)" ~f:(fun () -> propagate par) ()
    in
    assert (value seq = value par);
    ()
  end

(* let () = _array_sum_dnc 1000000 () *)

module Var = Incr.Var
open Incr.Syntax

let seq_executor =
  Incr.{run = (fun f -> f ()); par_do = (fun l r -> (l (), r ()))}

let return = Incr.return
let map fn xs = Incr.map xs ~fn
let bind fn xs = Incr.bind ~fn xs

let rec to_var_list xs =
  Var.create (match xs with [] -> `Nil | x :: xs -> `Cons (x, to_var_list xs))

let rec to_incr_list xs =
  Var.watch xs
  |> map @@ function `Nil -> `Nil | `Cons (x, xs) -> `Cons (x, to_incr_list xs)

let rec filter predicate xs =
  xs
  |> bind @@ function
     | `Nil -> return `Nil
     | `Cons (x, xs) ->
       let xs = filter predicate xs in
       if predicate x then return @@ `Cons (x, xs) else xs

let qsort xs =
  let rec qs xs nil =
    xs
    |> bind @@ function
       | `Nil -> return nil
       | `Cons (x, xs) ->
         let gt = `Cons (x, qs (filter (fun y -> x <= y) xs) nil) in
         qs (filter (fun y -> y < x) xs) gt
  in
  qs xs `Nil

let rec to_list xs =
  xs
  |> bind @@ function
     | `Nil -> return []
     | `Cons (x, xs) ->
       let+ xs = to_list xs in
       x :: xs

(* *)

let rec pb t v =
  match Var.value t with
  | `Nil -> (* failwith "Impossible" *) Var.set t (`Cons (v, Var.create `Nil))
  | `Cons (_, xs) ->
    (* ( *)
    (* match Var.value xs with `Nil -> Var.set t (`Cons (v, xs)) | _ -> pb xs v) *)
    pb xs v

let () =
  begin
    let n = Sys.argv.(1) |> int_of_string in
    Printf.printf "For n: %d\n" n;

    let lim = 100 in
    let dummy_list = List.init n (fun _ -> Random.int lim) in
    (* List.iter (fun x -> Printf.printf "%d " x) dummy_list; *)
    (* Printf.printf "\n"; *)
    let expected = List.sort Int.compare dummy_list in
    let xs = to_var_list dummy_list in
    (* let ys = to_list (qsort (to_incr_list xs)) in *)
    let ys = to_list (qsort (to_incr_list xs)) in
    let c =
      time_fn ~fn_name:"qsort"
        ~f:(fun () -> Incr.run ~executor:seq_executor ys)
        ()
    in
    assert (Incr.value c = expected);
    let stat = get_stat c in
    print_stats stat;
    pb xs lim;
    print_stats (get_stat c);
    time_fn ~fn_name:"propagation" ~f:(fun () -> propagate c) ();
    let expected = lim :: dummy_list |> List.sort Int.compare in
    assert (Incr.value c = expected);
    destroy_comp c
  end
