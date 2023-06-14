module Incr = Par_incr

let seq_executor =
  Incr.{run = (fun f -> f ()); par_do = (fun l r -> (l (), r ()))}

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

let get_par_executor ~num_domains () =
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains () in
  let par_runner f = T.run pool f in
  let par_do l r =
    let lres = T.async pool l in
    let rres = r () in
    (T.await pool lres, rres)
  in
  (pool, Incr.{run = par_runner; par_do})

let reduce_arr ~mode (zero : 'a) (one : 'b -> 'a) (plus : 'a -> 'a -> 'a)
    (xs : 'b Incr.t array) : 'a Incr.t =
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
