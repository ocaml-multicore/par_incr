module Incr = Par_incr

let seq_executor =
  Incr.{run = (fun f -> f ()); par_do = (fun l r -> (l (), r ()))}

let time_fn ?fn_name ~f () =
  let t = Unix.gettimeofday () in
  let res = f () in
  let d = Unix.gettimeofday () -. t in
  let () =
    match fn_name with
    | None -> Printf.printf "Execution time: %fs\n" d
    | Some name -> Printf.printf "Execution time for %s: %fs\n" name d
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

let reduce_arr ~mode ?(eq = ( == )) (zero : 'a) (one : 'b -> 'a)
    (plus : 'a -> 'a -> 'a) (xs : 'b Incr.t array) : 'a Incr.t =
  let n = Array.length xs in
  if n = 0 then Incr.return zero
  else
    let rec reduce lo hi =
      Incr.delay @@ fun () ->
      let delta = hi - lo in
      if delta = 1 then Incr.map ~eq ~fn:one xs.(lo)
      else
        let mid = lo + (delta asr 1) in
        Incr.map2 ~eq ~mode ~fn:plus (reduce lo mid) (reduce mid hi)
    in
    reduce 0 n

let reduce_lst ~mode ?(eq = ( == )) (zero : 'a) (one : 'b -> 'a)
    (plus : 'a -> 'a -> 'a) (l : 'b Incr.t list) : 'a Incr.t =
  (* Code highly inspired from Base.List.reduced_balanced. *)
  let rec step_accum' num acc x =
    if num land 1 = 0 then x :: acc
      (* (Incr.Debug.attach ~fn:to_s @@ Incr.map ~eq ~fn:Fun.id x) :: acc *)
    else
      match acc with
      | [] -> assert false
      | y :: ys ->
        step_accum' (num asr 1) ys
          (Incr.delay @@ fun () -> Incr.map2 ~mode ~eq ~fn:plus y x)
  in
  let step_accum num acc x =
    if num land 1 = 0 then
      (Incr.delay @@ fun () -> Incr.map ~eq ~fn:one x) :: acc
    else
      match acc with
      | [] -> assert false
      (* New elements from later in the input list go on the front of the
         accumulator, so the accumulator is in reverse order wrt the original
         list order, hence [f y x] instead of [f x y]. *)
      | y :: ys ->
        step_accum' (num asr 1) ys
          ( Incr.delay @@ fun () ->
            Incr.map2 ~eq ~mode ~fn:plus y (Incr.map ~eq ~fn:one x) )
  in
  let foldi (l : 'b Incr.t list) ~(init : 'a Incr.t list)
      ~(f : int -> 'a Incr.t list -> 'b Incr.t -> 'a Incr.t list) =
    let v, _ =
      List.fold_left (fun (acc, i) x -> (f i acc x, i + 1)) (init, 0) l
    in
    v
  in
  let res : 'a Incr.t list = foldi l ~init:[] ~f:step_accum in
  List.fold_left
    (fun x y -> Incr.delay @@ fun () -> Incr.map2 ~eq ~fn:plus y x)
    (Incr.return zero) res
