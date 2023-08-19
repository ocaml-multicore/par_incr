module Incr = Par_incr

module M (M' : Incremental.S) : sig
  type ('a, 'b, 'c, 'd) incr_type =
    | Js_incr : ('a, 'b, 'a M'.t, 'b M'.t) incr_type
    | Current_incr : ('a, 'b, 'a Current_incr.t, 'b Current_incr.t) incr_type

  val reduce_arr :
    'a 'b 'c 'd.
    incr_type:('a, 'b, 'c, 'd) incr_type ->
    ?eq:('b -> 'b -> bool) ->
    'b ->
    ('a -> 'b) ->
    ('b -> 'b -> 'b) ->
    'c array ->
    'd

  val reduce_lst :
    'a 'b 'c 'd.
    incr_type:('a, 'b, 'c, 'd) incr_type ->
    ?eq:('b -> 'b -> bool) ->
    'b ->
    ('a -> 'b) ->
    ('b -> 'b -> 'b) ->
    'c list ->
    'd
end = struct
  type ('a, 'b, 'c, 'd) incr_type =
    | Js_incr : ('a, 'b, 'a M'.t, 'b M'.t) incr_type
    | Current_incr : ('a, 'b, 'a Current_incr.t, 'b Current_incr.t) incr_type

  let rec reduce :
      type a b c d.
      incr_type:(a, b, c, d) incr_type ->
      ?eq:(b -> b -> bool) ->
      (a -> b) ->
      (b -> b -> b) ->
      c array ->
      int ->
      int ->
      d =
   fun ~incr_type ?eq one plus xs lo hi ->
    let delta = hi - lo in
    if delta = 1 then
      match incr_type with
      | Current_incr -> Current_incr.map ?eq one xs.(lo)
      | Js_incr -> M'.map ~f:one xs.(lo)
    else
      let mid = lo + (delta asr 1) in
      match incr_type with
      | Current_incr ->
        let open Current_incr in
        let x = reduce ~incr_type ?eq one plus xs lo mid in
        let y = reduce ~incr_type ?eq one plus xs mid hi in
        of_cc @@ read x (fun x -> read y (fun y -> write (plus x y)))
      | Js_incr ->
        M'.map2
          (reduce ~incr_type ?eq one plus xs lo mid)
          (reduce ~incr_type ?eq one plus xs mid hi)
          ~f:plus

  let reduce_arr :
      type a b c d.
      incr_type:(a, b, c, d) incr_type ->
      ?eq:(b -> b -> bool) ->
      b ->
      (a -> b) ->
      (b -> b -> b) ->
      c array ->
      d =
   fun ~incr_type ?eq zero one plus xs ->
    let n = Array.length xs in
    if n = 0 then
      match incr_type with
      | Current_incr -> Current_incr.const zero
      | Js_incr -> M'.return zero
    else reduce ~incr_type ?eq one plus xs 0 n

  let rec step_accum' :
      type a b c d.
      incr_type:(a, b, c, d) incr_type ->
      ?eq:(b -> b -> bool) ->
      (b -> b -> b) ->
      int ->
      d list ->
      d ->
      d list =
   fun ~incr_type ?eq plus num acc x ->
    if num land 1 = 0 then x :: acc
    else
      match acc with
      | [] -> assert false
      | y :: ys -> (
        match incr_type with
        | Current_incr ->
          step_accum' ~incr_type ?eq plus (num asr 1) ys
            (Current_incr.of_cc
            @@ Current_incr.read y (fun y ->
                   Current_incr.read x (fun x ->
                       Current_incr.write ?eq (plus y x))))
        | Js_incr ->
          step_accum' ~incr_type ?eq plus (num asr 1) ys (M'.map2 ~f:plus y x))

  let step_accum :
      type a b c d.
      incr_type:(a, b, c, d) incr_type ->
      ?eq:(b -> b -> bool) ->
      (a -> b) ->
      (b -> b -> b) ->
      int ->
      d list ->
      c ->
      d list =
   fun ~incr_type ?eq one plus num acc x ->
    if num land 1 = 0 then
      match incr_type with
      | Current_incr -> Current_incr.map ?eq one x :: acc
      | Js_incr -> M'.map x ~f:one :: acc
    else
      match acc with
      | [] -> assert false
      (* New elements from later in the input list go on the front of the
         accumulator, so the accumulator is in reverse order wrt the original
         list order, hence [f y x] instead of [f x y]. *)
      | y :: ys -> (
        match incr_type with
        | Current_incr ->
          let x = Current_incr.map ?eq one x in
          step_accum' ~incr_type ?eq plus (num asr 1) ys
            (Current_incr.of_cc
            @@ Current_incr.read y (fun y ->
                   Current_incr.read x (fun x ->
                       Current_incr.write ?eq (plus y x))))
        | Js_incr ->
          step_accum' ~incr_type ?eq plus (num asr 1) ys
            (M'.map2 ~f:(fun x y -> plus y x) (M'.map ~f:one x) y))

  let reduce_lst :
      type a b c d.
      incr_type:(a, b, c, d) incr_type ->
      ?eq:(b -> b -> bool) ->
      b ->
      (a -> b) ->
      (b -> b -> b) ->
      c list ->
      d =
   fun ~incr_type ?eq zero one plus l ->
    (* Code highly inspired from Base.List.reduced_balanced. *)
    let foldi l ~init ~f =
      let v, _ =
        List.fold_left (fun (acc, i) x -> (f i acc x, i + 1)) (init, 0) l
      in
      v
    in
    let res = foldi l ~init:[] ~f:(step_accum ~incr_type ?eq one plus) in
    match incr_type with
    | Current_incr ->
      List.fold_left
        (fun x y ->
          Current_incr.of_cc
          @@ Current_incr.read y (fun y ->
                 Current_incr.read x (fun x ->
                     Current_incr.write ?eq (plus y x))))
        (Current_incr.const zero) res
    | Js_incr ->
      List.fold_left (fun x y -> M'.map2 ~f:plus y x) (M'.return zero) res
end

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

let reduce_arr ?mode ?cutoff (zero : 'a) (one : 'b -> 'a)
    (plus : 'a -> 'a -> 'a) (xs : 'b Incr.t array) : 'a Incr.t =
  let n = Array.length xs in
  if n = 0 then Incr.return zero
  else
    let rec reduce lo hi =
      Incr.delay @@ fun () ->
      let delta = hi - lo in
      if delta = 1 then Incr.map ?cutoff ~fn:one xs.(lo)
      else
        let mid = lo + (delta asr 1) in
        Incr.map2 ?cutoff ?mode ~fn:plus (reduce lo mid) (reduce mid hi)
    in
    reduce 0 n

let reduce_lst ?mode ?cutoff (zero : 'a) (one : 'b -> 'a)
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
          (Incr.delay @@ fun () -> Incr.map2 ?mode ?cutoff ~fn:plus y x)
  in
  let step_accum num acc x =
    if num land 1 = 0 then
      (Incr.delay @@ fun () -> Incr.map ?cutoff ~fn:one x) :: acc
    else
      match acc with
      | [] -> assert false
      (* New elements from later in the input list go on the front of the
         accumulator, so the accumulator is in reverse order wrt the original
         list order, hence [f y x] instead of [f x y]. *)
      | y :: ys ->
        step_accum' (num asr 1) ys
          ( Incr.delay @@ fun () ->
            Incr.map2 ?cutoff ?mode ~fn:plus y (Incr.map ?cutoff ~fn:one x) )
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
    (fun x y -> Incr.delay @@ fun () -> Incr.map2 ?cutoff ~fn:plus y x)
    (Incr.return zero) res
