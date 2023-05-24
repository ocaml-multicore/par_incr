open Par_incr

module Incr_list = struct
  type 'a t = 'a Par_incr.t list

  let reduce_exn (l : 'a Par_incr.t list) ~(fn : 'a -> 'a -> 'a) =
    (*Code snippet from Base.List.reduced_balanced*)
    let rec step_accum num acc x =
      if num land 1 = 0 then x :: acc
      else
        match acc with
        | [] -> assert false
        (* New elements from later in the input list go on the front of the accumulator, so
           the accumulator is in reverse order wrt the original list order, hence [f y x]
           instead of [f x y]. *)
        | y :: ys -> step_accum (num asr 1) ys (map2 ~fn y x)
    in
    let foldi l ~init ~f =
      let v, _ =
        List.fold_left (fun (acc, i) x -> (f i acc x, i + 1)) (init, 0) l
      in
      v
    in
    match foldi l ~init:[] ~f:step_accum with
    | [] -> failwith "Empty list"
    | x :: xs -> List.fold_left (fun x y -> map2 ~fn y x) x xs
end

module Incr_array = struct
  type 'a t = 'a Par_incr.t array

  let rec reduce_range_exn ~(fn : 'a -> 'a -> 'a) ~lo ~hi (l : 'a t) =
    delay @@ fun () ->
    if lo = hi - 1 then l.(lo)
    else
      let mid = lo + ((hi - lo) asr 1) in
      let open Syntax in
      let+ lhalf = reduce_range_exn l ~fn ~lo ~hi:mid
      and+ rhalf = reduce_range_exn l ~fn ~lo:mid ~hi in
      fn lhalf rhalf

  let rec reduce_range_par_exn ?(gran = 32) ~(fn : 'a -> 'a -> 'a) ~lo ~hi
      (l : 'a t) =
    delay @@ fun () ->
    if lo - hi <= gran then reduce_range_exn l ~fn ~lo ~hi
    else
      let mid = lo + ((hi - lo) asr 1) in
      let open Syntax in
      let& lhalf = reduce_range_par_exn l ~fn ~lo ~hi:mid
      and& rhalf = reduce_range_par_exn l ~fn ~lo:mid ~hi in
      fn lhalf rhalf
end
