let time_fn ~f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let time_elapsed = Unix.gettimeofday () -. t in
  (res, time_elapsed)

type result = {
  bench_name : string;
  median_exec_time : float;
  avg_exec_time : float;
  num_of_runs : int;
  shortest_exec_time : float;
  longest_exec_time : float;
}

let run ?(pre = Fun.id) ?(post = ignore) ?(runs = 10) ~name ~f () =
  assert (runs > 0);
  let runtimes =
    Array.init runs (fun _ ->
        pre ();
        let f_res, time_elapsed = time_fn ~f in
        let () = post f_res in
        Gc.full_major ();
        time_elapsed)
  in
  let () = Array.sort Float.compare runtimes in
  let runtime_sum = Array.fold_left (fun acc x -> acc +. x) 0. runtimes in
  let median =
    begin
      if runs mod 2 = 0 then begin
        (runtimes.((runs + 1) / 2) +. runtimes.((runs - 1) / 2)) /. 2.0
      end
      else runtimes.(runs / 2)
    end
  in
  {
    bench_name = name;
    median_exec_time = median;
    avg_exec_time = runtime_sum /. Float.of_int runs;
    num_of_runs = runs;
    shortest_exec_time = runtimes.(0);
    longest_exec_time = runtimes.(runs - 1);
  }

let process s =
  let first_n_after_nonzero n s =
    let non_zero_at = ref (-1) in
    let () =
      String.iteri
        (fun i c -> if c <> '0' && !non_zero_at = -1 then non_zero_at := i)
        s
    in
    String.sub s 0 (min (!non_zero_at + n) (String.length s))
  in

  let l = String.split_on_char '.' s in
  match l with
  | [x] -> x
  | [before; after] -> before ^ "." ^ first_n_after_nonzero 4 after
  | _ -> failwith "Impossible"

let report ?(in' = `Ms) res =
  let name_padding =
    List.fold_left
      (fun acc x -> Int.max acc (String.length x.bench_name))
      10 res
  in
  let pad_with_space x len =
    assert (String.length x <= len);
    x ^ String.init (len - String.length x) (fun _ -> ' ')
  in
  let f = pad_with_space in
  let time_unit_s =
    match in' with `S -> "s" | `Ms -> "ms" | `Us -> "µs" | `Ns -> "ns"
  in
  let g x =
    let y =
      match in' with
      | `S -> x
      | `Ms -> x *. 1000.
      | `Us -> x *. (1000. *. 1000.)
      | `Ns -> x *. (1000. *. 1000. *. 1000.)
    in
    (* let y' = String.sub ( Float.to_string y ) *)
    process (Float.to_string y) ^ " " ^ time_unit_s
  in

  let () =
    Printf.printf "%s %s %s %s %s %s\n" (f "Name" name_padding) (f "Avg" 18)
      (f "Median" 18) (f "Runs" 8) (f "Max time" 18) (f "Min time" 18)
  in
  res
  |> List.sort (fun x y -> Float.compare x.avg_exec_time y.avg_exec_time)
  |> List.iter (fun x ->
         Printf.printf "%s %s %s %s %s %s\n"
           (f x.bench_name name_padding)
           (f (g x.avg_exec_time) 18)
           (f (g x.median_exec_time) 18)
           (f (Int.to_string x.num_of_runs) 8)
           (f (g x.longest_exec_time) 18)
           (f (g x.shortest_exec_time) 18))
