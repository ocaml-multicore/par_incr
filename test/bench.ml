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
  begin
    assert (runs > 0);
    let runtimes =
      Array.init runs (fun _ ->
          pre ();
          let f_res, time_elapsed = time_fn ~f in
          let () = post f_res in
          time_elapsed)
    in
    let () = Array.sort Float.compare runtimes in
    let runtime_sum = Array.fold_left (fun acc x -> acc +. x) 0. runtimes in
    let median =
      begin
        if runs mod 2 = 0 then begin
          (runtimes.((runs + 1) / 2) +. runtimes.((runs - 1) / 2)) /. 2.0
        end
        else runtimes.((runs + 1) / 2)
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
  end

let report res =
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
  let g = Float.to_string in
  let () =
    Printf.printf "%s %s %s %s %s %s\n" (f "Name" name_padding)
      (f "Median(in s)" 18) (f "Avg(in s)" 18) (f "Runs" 8)
      (f "Max time(in s)" 18) (f "Min time(in s)" 18)
  in
  List.iter
    (fun x ->
      Printf.printf "%s %s %s %s %s %s\n"
        (f x.bench_name name_padding)
        (f (g x.median_exec_time) 18)
        (f (g x.avg_exec_time) 18)
        (f (Int.to_string x.num_of_runs) 8)
        (f (g x.longest_exec_time) 18)
        (f (g x.shortest_exec_time) 18))
    res
