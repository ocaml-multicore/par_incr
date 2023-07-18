type result = {
  bench_name : string;
  median_exec_time : float;
  avg_exec_time : float;
  num_of_runs : int;
  shortest_exec_time : float;
  longest_exec_time : float;
}

val run :
  ?pre:(unit -> unit) ->
  ?post:('a -> unit) ->
  ?runs:int ->
  name:string ->
  f:(unit -> 'a) ->
  unit ->
  result

val report : ?in':[< `S | `Ms | `Us | `Ns > `Ms] -> result list -> unit
