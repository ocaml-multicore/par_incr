let deref t =
  match t with Some x -> x | None -> failwith "deref on None failed"

let combine_eq eq1 eq2 (a', b') (a'', b'') = eq1 a' a'' && eq2 b' b''
let impossible () = failwith "Impossible case"
let undefined _ = "var"

let combine_to_s to_s1 to_s2 =
  if to_s1 == undefined || to_s2 == undefined then undefined
  else fun (x, y) -> to_s1 x ^ " , " ^ to_s2 y
