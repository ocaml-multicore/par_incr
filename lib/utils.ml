let dirty_flag = 4
let root_flag = 0
let s_flag = 1
let p_flag = 2
let r_flag = 3
let mask = 3

let[@inline] deref t =
  match t with Some x -> x | None -> failwith "deref on None failed"

let[@inline] combine_eq eq1 eq2 (a', b') (a'', b'') = eq1 a' a'' && eq2 b' b''
let[@inline] impossible () = failwith "Impossible case"
let undefined _ = "var"

let[@inline] combine_to_s to_s1 to_s2 =
  if to_s1 == undefined || to_s2 == undefined then undefined
  else fun (x, y) -> to_s1 x ^ " , " ^ to_s2 y

let[@inline] is_marked flag = flag >= dirty_flag
let[@inline] make_marked flag = flag lor dirty_flag
let[@inline] is_rnode flag = flag land mask = r_flag
let[@inline] is_snode flag = flag land mask = s_flag
let[@inline] is_pnode flag = flag land mask = p_flag
let[@inline] is_root flag = flag land mask = root_flag
let[@inline] masked flag = flag land mask

let[@inline] typeflag_to_string flag =
  if flag = root_flag then "Root"
  else if flag = r_flag then "R"
  else if flag = s_flag then "Seq"
  else if flag = p_flag then "Par"
  else impossible ()
