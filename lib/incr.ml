type 'a t = {
  mutable value : 'a option;
  mutable eq : 'a -> 'a -> bool;
  mutable to_string : 'a -> string;
  mutable readers : Rsp.RNode.t Reader_list.t;
}

let empty ~eq ~to_s () =
  {value = None; eq; to_string = to_s; readers = Reader_list.empty ()}

let create ~eq ~to_s x =
  {value = Some x; eq; to_string = to_s; readers = Reader_list.empty ()}

let value_exn t = Utils.deref t.value
let add_reader t r = Reader_list.add_reader t.readers r
let remove_reader t r = Reader_list.remove_reader t.readers r
let num_readers t = Reader_list.len t.readers
let eq t = t.eq
let to_s t = t |> value_exn |> t.to_string
let get_to_string t = t.to_string

let notify_readers t =
  begin
    Reader_list.for_all t.readers (fun r -> Rsp.RNode.mark_dirty r)
  end

let set t x =
  begin
    match t.value with
    | None -> t.value <- Some x
    | Some x' ->
      if not (t.eq x x') then (
        t.value <- Some x;
        notify_readers t)
  end

let attach_eq t f = if t.eq != f then t.eq <- f else ()
let attach_to_string t f = if t.to_string != f then t.to_string <- f else ()
