let rec lookup key dict=
  match dict with
  | [] -> None
  | (k,v)::t -> if k=key then Some(v) else lookup key t
