
let scalar t =
  match t with
  | S x -> x
  | T _ -> failwith "not a scalar"

let rec summed t i acc =
  if i = 0
  then scalar (tref t 0) +. acc
  else
    summed t (i - 1) (scalar (tref t i) +. acc)

let sum_1 t =
  summed t (tlen t - 1) 0.

