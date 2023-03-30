
(* t0 for tensor0 *)
let scalar t0 =
  match t0 with
  | S x -> x
  | T _ -> failwith "not a scalar"

let rec summed t i acc =
  if i = 0
  then S (scalar (tref t 0) +. acc)
  else
    summed t (i - 1) (scalar (tref t i) +. acc)

let sum_1 t1 =
  summed t1 (tlen t1 - 1) 0.

let res_frame_22 =
  sum_1 (T [| S 10.; S 12.; S 14.|])

(* scalar exn *)
let res_frame_22_bis_invalid =
  sum_1 (T [| T [| (S 10.)|] |])
