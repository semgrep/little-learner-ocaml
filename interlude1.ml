
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

(* with code from interlude5.ml really *)
open Tensor

let ex_frame_6 =
  (T [| S 2.|]) + (T [| S 7.|])

(* APL/J/K !! *)
let res_frame_12 =
  (S 4.) + (T [| S 3.; S 6.; S 5.|])

let res_frame_17 =
  sqrt (T [| S 9.; S 16.; S 25.|])

let res_frame_26 =
  sum
    (T [|
         T [|
             T [| S 1.; S 2.|];
             T [| S 3.; S 4.|];
           |];
         T [|
             T [| S 5.; S 6.|];
             T [| S 7.; S 8.|];
           |];
       |])
             
(* from chap1.ml, but with extended operators *)

let lref xs n =
  List.nth xs n

let line = fun x -> fun theta ->
  x * (lref theta 0) + (lref theta 1)

let res_frame_29 =
  line (T [| S 2.; S 7.; S 5.; S 11.|]) [S 4.; S 6.]
