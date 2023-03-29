type t =
  (* scalar *)
  | S of float
  (* tensor *)
  | T of t array

let is_scalar = function
  | S _ -> true
  | T _ -> false

val scalar = function
  | S x -> x
  | T _ -> failwith "scalar: not a scalar"

let tlen ts =
  match ts with
  | S _ -> failwith "tlen: scalar tensor"
  | T xs -> Array.length xs

let tref t n =
  match t with
  | S _ -> failwith "tref: scalar tensor"
  | T ts -> ts.(n)

(* not tail-rec *)
(*
let rec rank t =
  match t with
  | S _ -> 0
  | T _ts ->
     1 + rank (tref t 0)
 *)

let rec shape t =
  match t with
  | S _ -> []
  | T _ ->
     tlen t :: shape (tref t 0)

(* tail-rec *)
let rec ranked t acc =
  match t with
  | S _ -> acc
  | T _ ->
     ranked (tref t 0) (1 + acc)

let rank t =
  ranked t 0
