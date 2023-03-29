type tensor =
  (* scalar *)
  | S of float
  (* tensor *)
  | T of tensor array

let is_scalar = function
  | S _ -> true
  | T _ -> false

let tlen ts =
  match ts with
  | S _ -> failwith "tlen: scalar tensor"
  | T xs -> Array.length xs

let tref t n =
  match t with
  | S _ -> failwith "tref: scalar tensor"
  | T ts -> ts.(n)

let rec rank t =
  match t with
  | S _ -> 0
  | T ts ->
     1 + rank (tref t 0)

let ex_frame_26 =
  T [| T [| T [|S 8.|]; T [|S 9.|] |];
       T [| T [|S 4.|]; T [|S 7.|] |];
    |]

let res_frame_26 =
  rank ex_frame_26

let rec shape t =
  match t with
  | S _ -> []
  | T _ ->
     tlen t :: shape (tref t 0)

let ex_frame_41 =
  T [|
      T [| T [|S 5.0|]; T [|S 6.0|]; T [|S 8.0|]|];
      T [| T [|S 7.0|]; T [|S 9.0|]; T [|S 4.0|]|];
    |]
let res_frame_41 =
  shape ex_frame_41


(* tail-rec *)
let rec ranked t acc =
  match t with
  | S _ -> acc
  | T _ ->
     ranked (tref t 0) (1 + acc)

let rank t =
  ranked t 0

let res_frame_44 =
  rank ex_frame_26
