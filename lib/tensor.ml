(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type t =
  (* scalar *)
  | S of float
  (* tensor *)
  | T of t array

(* no dependent types in OCaml :( so have to use those aliases as a hack *)

type t0 = t (* tensor^0 *)
type t1 = t (* tensor^1 *)
type t2 = t (* tensor^2 *)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* scheme: was called 'scalar?' *)
let is_scalar = function
  | S _ -> true
  | T _ -> false

(* new: *)
let scalar = function
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

(*****************************************************************************)
(* Shape/rank *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Function Extensions *)
(*****************************************************************************)

let tmap f t =
  match t with
  | S _x -> f t
  | T ts ->
     (* bugfix: if you do 'T (Array.map (tmap f) ts)'
      * then function like 'let sum = ext1 sum_1 1' would loop forever.
      *)
     T (Array.map f ts)

let tmap2 g t u =
  match t, u with
  | S _x, S _y -> g t u
  | T xs, T ys ->
     T (Array.map2 g xs ys)
  | _ -> failwith "tmap2: tensor have different shapes"

(* scheme: was called of_rank? *)
let rec is_rank n t =
  match t with
  | _ when n = 0 -> is_scalar t
  | S _ -> false
  | T _ -> is_rank (n - 1) (tref t 0)

let rec ext1 f n = fun t ->
  if is_rank n t
  then f t
  else tmap (ext1 f n) t

let rec rank_gt t u =
  match t, u with
  | S _, _ -> false
  | _, S _ -> true
  | _else_ -> rank_gt (tref t 0) (tref u 0)

(* scheme: was called of_ranks? *)
let is_ranks n t m u =
  if is_rank n t
  then is_rank m u
  else false

let desc_t g t u =
  tmap (fun et -> g et u) t
let desc_u g t u =
  tmap (fun eu -> g t eu) u

let desc g n t m u =
  match () with
  | _ when is_rank n t -> desc_u g t u
  | _ when is_rank m u -> desc_t g t u
  | _ when tlen t = tlen u -> tmap2 g t u
  | _ when rank_gt t u -> desc_t g t u
  | _else_ -> desc_u g t u

let rec ext2 f n m = fun t u ->
  if is_ranks n t m u
  then f t u
  else
    desc (ext2 f n m) n t m u 

(*****************************************************************************)
(* Operators (t0/t1/t2 versions) *)
(*****************************************************************************)
let plus0 t u =
  match t, u with
  | S x, S y -> S (x +. y)
  | _ -> failwith "plus0: one of the argument is not a tensor0"

let mult0 t u =
  match t, u with
  | S x, S y -> S (x *. y)
  | _ -> failwith "mult0: one of the argument is not a tensor0"

let sqrt0 = function
  | S f -> S (Float.sqrt f)
  | T _ -> failwith "sqrt0: not a scalar"

let rec summed t i acc =
  if i = 0
  then S (scalar (tref t 0) +. acc)
  else
    summed t (i - 1) (scalar (tref t i) +. acc)

let sum_1 t1 =
  summed t1 (tlen t1 - 1) 0.

(* Base.Array.concat_map is the faster. See 
https://stackoverflow.com/questions/34752023/what-is-the-fastest-way-to-flatten-an-array-of-arrays-in-ocaml
 *)
let flatten_2 t2 =
  match t2 with
  | S _ -> failwith "flatten_2: not a tensor2"
  | T ts ->
     T (Base.Array.concat_map ~f:(function
            | S _ -> failwith "flatten_2: not a tensor2"
            | T ts -> ts)                      
             ts)

(*****************************************************************************)
(* Extended operators  *)
(*****************************************************************************)

let _zeroes =
  ext1 (fun _ -> S 0.) 0

let _sum =
  ext1 sum_1 1

let _sqrt =
  ext1 sqrt0 0

let _flatten =
  ext1 flatten_2 2

let (+) =
  ext2 plus0 0 0

let ( * ) =
  ext2 mult0 0 0

let _sqr t =
  t * t

let dotproduct_1_1 w t =
  sum_1 (w * t)

let dotproduct =
  ext2 dotproduct_1_1 1 1

let _mult_2_1 =
  ext2 ( * ) 2 1
