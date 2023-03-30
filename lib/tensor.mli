
type t =
  | S of float
  | T of t array

type t0 = t (* tensor^0 *)
type t1 = t (* tensor^1 *)
type t2 = t (* tensor^2 *)

val is_scalar: t -> bool

(* raise exn on array tensor *)
val scalar: t0 -> float

(* raise exn on scalar tensor *)
val tlen: t -> int

(* raise exn on scalar tensor *)
val tref: t -> int -> t

val rank: t -> int

val shape: t -> int list
