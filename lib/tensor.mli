
type t =
  | S of float
  | T of t array

val is_scalar: t -> bool

(* raise exn on array tensor *)
val scalar: t -> float

(* raise exn on scalar tensor *)
val tlen: t -> int

(* raise exn on scalar tensor *)
val tref: t -> int -> t

val rank: t -> int

val shape: t -> int list
