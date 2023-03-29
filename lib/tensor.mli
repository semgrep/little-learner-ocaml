
type t =
  | S of float
  | T of t array

val is_scalar: t -> bool

val tlen: t -> int

val tref: t -> int -> t

val rank: t -> int

val shape: t -> int list
