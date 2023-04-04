(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t =
  (* scalar *)
  | S of float
  (* tensor array *)
  | T of t array
[@@deriving show]

type t0 = t (* tensor^0 *)
type t1 = t (* tensor^1 *)
type t2 = t (* tensor^2 *)

type scalar = t0

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val is_scalar: t -> bool

(* raise exn on array tensor *)
val scalar: t0 -> float

(* raise exn on scalar tensor *)
val tlen: t -> int

(* raise exn on scalar tensor *)
val tref: t -> int -> t

val trefs: t -> int list -> t

val rank: t -> int

val shape: t -> int list

(*****************************************************************************)
(* Extended operators  *)
(*****************************************************************************)

val ( + ): t -> t -> t

val ( - ): t -> t -> t

val ( * ): t -> t -> t

val ( / ): t -> t -> t

val dotproduct: t -> t -> t

val sqrt: t -> t

(* useful for loss function *)
val sqr: t -> t

val sum: t -> scalar

(* returns a tensor with 0 everywhere of the same shape than t.
 * Useful for velocity gradient.
 *)
val zeroes: t -> t
