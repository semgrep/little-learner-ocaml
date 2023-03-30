open Tensor
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff to learn, the "parameters" of the target_fn (e.g., line()), theta *)
type parameters = t list

(* e.g., line() *)
type target_fn =
  t (* input, the "arguments" of the target_fn *) -> parameters -> t (* output *)

type objective_fn =
  parameters -> scalar (* the loss *)

(* expect datasets as arguments *)
type expectant_fn =
  t (* input dataset *) -> t (* predicted output dataset *) ->
  objective_fn

(*****************************************************************************)
(* Hyperparameters *)
(*****************************************************************************)

(* learning rate *)
val alpha: float ref

(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

val l2_loss: target_fn -> expectant_fn
