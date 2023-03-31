(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff to learn, the "parameters" of the target_fn (e.g., line()),
 * often called 'theta' *)
type parameters = Tensor.t list

(* e.g., line() *)
type target_fn =
  Tensor.t (* input, the "arguments" of the target_fn *) -> parameters ->
  Tensor.t (* output *)

type objective_fn =
  parameters -> Tensor.scalar (* the loss *)

(* expect datasets as arguments *)
type expectant_fn =
  Tensor.t (* input dataset *) -> Tensor.t (* predicted output dataset *) ->
  objective_fn

(*****************************************************************************)
(* Hyperparameters *)
(*****************************************************************************)

(* learning rate *)
val alpha: float ref

(* revisions *)
val revs: int ref

(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

val l2_loss: target_fn -> expectant_fn

(*****************************************************************************)
(* Gradient descent *)
(*****************************************************************************)

(* one step *)
val gradient_pad: objective_fn -> parameters -> parameters

(* internal *)
val revise : (parameters -> parameters) -> int -> parameters -> parameters
  
(* optimization by gradient descent, continue to revise the parameters.
 * Internally rely on !alpha, !revs (and gradient_pad)
 *)
val gradient_descent : objective_fn -> parameters -> parameters
