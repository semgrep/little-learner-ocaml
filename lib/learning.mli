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
(* Target functions *)
(*****************************************************************************)
(* f(x) = wx + b *)
val line: target_fn
(* f(x) = ax^2 + bx + c *)
val quad: target_fn
(* f(x) = a o x + b (o = dotproduct) *)
val plane: target_fn

(*****************************************************************************)
(* Hyperparameters *)
(*****************************************************************************)

(* learning rate *)
val alpha: float ref

(* revisions *)
val revs: int ref

val with_hyper : 'a ref -> 'a -> (unit -> 'b) -> 'b
                                                            
(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

val l2_loss: target_fn -> expectant_fn

(*****************************************************************************)
(* Gradient descent v1 *)
(*****************************************************************************)

(* one step *)
val gradient_pad: objective_fn -> parameters -> parameters

(* internal *)
val revise : (parameters -> parameters) -> int -> parameters -> parameters
  
(* optimization by gradient descent, continue to revise the parameters.
 * Internally rely on !alpha, !revs (and gradient_pad)
 *)
val gradient_descent_v1 : objective_fn -> parameters -> parameters
