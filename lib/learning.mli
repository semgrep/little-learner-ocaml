(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff we want to learn, the "parameters" of a target_fn (e.g., line()),
 * often called 'theta' *)
type parameters = Tensor.t list
[@@deriving show]

(* e.g., line(), quad() *)
type target_fn =
  Tensor.t (* input, the "arguments" of the target_fn *) ->
  parameters (* the "parameters" *) ->
  Tensor.t (* output *)

type objective_fn =
  parameters -> Tensor.scalar (* the loss *)

(* expect datasets as arguments. An example of an expectant_fn is
 * (l2_loss target): it returns a function that waits for its input and output
 *  datasets and then return an objective_fn (which itself will wait for
 *  its parameters to finally return a scalar, the loss)
 *)
type expectant_fn =
  Tensor.t (* input dataset *) -> Tensor.t (* predicted output dataset *) ->
  objective_fn

val debug: bool ref

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

(* samples size *)
val batch_size : int ref

val with_hyper : 'a ref -> 'a -> (unit -> 'b) -> 'b
                                                            
(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

(* sum of sqr of differences (euclidian distance) *)
val l2_loss: target_fn -> expectant_fn

(*****************************************************************************)
(* Gradient descent v1 *)
(*****************************************************************************)

(* one step *)
val gradient_pad: objective_fn -> parameters -> parameters

(* internal *)
val revise : (parameters -> parameters) -> int -> parameters -> parameters
  
(* optimization by gradient descent, continue to revise the parameters.
 * Internally rely on !alpha, !revs (and gradient_pad).
 * ex: gradient_descent ((l2_loss line) line_xs line_ys) [S 0.; S 0.]
 *)
val gradient_descent_v1 : objective_fn -> parameters -> parameters

(*****************************************************************************)
(* Stochastic gradient descent *)
(*****************************************************************************)

(* initialize the random number generator with Random.self_init() *)
val init: unit -> unit

(* internal *)
val samples: int -> int -> int list

val sampling_obj : expectant_fn -> Tensor.t -> Tensor.t -> objective_fn
