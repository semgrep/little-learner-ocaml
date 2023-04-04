(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff we want to learn, the "parameters" of a target_fn (e.g., line()),
 * often called 'theta' *)
type parameters = parameter list
(* a parameter is a tensor *)
and parameter = Tensor.t
[@@deriving show]

(* e.g., line(), quad() *)
type target_fn =
  Tensor.t (* input, the "arguments" of the target_fn *) ->
  parameters (* the "parameters" *) ->
  Tensor.t (* output *)

(* The first unit argument is a hack because of the way gradient_pad work.
 * See the comment in sampling_obj() for more information.
 *)
type objective_fn =
  unit -> parameters -> Tensor.scalar (* the loss *)

(* expect datasets as arguments. An example of an expectant_fn is
 * [l2_loss line]: it returns a function that waits for its input and output
 *  datasets and then returns an objective_fn (which itself will wait for
 *  its parameters to finally return a scalar, the loss)
 *)
type expectant_fn =
  Tensor.t (* input dataset *) -> Tensor.t (* expected output dataset *) ->
  objective_fn

val debug: bool ref

type 'a accompanied_param =
  { p : parameter;
    x : 'a; (* extra info, "accompaniment" *)
  }

type 'a ate = {
    inflate: parameter -> 'a accompanied_param;
    deflate: 'a accompanied_param -> parameter;
    update: 'a accompanied_param -> parameter (* gradient *) -> 'a accompanied_param;
}

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

(* learning rate: new_theta = old_theta - alpha x g (fradient) *)
val alpha: float ref

(* revisions: revise f revs theta_init *)
val revs: int ref

(* samples size: sampling_obj input_set !batch_size *)
val batch_size : int ref

(* usage: with_hyper alpha 0.01 (fun () -> ...). They can be nested *)
val with_hyper : 'a ref -> 'a -> (unit -> 'b) -> 'b
                                                            
(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

(* sum of sqr of differences (Euclidian distance) *)
val l2_loss: target_fn -> expectant_fn

(*****************************************************************************)
(* Gradient descent v1 *)
(*****************************************************************************)

(* one step *)
val gradient_pad: objective_fn -> parameters -> parameters

(* internal *)
val revise: ('parameters -> 'parameters) -> int -> 'parameters -> 'parameters
  
(* optimization by gradient descent, continue to revise the parameters.
 * Internally relies on !alpha, !revs (and gradient_pad).
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

(*****************************************************************************)
(* Gradient descent v2 *)
(*****************************************************************************)

val gradient_descent_v2 :
  (parameter -> 'a) * ('a -> parameter) * ('a -> parameter -> 'a) ->
  objective_fn -> parameters -> parameters

(* pad's typed extension *)
val gradient_descent_v3 :
  'a ate -> objective_fn -> parameters -> parameters
