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
  parameters (* the "parameters" to learn *) ->
  Tensor.t (* output *)

(* The first unit argument is a hack because of the way gradient_pad works.
 * See the comment in sampling_obj() for more information.
 *)
type objective_fn =
  unit -> parameters -> Tensor.scalar (* the loss, to minimize as we learn *)

(* expect datasets as arguments. An example of an expectant_fn is
 * [l2_loss line]: it returns a function that waits for its input and output
 *  datasets and then returns an objective_fn (which itself will wait for
 *  its parameters to finally return a scalar, the loss)
 *)
type expectant_fn =
  Tensor.t (* input dataset *) -> Tensor.t (* expected output dataset *) ->
  objective_fn

val debug: bool ref

(* for tweaking the general gradient descent algorithm *)
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

(* See also Nn.neuron with relu() for another example of target function *)

(*****************************************************************************)
(* Hyperparameters *)
(*****************************************************************************)

(* learning rate: new_theta = old_theta - alpha x g (gradient),
 * usually between 0.01 and 0.001
 *)
val alpha: float ref

(* revisions: revise f revs theta_init,
 * usually more than 1000.
 *)
val revs: int ref

(* samples size: sampling_obj input_set !batch_size,
 * usually around 4.
 *)
val batch_size : int ref

(* boosting: for velocity/momentum gradient descent,
 * usually around 0.9
 *)
val mu: float ref

(* smooth decay, for rms and adam gradient descent, between 0.0 and 1.0,
 * usually around 0.9 *)
val beta: float ref

(* usage: with_hyper alpha 0.01 (fun () -> ...). They can be nested *)
val with_hyper : 'a ref -> 'a -> (unit -> 'b) -> 'b

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)
val lref : parameters -> int -> parameter

(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

(* Sum of sqr of differences (Euclidian distance).
 * usage: let obj_fn = (l2_loss line) line_xs line_ys in ...
 *)
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

(* Small wrapper usually around l2_loss to speedup things.
 * Usage: let obj = sampling_obj (l2_loss plane) plane_xs plane_ys in ...
 * Internally relies on !batch_size
 *)
val sampling_obj : expectant_fn -> Tensor.t -> Tensor.t -> objective_fn

(*****************************************************************************)
(* Gradient descent v2 and v3 (general algorithm) *)
(*****************************************************************************)

(* useful for the chapX.ml, but you should use _v3 *)
val gradient_descent_v2 :
  (parameter -> 'a) * ('a -> parameter) * ('a -> parameter -> 'a) ->
  objective_fn -> parameters -> parameters

(* pad's typed extension *)
val gradient_descent_v3 :
  'a ate -> objective_fn -> parameters -> parameters

(* this one of the 3 xxxate is pretty general and can be reused *)
val deflate: 'a accompanied_param -> parameter

(*****************************************************************************)
(* Special gradient descents *)
(*****************************************************************************)

(* a.k.a momentum gradient descent.
 * Internally relies also on !mu (boost) in addition to !alpha and !revs.
 *)
val velocity_gradient_descent: objective_fn -> parameters -> parameters

(* Root Mean Square gradient descent.
 * Internally relies also on !beta (smooth decay for historical average).
 *)
val rms_gradient_descent: objective_fn -> parameters -> parameters

(* Adaptive moment estimation.
 * Internally relies on !mu (boost) and !beta (decay).
 *)
val adam_gradient_descent: objective_fn -> parameters -> parameters
