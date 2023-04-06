(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* artificial neuron (contains a linear part, and a nonlinear decider) *)
type neuron = Learning.target_fn

(*****************************************************************************)
(* Simple units *)
(*****************************************************************************)

(* not a neuron, because only linear part *)
val linear_1_1: Learning.target_fn

(* decider, a.k.a activation function (nonlinear) *)
val rectify: Tensor.t -> Tensor.t

(* rectifying linear unit (use rectify and linear_1_1) *)
val relu_1_1: neuron
