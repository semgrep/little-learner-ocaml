open Tensor
open Learning (* for lref *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* neuron *)
type neuron = Learning.target_fn

(*****************************************************************************)
(* Simple units *)
(*****************************************************************************)

let rectify0 t =
  match t with
  | S s ->
     if s < 0.0
     then S 0.0
     else t
  | _ -> failwith "rectify0: the argument is not a tensor0 (a scalar)"

(* decider, a.k.a activation function *)
let rectify = ext1 rectify0 0

let linear_1_1 t theta =
  dotproduct (lref theta 0) t + lref theta 1

(* rectifying linear unit *)
let relu_1_1 t theta =
  rectify (linear_1_1 t theta)

