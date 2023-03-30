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

(* learning rate, usually between 0.0 and 0.01, a.k.a step size *)
let alpha = ref 0.01

(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

let l2_loss target (* f *) =
  fun xs (* input *) ys (* expected output *) (* xs and ys = dataset *) ->
  fun theta (* tolearn *) ->
  let pred_ys (* predicted ys *) = target xs theta in
  sum (sqr (ys - pred_ys))
