open Tensor

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff to learn, the "parameters" of the target_fn (e.g., line()),
 * often called 'theta' *)
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

let revs = ref 1000

(*****************************************************************************)
(* Loss *)
(*****************************************************************************)

let l2_loss target (* f *) =
  fun xs (* input *) ys (* expected output *) (* xs and ys = dataset *) ->
  fun theta (* tolearn *) ->
  let pred_ys (* predicted ys *) = target xs theta in
  (* extended operations! *)
  sum (sqr (ys - pred_ys))

(*****************************************************************************)
(* Gradient descent *)
(*****************************************************************************)

(* TODO: define with better one using AutoGrad, see Appendix A *)
(* define gradient myself, just enough to handle the example above *)
let gradient_pad (obj : objective_fn) (theta : parameters) : parameters =
  let vold = obj theta in
  let rec aux theta_before theta_rest =
    match theta_rest with
    | [] -> []
    | theta0::xs ->
       let new_theta0 = theta0 + (S 0.0001) in
       let new_theta =
         List.rev theta_before @ [new_theta0] @ xs in
       let vnew = obj new_theta in
       let grad_theta0 = (vnew - vold) / (S 0.0001) in
       grad_theta0 :: aux (theta0::theta_before) xs
  in
  aux [] theta

(* f = new theta compute = gradient *)
let rec revise (f : parameters -> parameters) (revs : int) (theta : parameters) : parameters =
  if revs = 0
  then theta
  else revise f (Stdlib.(-) revs 1) (f theta)


let gradient_descent (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
    List.map2
      (fun p g -> p - (S !alpha) * g)
      big_theta
      (gradient_pad obj big_theta)
  in
  revise f !revs theta_init
