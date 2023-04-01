open Tensor
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff to learn, the "parameters" of the target_fn (e.g., line()),
 * often called 'theta' *)
type parameters = t list

(* e.g., line(), quad(), relu() *)
type target_fn =
  t (* input, the "arguments" of the target_fn *) -> parameters -> t (* output *)

type objective_fn =
  parameters -> scalar (* the loss *)

(* expect datasets as arguments *)
type expectant_fn =
  t (* input dataset *) -> t (* predicted output dataset *) ->
  objective_fn

let debug = ref true

(*****************************************************************************)
(* Target functions *)
(*****************************************************************************)

let lref xs n =
  List.nth xs n

(* y = f(x) = wx + b *)
let line x = fun theta ->
  x * (lref theta 0) + (lref theta 1)

(* f(x) = ax^2 + bx + c *)
let quad t = fun theta ->
  sqr t * (lref theta 0) +
    t * (lref theta 1) +
    (lref theta 2)

(* f(x) = a o x + b (o = dotproduct) *)
let plane t = fun theta ->
  dotproduct (lref theta 0) t +
    (lref theta 1)

(*****************************************************************************)
(* Hyperparameters *)
(*****************************************************************************)

(* learning rate, usually between 0.0 and 0.01, a.k.a step size *)
let alpha = ref 0.01

let revs = ref 1000

(* samples size *)
let batch_size = ref 4

(* =~ save_excursion *)
let with_hyper aref newv f =
  let old = !aref in
  aref := newv;
  Fun.protect f ~finally:(fun _ -> aref := old)

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
(* Gradient descent v1 *)
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
       if !debug
       then pr2 (spf "grad_theta0: %s" (Tensor.show grad_theta0));
       grad_theta0 :: aux (theta0::theta_before) xs
  in
  aux [] theta

(* f = new theta compute = gradient *)
let rec revise (f : parameters -> parameters) (revs : int) (theta : parameters) : parameters =
  if revs =|= 0
  then theta
  else revise f (Stdlib.(-) revs 1) (f theta)

let gradient_descent_v1 (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
    List.map2
      (fun p g -> p - (S !alpha) * g)
      big_theta
      (gradient_pad obj big_theta)
  in
  revise f !revs theta_init

(*****************************************************************************)
(* Stochastic gradient *)
(*****************************************************************************)

let init () = Random.self_init ()

let rec sampled n i acc =
  if i =|= 0
  then acc
  else sampled n (Stdlib.(-) i 1) ((Random.int n)::acc)

let samples n s =
  sampled n s []

let trefs t xs =
  xs |> List.map (fun i -> tref t i) |> Array.of_list |> (fun arr -> T arr)

let sampling_obj (expectant : expectant_fn) (xs : t) (ys : t) : objective_fn =
  let n = tlen xs in
  (fun theta ->
    (* we must generate samples inside the closure, otherwise
     * we would get the loss each time from the same batch
     *)
     let b = samples n !batch_size in
     (* b |> List.iter (fun x -> print_int x); *)
     (* manual sample that works: [3; 2; 0; 1 ] *)
    expectant (trefs xs b) (trefs ys b) theta)
