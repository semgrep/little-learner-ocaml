open Tensor
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the stuff we want to learn, the "parameters" of a target_fn (e.g., line()),
 * often called 'theta' *)
type parameters = parameter list
(* a parameter is a tensor *)
and parameter = Tensor.t
[@@deriving show]

(* e.g., line(), quad(), relu() *)
type target_fn =
  t (* input, the "arguments" of the target_fn *) -> parameters -> t (* output *)

(* The first unit argument is a hack because of the way gradient_pad work.
 * See the comment in sampling_obj() for more information.
 *)
type objective_fn =
  unit -> parameters -> scalar (* the loss *)

(* expect datasets as arguments *)
type expectant_fn =
  t (* input dataset *) -> t (* predicted output dataset *) ->
  objective_fn

let debug = ref true

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

(* boosting *)
let mu = ref 0.9

(* smooth decay *)
let beta = ref 0.9

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
  fun () theta (* tolearn *) ->
  let pred_ys (* predicted ys *) = target xs theta in
  (* extended operations! *)
  let res = sum (sqr (ys - pred_ys)) in
  if !debug
  then pr2 (spf "loss for %s: %s" (show_parameters theta) (Tensor.show res));
  res

(*****************************************************************************)
(* Gradient descent v1 *)
(*****************************************************************************)

(* TODO: define with better one using AutoGrad, see Appendix A *)
(* define gradient myself, just enough to handle the example above *)
let gradient_pad (obj : objective_fn) (theta : parameters) : parameters =
  (* this is needed because of sampling_obj(). See the comment there. *)
  let fobj = obj () in
  let vold = fobj theta in
  let rec aux theta_before theta_rest =
    match theta_rest with
    | [] -> []
    | theta0::xs ->
       let new_theta0 = theta0 + (S 0.0001) in
       let new_theta =
         List.rev theta_before @ [new_theta0] @ xs in
       let vnew = fobj new_theta in
       let grad_theta0 = (vnew - vold) / (S 0.0001) in
       grad_theta0 :: aux (theta0::theta_before) xs
  in
  let res = aux [] theta in
  if !debug
  then pr2 (spf "gradient_pad: [%s]" (show_parameters res));
  res


(* f = new theta compute = gradient.
 * use 'parameters instead of parameters so that it can be used also in
 * gradient_descent_v2 which use accompanied parameters
 *)
let rec revise (f : 'parameters -> 'parameters) (revs : int) (theta : 'parameters) : 'parameters =
  if revs =|= 0
  then theta
  else revise f (Stdlib.(-) revs 1) (f theta)

let gradient_descent_v1 (obj : objective_fn) (theta_init : parameters) : parameters =
  let f theta =
    List.map2
      (fun p g -> p - (S !alpha) * g)
      theta
      (gradient_pad obj theta)
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

let sampling_obj (expectant : expectant_fn) (xs : t) (ys : t) : objective_fn =
  let n = tlen xs in
  (fun () ->
    (* We must generate samples inside the closure, otherwise
     * we would get the loss each time from the same batch
     * But gradient_pad() calls the objective function [obj] a few times,
     * and here we would call it each time with a different samples, which
     * can't work! We must call obj with theta and theta+delta on the same
     * (sampled) dataset otherwise the loss will be completely different.
     * Which is why we introduce this extra unit argument, to give
     * the chance to compule the samples here and to share it
     * across multiple calls to the objective function.
     *)
     let b = samples n !batch_size in
    
    (fun theta ->
      expectant (trefs xs b) (trefs ys b) () theta))

(*****************************************************************************)
(* Gradient descent v2 and v3 (with the -ate) *)
(*****************************************************************************)

let gradient_descent_v2
    (inflate, deflate, update)
      (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
      List.map2 update
      big_theta
      (gradient_pad obj (List.map deflate big_theta))
  in
  List.map deflate (revise f !revs (List.map inflate theta_init))

let gradient_descent_v3
      (ate: 'a ate)
      (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
      List.map2 ate.update
      big_theta
      (gradient_pad obj (List.map ate.deflate big_theta))
  in
  List.map ate.deflate (revise f !revs (List.map ate.inflate theta_init))

let deflate big_p =
  big_p.p

  
(*****************************************************************************)
(* Special gradient descent *)
(*****************************************************************************)

(* a.k.a momentum gradient descent (use boosting) *)
let velocity_gradient_descent =
  gradient_descent_v3 {
      (* x = previous value of the velocity =~ - (alpha x g)+ boost, initiliazed to 0 *)
      inflate = (fun p -> { p; x = zeroes p });
      deflate;
      update = (fun big_p g ->
        let v = (S !mu) * big_p.x  - (S !alpha) * g in
        let p = big_p.p + v in
        {p; x = v })
    }

let smooth decay_rate average g =
  (S decay_rate) * average + (S (1.0 -. decay_rate)) * g

(* stabilizer for modifier 1 / G *)
let epsilon = 1e-08

(* adaptive descent 1, RMSProp = Root Mean Square, because we use the Mean
 *  (the smoothed historical average), of the Squares and then takes
 * its square Root. Prop stands for backPropagation.
 * Adaptive because as we descent we adapt the learning rate with
 * a modifier which grows inversely proportional to the gradient
 * (as the gradient gets smaller, alpha(hat) gets bigger to "compensate".
 *)
let rms_gradient_descent =
  gradient_descent_v3 {
      (* x = historical accumulated average (squared) *)
      inflate = (fun p -> { p; x = zeroes p } );
      deflate;
      update = (fun {p = old_p; x = old_r} g ->
         let r = smooth !beta old_r (sqr g) in
         let alpha_hat = (S !alpha) / (sqrt r + (S epsilon)) in
         let p = old_p - alpha_hat * g in
         { p; x = r }
      )
    }

(* adaptive descent 2, AdaM for Adaptive Momentum estimation *)
let adam_gradient_descent =
  gradient_descent_v3 {
      (* x = (historical average for XXX x historical avererage for YYY)  *)
      inflate = (fun p -> { p; x = (zeroes p, zeroes p) });
      deflate;
      update = (fun { p = old_p; x = (old_v, old_r) } g ->
        (* from rms *)
        let r = smooth !beta old_r (sqr g) in
        let alpha_hat = (S !alpha) / (sqrt r + (S epsilon)) in
        (* from velocity, could be called g_hat *)
        let v = smooth !mu old_v g in
        { p = old_p - alpha_hat * v; x = (v, r) }
      )
    }
