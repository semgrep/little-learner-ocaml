(* mostly a continuation of chap1.ml but using tensors this time!
 * and extended operations! which are explained in interlude5.ml
 * actually, far later!
 *)
open Tensor (* *, +, on tensors *)

let line_xs = T [| S 2.0; S 1.0; S 4.0; S 3.0 |]
let line_ys = T [| S 1.8; S 1.2; S 4.2; S 3.3 |]

let lref xs n =
  List.nth xs n

let line = fun x -> fun theta ->
  x * (lref theta 0) + (lref theta 1)

let res_frame_6 =
  let theta = [S 0.0; S 0.0] in
  line line_xs theta

(* update: now in learning.ml *)

let l2_loss target (* f *) =
  fun xs (* input *) ys (* expected output *) (* xs and ys = dataset *) ->
  fun theta (* tolearn *) ->
  let pred_ys (* predicted ys *) = target xs theta in
  sum (sqr (ys - pred_ys))

(* "expectant" function (expect arguments) *)
let res_frame_24 =
  l2_loss line

(* "objective" function (expect parameters) *)
let res_frame_26 =
  l2_loss line line_xs line_ys

let res_frame_28 =
  l2_loss line line_xs line_ys [S 0.; S 0.]

let res_frame_31 =
    l2_loss line line_xs line_ys [S 0.0099; S 0.]

let res_frame_37 =
    l2_loss line line_xs line_ys [S 62.63; S 0.]
  
let res_frame_42 =
    l2_loss line line_xs line_ys [S 0.6263; S 0.]
  
let res_frame_44 =
  let obj : objective_fn =
    l2_loss line line_xs line_ys in
  obj
