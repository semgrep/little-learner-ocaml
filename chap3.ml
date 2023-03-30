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
  let theta = [0.0; 0.0] in
  line line_xs theta
