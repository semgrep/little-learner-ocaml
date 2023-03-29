open Float

(* =~ y = x + 0 *)
let line_xs = [| 2.0; 1.0; 4.0; 3.0 |]
let line_ys = [| 1.8; 1.2; 4.2; 3.3 |]

let lref xs n =
  List.nth xs n

let line = fun x -> fun theta ->
  x *. (lref theta 0) +. (lref theta 1)

let ex_frame_28 =
  line 7.3 [1.0; 0.0]
(* => 7.3 *)
