open Tensor

let obj : objective_fn =
    l2_loss line line_xs line_ys

let res_frame_6 =
  (
    obj [S (-1.0); S 0.],
    obj [S 0.0; S 0.],
    obj [S 1.0; S 0.],
    obj [S 2.0; S 0.],
    obj [S 3.0; S 0.]
  )

(* update: now in learning.ml *)

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

let res_frame_17 =
  gradient_pad (fun theta -> sqr (lref theta 0)) [S 27.0]

let res_frame_20 =
  gradient_pad obj [S 0.; S 0.]

(* f = new theta compute = gradient *)
let rec revise (f : parameters -> parameters) (revs : int) (theta : parameters) : parameters =
  if revs = 0
  then theta
  else revise f (Stdlib.(-) revs 1) (f theta)

let res_frame_25 =
  let f = fun theta ->
    List.map (fun p -> p - (S 3.)) theta in
  revise f 5 [S 1.; S 2.; S 3.]

let res_frame_27 =
  List.map2 (fun x y -> x + y)
    [ S 12.; S 17.; S 32. ]
    [ S 8.; S 3.; S 11. ]

(* learning rate *)
let alpha = ref 0.01

(* "we need to increase  theta0 to reduce the lost, we must substract this negative value from the current theta0" *)
let res_frame_33 =
  let f theta =
    let gs = gradient_pad obj theta in
    [lref theta 0 - (S !alpha) * (lref gs 0);
     lref theta 1 - (S !alpha) * (lref gs 1)]
  in
  revise f 1000 [S 0.; S 0.]

let res_frame_42 =
  let f theta =
    let gs = gradient_pad obj theta in
    List.map2 (fun p g -> p - (S !alpha) * g) theta gs
  in
  revise f 1000 [S 0.; S 0.]

let revs = ref 1000

let gradient_descent (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
    List.map2
      (fun p g -> p - (S !alpha) * g)
      big_theta
      (gradient_pad obj big_theta)
  in
  revise f !revs theta_init

let res_frame_50 =
  gradient_descent ((l2_loss line) line_xs line_ys)
   [S 0.; S 0.]
