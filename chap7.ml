
(* from chap5.ml *)
let plane_xs =
  T [|
      T [| S 1.0; S 2.05|];
      T [| S 1.0; S 3.0|];
      T [| S 2.0; S 2.0|];
      T [| S 2.0; S 3.91|];
      T [| S 3.0; S 6.13|];
      T [| S 4.0; S 8.09|];
    |]
let plane_ys =
  T [| S 13.99; S 15.99; S 18.0; S 22.4; S 30.2; S 37.94|]

let try_plane a_gradient_descent =
  let obj = (sampling_obj (l2_loss plane)
               plane_xs plane_ys) in
  with_hyper revs 15000 (fun () ->
      with_hyper alpha 0.001 (fun () ->
         with_hyper batch_size 4 (fun () ->
             a_gradient_descent obj
            [T [| S 0.; S 0.|]; S 0.])))

let res_frame_24_alt =
  try_plane gradient_descent_v1

let lonely_i ( theta : parameters) =
  List.map (fun t -> [ t ]) theta

let lonely_d big_theta : parameters =
  List.map (fun x -> lref x 0) big_theta

let lonely_u big_theta gs =
    List.map2
      (fun big_p g -> [lref big_p 0 - (S !alpha) * g])
      big_theta
      gs

let gradient_descent_almost_v2
    (inflate, deflate, update)
      (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
      update
      big_theta
      (gradient_pad obj (deflate big_theta))
  in
  deflate (revise f !revs (inflate theta_init))


let lonely_gradient_descent =
  gradient_descent_almost_v2 (lonely_i, lonely_d, lonely_u)

let res_frame_24 =
  try_plane lonely_gradient_descent

let naked_i ( theta : parameters) =
  List.map (fun p -> let big_p = p in big_p) theta

let naked_d big_theta : parameters =
  List.map (fun big_p -> let p = big_p in p) big_theta

let naked_u big_theta gs =
    List.map2
      (fun big_p g -> big_p - (S !alpha) * g)
      big_theta
      gs

let naked_gradient_descent =
  gradient_descent_almost_v2 (naked_i, naked_d, naked_u)

let res_frame_24 =
  try_plane lonely_gradient_descent

let gradient_descent_almost_v2
    (inflate, deflate, update)
      (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
      update
      big_theta
      (gradient_pad obj (deflate big_theta))
  in
  deflate (revise f !revs (inflate theta_init))

let gradient_descent_v2
    (inflate, deflate, update)
      (obj : objective_fn) (theta_init : parameters) : parameters =
  let f big_theta =
      List.map2 update
      big_theta
      (gradient_pad obj (List.map deflate big_theta))
  in
  List.map deflate (revise f !revs (List.map inflate theta_init))


let lonely_i  (p : parameter) =
  [ p ]

let lonely_d big_p : parameter =
  lref big_p 0

let lonely_u big_p g =
  [lref big_p 0 - (S !alpha) * g]

let lonely_gradient_descent =
  gradient_descent_v2 (lonely_i, lonely_d, lonely_u)

let res_frame_41 =
  try_plane lonely_gradient_descent

let naked_i  (p : parameter) =
  p

let naked_d big_p : parameter =
  big_p

let naked_u big_p g =
  big_p - (S !alpha) * g

let naked_gradient_descent =
  gradient_descent_v2 (naked_i, naked_d, naked_u)
