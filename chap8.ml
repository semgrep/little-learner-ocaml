
(* boosting *)
let mu = ref 0.9

let velocity_i p =
  [p; zeroes p]

let velocity_d big_p =
  lref big_p 0

let velocity_u big_p g =
  let v = (S !mu) * (lref big_p 1)  - (S !alpha) * g in
  [lref big_p 0 + v; v ]

let velocity_gradient_descent =
  gradient_descent_v2 (velocity_i, velocity_d, velocity_u)

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

let try_plane a_gradient_descent a_revs =
  let obj = (sampling_obj (l2_loss plane)
               plane_xs plane_ys) in
  with_hyper revs a_revs (fun () ->
      with_hyper alpha 0.001 (fun () ->
         with_hyper batch_size 4 (fun () ->
             a_gradient_descent obj
            [T [| S 0.; S 0.|]; S 0.])))

let res_frame_32 =
  with_hyper mu 0.9 (fun () ->
    try_plane velocity_gradient_descent 5000)
