
let res_frame_6 =
  with_hyper revs 1000 (fun () ->
      with_hyper alpha 0.01 (fun () ->
          gradient_descent_v1 ((l2_loss line) line_xs line_ys)
            [S 0.; S 0.]))

let quad_xs = T [| S (-1.0); S 0.0; S 1.0; S 2.0; S 3.0 |]
let quad_ys = T [| S 2.55; S 2.1; S 4.35; S 10.2; S 18.25 |]

let quad t = fun theta ->
  sqr t * (lref theta 0) +
    t * (lref theta 1) +
    (lref theta 2)

let res_frame_16 =
  quad (S 3.0)
    [S 4.5; S 2.1; S 7.8]

let res_frame_22 =
  with_hyper revs 1000 (fun () ->
      with_hyper alpha 0.001 (fun () ->
          gradient_descent_v1 ((l2_loss quad) quad_xs quad_ys)
            [S 0.; S 0.; S 0.]))

let plane_xs =
  T [|
      T [| S 1.0; S 2.05|];
      T [| S 1.0; S 3.0|];
      T [| S 2.0; S 2.0|];
      T [| S 2.0; S 3.91|];
      T [| S 3.0; S 6.13|];
      T [| S 4.0; S 8.09|];
    |]

(* =~ plane_zs *)
let plane_ys =
  T [| S 13.99; S 15.99; S 18.0; S 22.4; S 30.2; S 37.94|]

let plane t = fun theta ->
  dotproduct (lref theta 0) t +
    (lref theta 1)

let res_frame_27 =
  dotproduct
    (T [| S 2.; S 1.; S 7.|])
    (T [| S 8.; S 4.; S 3.|])

(* TODO: get different result than in book :( but those values
 * also fit well the data :) so maybe because gradient_pad is different
 *)
let res_frame_38 =
  with_hyper revs 10000 (fun () ->
      with_hyper alpha 0.001 (fun () ->
          gradient_descent_v1 ((l2_loss plane) plane_xs plane_ys)
            [T [| S 0.; S 0.|]; S 0.]))

let res_frame_40_my_value =
  plane (T [| S 2.; S 3.91|])
   [ T [| S 2.61; S 2.61|]; S 6.47]

let res_frame_40 =
  plane (T [| S 2.; S 3.91|])
   [ T [| S 3.98; S 2.04|]; S 5.78]
