
(* smooth decay *)
let beta = ref 0.9

(* stabilizer for modifier 1 / G *)
let epsilon = 1e-08

let rms_u big_p g =
  let r = smooth !beta (lref big_p 1) (sqr g) in
  let alpha_hat = (S !alpha) / (sqrt r + (S epsilon)) in
  [lref big_p 0 - alpha_hat * g; r ]

let rms_i p =
  [p; zeroes p]

let rms_d big_p =
  lref big_p 0


let rms_gradient_descent =
  gradient_descent_v2 (rms_i, rms_d, rms_u)

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

let try_plane a_gradient_descent a_revs an_alpha =
  let obj = (sampling_obj (l2_loss plane)
               plane_xs plane_ys) in
  with_hyper revs a_revs (fun () ->
      with_hyper alpha an_alpha (fun () ->
         with_hyper batch_size 4 (fun () ->
             a_gradient_descent obj
            [T [| S 0.; S 0.|]; S 0.])))

let res_frame_33 =
  with_hyper beta 0.9 (fun () ->
    try_plane rms_gradient_descent 3000 0.01)
      

let adam_u big_p g =
  let r = smooth !beta (lref big_p 2) (sqr g) in
  let alpha_hat = (S !alpha) / (sqrt r + (S epsilon)) in
  (* could be called g_hat *)
  let v = smooth !mu (lref big_p 1) g in
  [lref big_p 0 - alpha_hat * v; v; r ]


let adam_i p =
  let v = zeroes p in
  let r = v in
  [p; v; r]

let adam_d big_p =
  lref big_p 0

let adam_gradient_descent =
  gradient_descent_v2 (adam_i, adam_d, adam_u)

(* in the book they use 0.001 for alpha, but I think it's a typo
 * because with 0.001 we need more revisions actually. And the whole
 * idea of adaptive momentum is to take bigger alpha since
 * they will be adapted anyway so no fear we would go too fast.
 *)
let res_frame_43 =
  with_hyper mu 0.85 (fun () ->
  with_hyper beta 0.9 (fun () ->
    try_plane adam_gradient_descent 1500 0.001))

let res_frame_43_pad =
  with_hyper mu 0.85 (fun () ->
  with_hyper beta 0.9 (fun () ->
    try_plane adam_gradient_descent 1500 0.01))
