
(* Usually take the time of the day for init argument.
   Can also now use Random.self_init ()
 *)
let _ = Random.init 0

let res_frame_22 =
  Random.int 45

let rec sampled n i acc =
  if i = 0
  then acc
  else sampled n (Stdlib.(-) i 1) ((Random.int n)::acc)

let samples n s =
  sampled n s []

let res_frame_25 =
  samples 20 3

let trefs t xs =
  xs |> List.map (fun i -> tref t i) |> Array.of_list |> (fun arr -> T arr)

let res_frame_29 =
  let t = T [| S 5.0; S 2.8; S 4.2; S 2.3; S 7.4; S 1.7; S 8.1|] in
  trefs t [6; 0; 3; 1]

let batch_size = ref 4

(* now in learning.ml *)
let sampling_obj (expectant : expectant_fn) (xs : t) (ys : t) : objective_fn =
  let n = tlen xs in
  (* see comment in learning.ml about why we need this extra unit parameter *)
  (fun () ->
     let b = samples n !batch_size in
     (fun theta ->
       expectant (trefs xs b) (trefs ys b) () theta))

(* from chap3.ml *)
let line_xs = T [| S 2.0; S 1.0; S 4.0; S 3.0 |]
let line_ys = T [| S 1.8; S 1.2; S 4.2; S 3.3 |]

(* bugfix: used to get S nan; S nan as result theta :(
 * because of the way gradient_pad was written. Now with
 * the extra unit argument and 'let fobj = obj ()' in
 * gradient_pad this works better.
 *)
let res_frame_37 =
  let obj = (sampling_obj (l2_loss line)
               line_xs line_ys) in
  with_hyper revs 1000 (fun () ->
      with_hyper alpha 0.001 (fun () ->
         with_hyper batch_size 4 (fun () ->
             gradient_descent_v1 obj
            [S 0.; S 0.])))

(* with bad batch, the gradient_pad may be big and we might
 * diverge too fast, so better put more revisions and
 * a smaller alpha? no, not better actually.
 *)
let res_frame_37_different_hyperparam =
  let obj = (sampling_obj (l2_loss line)
               line_xs line_ys) in
  with_hyper revs 15000 (fun () ->
      with_hyper alpha 0.00001 (fun () ->
         with_hyper batch_size 4 (fun () ->
             gradient_descent_v1 obj
            [S 0.; S 0.])))

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

let res_frame_42 =
  let obj = (sampling_obj (l2_loss plane)
               plane_xs plane_ys) in
  with_hyper revs 15000 (fun () ->
      with_hyper alpha 0.001 (fun () ->
         with_hyper batch_size 4 (fun () ->
             gradient_descent_v1 obj
            [T [| S 0.; S 0.|]; S 0.])))

