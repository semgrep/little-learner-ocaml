
let smooth decay_rate average g =
  (S decay_rate) * average + (S (1.0 -. decay_rate)) * g

let res_frame_4 =
  smooth 0.9 (S 0.0) (S 50.3)

let res_frame_5 =
  smooth 0.9 (S 5.03) (S 22.7)

let res_frame_6 =
  smooth 0.9 (S 6.8) (S 4.3)

let res_frame_17 =
  let avg = T [| S 0.8; S 3.1; S 2.2|] in
  let t1 = T [| S 1.0; S 1.1; S 3.0|] in
  let avg1 = smooth 0.9 avg t1 in
  let t2 = T [| S 13.4; S 18.2; S 41.4 |] in
  let avg2 = smooth 0.9 avg1 t2 in
  let t3 = T [| S 1.1; S 0.3; S 67.3 |] in
  let avg3 = smooth 0.9 avg2 t3 in
  avg1, avg2, avg3
