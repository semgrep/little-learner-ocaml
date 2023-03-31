
(* =~ save_excursion *)
let with_hyper aref newv f =
  let old = !aref in
  aref := newv;
  Fun.protect f ~finally:(fun _ -> aref := old)
