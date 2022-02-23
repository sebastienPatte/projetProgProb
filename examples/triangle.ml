(* replace here with desired sampling method *)
open Basic.Multi_sites_MH

let funny_bernoulli prob () =
  let lower = 10 in 
  let upper = 30 in 
  let x1 = sample prob (uniform ~a:lower ~b:upper) in
  let y1 = sample prob (uniform ~a:lower ~b:upper) in
  let x2 = sample prob (uniform ~a:lower ~b:upper) in
  let y2 = sample prob (uniform ~a:lower ~b:upper) in
  let x3 = sample prob (uniform ~a:lower ~b:upper) in
  let y3 = sample prob (uniform ~a:lower ~b:upper) in 
  
  x1, y1, x2, y2, x3, y3;

let _ =
  (* Format.printf "@.-- Funny Bernoulli, Multi_sites_MH Sampling --@."; *)
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
  
