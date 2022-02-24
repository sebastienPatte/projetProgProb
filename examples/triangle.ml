(* replace here with desired sampling method *)
open Byoppl
open Distribution
open Basic.Multi_sites_MH

let funny_bernoulli prob () =
  let lower = 0. in 
  let upper = 2. in 
  let x1 = int_of_float (sample prob (uniform ~a:lower ~b:upper)) in
  let y1 = int_of_float (sample prob (uniform ~a:lower ~b:upper)) in
  let x2 = int_of_float (sample prob (uniform ~a:lower ~b:upper)) in
  let y2 = int_of_float (sample prob (uniform ~a:lower ~b:upper)) in
  let x3 = int_of_float (sample prob (uniform ~a:lower ~b:upper)) in
  let y3 = int_of_float (sample prob (uniform ~a:lower ~b:upper) )in 
  
  x1, y1, x2, y2, x3, y3

let _ =
  (* Format.printf "@.-- Funny Bernoulli, Multi_sites_MH Sampling --@."; *)
  let dist = infer ~n:2 funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i (x1, y1, x2, y2, x3, y3) -> Format.printf "%d %d %d %d %d %d %f@." x1 y1 x2 y2 x3 y3 probs.(i)) values
  
