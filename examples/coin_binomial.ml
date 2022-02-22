open Byoppl
open Distribution


open Basic.Enum_sampling

let coin prob () = 
  sample prob (binomial_sup ~p:0.5 ~n:5)
  

let _ =
  Format.printf "@.-- Coin binomial, Basic Enumeration Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values


open Basic.Rejection_sampling
let coin prob () = 
  sample prob (binomial ~p:0.5 ~n:5)
  

let _ =
  Format.printf "@.-- Coin binomial, Rejection Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values


open Basic.Multi_sites_MH
let coin prob () = 
  sample prob (binomial ~p:0.5 ~n:5)
  

let _ =
  Format.printf "@.-- Coin binomial, Multi_sites_MH Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values  
    


open Basic.Importance_sampling
let coin prob () = 
  sample prob (binomial ~p:0.5 ~n:5)
  

let _ =
  Format.printf "@.-- Coin binomial, Importance Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values  

open Cps_operators
open Infer.Gen

let coin () = 
  let* c = sample (binomial ~p:0.5 ~n:5) in
  return c
  
let _ =
  Format.printf "@.-- Coin binomial, CPS Generation --@.";
  for _ = 1 to 10 do
    let v = draw coin () in
    Format.printf "%d " v
  done;
  Format.printf "@."

open Infer.Multi_MH

let coin () = 
  let* c = sample (binomial ~p:0.5 ~n:5) in
  return c

let _ =
  Format.printf "@.-- Coin binomial, CPS Multi-sites MH Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
  
open Infer.MH

let coin () = 
  let* c = sample (binomial ~p:0.5 ~n:5) in
  return c

let _ =
  Format.printf "@.-- Coin binomial, CPS Single-site MH Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
  

open Infer.Importance_sampling

let coin () = 
  let* c = sample (binomial ~p:0.5 ~n:5) in
  return c

let _ =
  Format.printf "@.-- Coin binomial, CPS Importance Sampling --@.";
  let dist = infer coin () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
