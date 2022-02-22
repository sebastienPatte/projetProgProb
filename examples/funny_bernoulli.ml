open Byoppl
open Distribution

  
open Basic.Enum_sampling

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a = 1 || b = 1) in
  a + b + c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Enumeration Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

  (* open Basic.Single_site_MH

  let funny_bernoulli prob () =
    let a = sample prob (bernoulli ~p:0.5) in
    let b = sample prob (bernoulli ~p:0.5) in
    let c = sample prob (bernoulli ~p:0.5) in
    let () = assume prob (a = 1 || b = 1) in
    a + b + c
  
  let _ =
    Format.printf "@.-- Funny Bernoulli, Single_site_MH Sampling --@.";
    let dist = infer funny_bernoulli () in
    let { values; probs; _ } = get_support ~shrink:true dist in
    Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
   *)
    
open Basic.Multi_sites_MH

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a = 1 || b = 1) in
  a + b + c

let _ =
  Format.printf "@.-- Funny Bernoulli, Multi_sites_MH Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
  

open Basic.Rejection_sampling

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a = 1 || b = 1) in
  a + b + c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Rejection Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

open Basic.Importance_sampling

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a = 1 || b = 1) in
  a + b + c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Importance Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

open Cps_operators
open Infer.Importance_sampling

let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Importance Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

open Infer.Multi_MH

let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Multi-sites MH Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

open Infer.MH

let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Single-site MH Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
  
open Infer.Gen

let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Generation --@.";
  for _ = 1 to 10 do
    let v = draw funny_bernoulli () in
    Format.printf "%d " v
  done;
  Format.printf "@."
  