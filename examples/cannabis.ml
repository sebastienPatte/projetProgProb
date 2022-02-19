open Byoppl
open Distribution


let () =
  Arg.parse Basic.speclist (fun _ -> ()) Basic.usage_msg;

  open Basic.Enum_sampling

let hmm _prob () =
  let m = 1.0 in
  let log = Owl_stats.gaussian_pdf ~mu:m ~sigma:1.0 1.2 in
  log
  
let _ = 
  Format.printf "gaussian logpdf : %f\n" (hmm {old=[||]; cur=[||]; scores=[||];id_exec=0;id_sample=0} ());
  

  
open Basic.Enum_sampling

let cannabis prob () = 
  let smoke = sample prob (bernoulli ~p:0.6) in
  let coin = sample prob (bernoulli ~p:0.5) in
  assume prob (coin=1 || smoke=1);
  smoke

  let _ =
    Format.printf "@.-- Cannabis, Basic Enumeration Sampling --@.";
    let dist = infer cannabis () in
    let { values; probs; _ } = get_support ~shrink:true dist in
    Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values


open Basic.Rejection_sampling
let cannabis prob () = 
  let smoke = sample prob (bernoulli ~p:0.6) in
  let coin = sample prob (bernoulli ~p:0.5) in
  assume prob (coin=1 || smoke=1);
  smoke

let _ =
  Format.printf "@.-- Cannabis, Rejection Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

open Basic.Importance_sampling
let cannabis prob () = 
  let smoke = sample prob (bernoulli ~p:0.6) in
  let coin = sample prob (bernoulli ~p:0.5) in
  assume prob (coin=1 || smoke=1);
  smoke

let _ =
  Format.printf "@.-- Cannabis, Importance Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
  

open Cps_operators
open Infer.Gen

let cannabis () = 
  let* smoke = sample (bernoulli ~p:0.6) in
  let* coin = sample (bernoulli ~p:0.5) in
  let* () = assume (coin=1 || smoke=1) in
  return smoke

let _ =
  Format.printf "@.-- Cannabis, CPS Generation --@.";
  for _ = 1 to 10 do
    let v = draw cannabis () in
    Format.printf "%d " v
  done;
  Format.printf "@."

open Cps_operators
open Infer.Importance_sampling

let cannabis () = 
  let* smoke = sample (bernoulli ~p:0.6) in
  let* coin = sample (bernoulli ~p:0.5) in
  let* () = assume (coin=1 || smoke=1) in
  return smoke

let _ =
  Format.printf "@.-- Cannabis, CPS Importance Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values
