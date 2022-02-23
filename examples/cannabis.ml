open Byoppl
open Distribution
open Owl_plplot

(* if set to "true" generates a graph at "graphs/cannabis.png" *)
let gen_graph = false ;;

let h = Plot.create ~m:3 ~n:3 ("graphs/cannabis.png") ;;

let graph values probs title x y = 
  let values = Array.append [|0|] (Array.append values [|0|]) in
  let probs = Array.append [|0.|] (Array.append probs [|0.|]) in
  if gen_graph then begin
  Plot.subplot h x y;
  let xaxis = Array.mapi 
    (fun i v -> (float_of_int (i+1),string_of_int v)) values  
  in
  xaxis.(0) <- (1.," ");
  xaxis.(3) <- (4.," ");

  Plot.set_title h title;
  Plot.set_xticklabels h (Array.to_list xaxis);
  Plot.set_xlabel h "results";
  Plot.set_ylabel h "probability";
  Plot.bar ~h  (Owl.Mat.of_arrays [| probs |]);
  Plot.output h
  end else ();
;;

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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "enum" 0 0


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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "rejection" 0 1


open Basic.Multi_sites_MH

let cannabis prob () = 
  let smoke = sample prob (bernoulli ~p:0.6) in
  let coin = sample prob (bernoulli ~p:0.5) in
  assume prob (coin=1 || smoke=1);
  smoke

let _ =
  Format.printf "@.-- Cannabis, Multi_sites_MH Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "multi_MH" 0 2

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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "importance" 1 0


open Basic.HMC
let cannabis prob () = 
  let smoke = sample prob (bernoulli ~p:0.6) in
  let coin = sample prob (bernoulli ~p:0.5) in
  assume prob (coin=1 || smoke=1);
  smoke

let _ =
  Format.printf "@.-- Cannabis, HMC Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "HMC" 1 1


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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "CPS_importance" 1 2



open Infer.Multi_MH

let cannabis () = 
  let* smoke = sample (bernoulli ~p:0.6) in
  let* coin = sample (bernoulli ~p:0.5) in
  let* () = assume (coin=1 || smoke=1) in
  return (smoke)

let _ =
  Format.printf "@.-- Cannabis, CPS Multi-sites MH Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "CPS_multi_MH" 2 0

open Infer.MH

let cannabis () = 
  let* smoke = sample (bernoulli ~p:0.6) in
  let* coin = sample (bernoulli ~p:0.5) in
  let* () = assume (coin=1 || smoke=1) in
  return (smoke)

let _ =
  Format.printf "@.-- Cannabis, CPS MH Single-site Sampling --@.";
  let dist = infer cannabis () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "CPS_MH" 2 1




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