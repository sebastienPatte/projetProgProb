open Byoppl
open Distribution
open Owl_plplot

(* if set to "true" generates a graph at "graphs/funny_bernoulli.png" *)
let gen_graph = false ;;

let h = Plot.create ~m:3 ~n:3 ("graphs/funny_bernoulli.png") ;;

let graph values probs title x y = 
  if gen_graph then begin
    Format.printf "graph\n";
  Plot.subplot h x y;
  let xaxis = Array.mapi (fun i v -> 
    Format.printf "%d" v ;
    (float_of_int (i+1),string_of_int v)) values  in
  Plot.set_title h title;
  Plot.set_xticklabels h (Array.to_list xaxis);
  Plot.set_xlabel h "results";
  Plot.set_ylabel h "probability";
  Plot.bar ~h  (Owl.Mat.of_arrays [| probs |]);
  Plot.output h
  end else ()
;;

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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  Format.printf "graph\n";
  (* let probs = Array.map (Float.exp) probs in *)
  graph values probs "enum" 0 0


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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  Format.printf "%d %d\n" (Array.length values) (Array.length probs);
  
  (* add 0 value/prob for graph generation*)
  let values = Array.append values [|0|] in
  let probs = Array.append probs [|0.|] in
  graph values probs "rejection" 0 1

  
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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  
  graph values probs "multi_MH" 0 2



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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "importance" 1 0

open Basic.HMC

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a = 1 || b = 1) in
  a + b + c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic HMC Sampling --@.";
  let dist = infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "HMC" 1 1


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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "CPS_importance" 1 2

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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "CPS_multi_MH" 2 0

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
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values;
  graph values probs "CPS_MH" 2 1
  
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
  