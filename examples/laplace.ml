open Byoppl
open Distribution


open Basic.Multi_sites_MH

let laplace prob () =
  let p = sample prob (uniform ~a:0. ~b:1.) in
  let () = observe prob (binomial ~p ~n:493_472) 241_945 in
  p

let _ =
  Format.printf "@.-- Laplace, Basic Multi_sites_MH Sampling --@.";
  let dist = infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s

open Basic.Importance_sampling

let laplace prob () =
  let p = sample prob (uniform ~a:0. ~b:1.) in
  let () = observe prob (binomial ~p ~n:493_472) 241_945 in
  p

let _ =
  Format.printf "@.-- Laplace, Basic Importance Sampling --@.";
  let dist = infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s

open Basic.HMC

let laplace prob () =
  let p = sample prob (uniform ~a:0. ~b:1.) in
  let () = observe prob (binomial ~p ~n:493_472) 241_945 in
  p

let _ =
  Format.printf "@.-- Laplace, Basic HMC Sampling --@.";
  let dist = infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s


open Cps_operators
open Infer.Importance_sampling

let laplace () =
  let* p = sample (uniform ~a:0. ~b:1.) in
  let* () = observe (binomial ~p ~n:493_472) 241_945 in
  return p

let _ =
  Format.printf "@.-- Laplace, CPS Importance Sampling --@.";
  let dist = infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s


open Infer.Multi_MH

let laplace () =
  let* p = sample (uniform ~a:0. ~b:1.) in
  let* () = observe (binomial ~p ~n:493_472) 241_945 in
  return p

let _ =
  Format.printf "@.-- Laplace, CPS Multi-sites MH --@.";
  let dist = infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s

open Infer.MH

let laplace () =
  let* p = sample (uniform ~a:0. ~b:1.) in
  let* () = observe (binomial ~p ~n:493_472) 241_945 in
  return p

let _ =
  Format.printf "@.-- Laplace, CPS Single-site MH --@.";
  let dist = infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s



