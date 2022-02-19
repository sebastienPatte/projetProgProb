
let usage_msg = "dune exec ./examples/[name].exe  [-verbose] "
let verbose = ref false
let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");]

module Enum_sampling = struct
  type info = {
    value: int;
    last : bool
  }
  type prob = {
    mutable old : info array;  
    mutable cur : info array;
    mutable scores : float array;
    mutable id_exec : int;
    mutable id_sample : int
  }

  let print_prob fmt prob = 
    Format.fprintf fmt "\nEXEC %d" prob.id_exec ;
    Format.fprintf fmt "\nOLD : ";
    Array.iter (fun {value=v;last=l} -> Format.fprintf fmt "(%d, %b); " v l;) prob.old;
    Format.fprintf fmt "\nCUR : ";
    Array.iter (fun {value=v;last=l} -> Format.fprintf fmt "(%d, %b); " v l;) prob.cur;
    Format.fprintf fmt "\nSCORES : ";
    Array.iter (Format.fprintf fmt "%f; ") prob.scores;
    Format.fprintf fmt "\n\n"

  let sample prob d = 
    
    let support = Distribution.get_support d in    

    let id = 
      if Array.length prob.old = 0 
      then 0 (*first execution*)
      else begin        
        let old_info = (prob.old.(prob.id_sample)) in
        if (prob.id_sample = 0 || (prob.old.(prob.id_sample-1).last))
        then begin 
          if old_info.last
          then 0 (* old value of same sample was the last : reinit to first possible value *)
          else old_info.value + 1 
        end
        else old_info.value (* previous sample did not reach its last value : don't increment *)
      end
    in 
    
    let new_info = {
      value = support.values.(id);
      last  = Array.length support.values <= (id+1) 
              && (prob.id_sample = 0 || (prob.cur.(prob.id_sample-1).last)) } 
    in
    
    (* we enlarge the array on 1st model execution, otherwise just modify it *)
    if prob.id_exec = 0 
    then prob.cur <- Array.append prob.cur [| new_info |]
    else prob.cur.(prob.id_sample) <- new_info;

    (* update "score" for current execution (add logit of current sample) *)
    prob.scores.(prob.id_exec) <- prob.scores.(prob.id_exec) +. (support.logits.(id));
    
    prob.id_sample <- prob.id_sample + 1;
    
    (* return choosen value for current sample *)
    new_info.value
  
  let assume prob p = 
    if not p then (prob.scores.(prob.id_exec) <- -.infinity)

  let observe prob d x =
    let y = sample prob d in
    assume prob (x = y)

  let infer  model data =
    let prob = {old=[||]; cur=[||]; scores=[||];id_exec=0;id_sample=0} in
    let rec exec i =
      prob.scores <- Array.append prob.scores [|1.|];
      
      let r = model prob data in
      
      if !verbose then print_prob Format.std_formatter prob;
      
      (* prob.old now points to prob.cur (pointer alias) *)
      prob.old <- prob.cur;
      (* replace prob.cur with an array of the same length, filled with dummy values *)
      prob.cur <- Array.make (Array.length prob.cur) {value=0;last=false};
      (* incr "exec" counter and reset "sample" counter *)
      prob.id_exec <- (prob.id_exec)+1;
      prob.id_sample <- 0;

      (* when the last "sample" reach its last value, we stop executions *)
      if prob.old.(Array.length prob.old -1).last   
      then [r]
      else r::exec (i+1)
    in


    let values = Array.of_list (exec 0) in
    
    Distribution.support ~values ~logits:prob.scores
end

module Rejection_sampling = struct
  type prob = Prob

  let sample _prob d = Distribution.draw d

  exception Reject

  let assume _prob p = if not p then raise Reject

  let observe prob d x =
    let y = Distribution.draw d in
    assume prob (x = y)

  let infer ?(n = 1000) model data =
    let rec exec i = try model Prob data with Reject -> exec i in
    let values = Array.init n exec in
    Distribution.uniform_support ~values
end

module Importance_sampling = struct
  type prob = { id : int; scores : float array }

  let sample _prob d = Distribution.draw d
  let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s
  let observe prob d x = factor prob (Distribution.logpdf d x)
  let assume prob p = factor prob (if p then 0. else -.infinity)

  let infer ?(n = 1000) model data =
    let scores = Array.make n 0. in
    let values = Array.mapi (fun i _ -> model { id = i; scores } data) scores in
    Distribution.support ~values ~logits:scores
end





module MH_MCMC = struct

type like_prior = {
  log_likelihood : float;
  log_prior : float;
}
type 'a mcmc_sample = {
  value : 'a;
  like_prior : like_prior
}
  type prob = { id : int; scores : float array }
  (* let q x y =
    let dist =  x - y in 
    exp  *)
  (* let target v =  (*do sth with v*) *)
  let min_s x = if 1. > x then x else  1.

  let sample _prob d = Distribution.draw d
  (* let q prob =   *)

  let MH prob d n q trace scores = 
    let x_current = sample prob d in
    for  i = 0 to n do 
      let x_new_val = (q x_current ) in 
      let x_new_score = scores in
      let rw  = Random.float 1.0  in
      let  p = (target x_new ) /. (target x_current) in 
      let pq_ratio = in (* ici faut diviser les scores*)
      let alpha = min_s pq   in 
      if alpha > rw 
        then 
        (*inserer dans le trace*)
          (* trace <- Array.append trace [|x_new_val|]; *)
          (* insert score too, or should we have trace as array of a custom type pair {value,score?} *)
          x_current = x_new_val
        else
          (* trace <- Array.append trace [|x_current|]; *)
          (* + maj score aussi *)
    done

  let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s
  let observe prob d x = factor prob (Distribution.logpdf d x)
  let assume prob p = factor prob (if p then 0. else -.infinity)

  let infer ?(n = 1000) model data =
    let scores = Array.make n 0. in
    let values = Array.mapi (fun i _ -> model { id = i; scores } data) scores in
    Distribution.support ~values ~logits:scores
end
