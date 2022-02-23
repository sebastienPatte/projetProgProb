


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
      prob.scores <- Array.append prob.scores [|0.|];
      
      let r = model prob data in
      
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


module Multi_sites_MH = struct
  type prob = { id : int; scores : float array; }

  let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s

  let sample prob d = 
    let s = Distribution.draw d in
    factor prob (Distribution.logpdf d s);
    s
  
  let assume prob p = factor prob (if p then 0. else -.infinity)
  
  let observe prob d x = factor prob (Distribution.logpdf d x)
  
  let infer ?(n = 1000) model data =
    let scores = Array.make n 0. in
    
    let old_res = ref (model { id = 0; scores } data) in
    
    (* returns true with probability "p" *)
    let decide p = Random.float 1. <= p in
    
    let exec i _ =  
      if i=0 then !old_res 
      else begin
        let r = model { id = i; scores } data  in
        if scores.(i) >= scores.(i-1) || decide ( scores.(i) /. scores.(i-1) )
        then old_res := r
        else scores.(i) <- scores.(i-1);
        !old_res
      end
    in 
    let values = Array.mapi exec scores in
    Distribution.support ~values ~logits:scores
end

module HMC = struct
  type prob = { id : int; scores : float array; }
 
  
  let factor prob s = prob.scores.(prob.id) <- prob.scores.(prob.id) +. s
  let sample prob d = 
    let s = Distribution.draw d in
    factor prob (Distribution.logpdf d s);
    s
  let observe prob d x = factor prob (Distribution.logpdf d x)
  let assume prob p = factor prob (if p then 0. else -.infinity)
 
  (* maybe add the normal function to distribution later *)
  (* the momentum P is sampled from a normal distro *)  
  let normal x mu sigma= 
    let a =exp  (-1. *. ((x -. mu)**2.)/. (2. *. sigma**2.))  in
    let pi =  3.1415926 in 
    let b = sigma *. sqrt (2. *. pi) in
    a /. b
 
  let neg_log_prob x mu sigma = 
    -. 1. *.  (  normal  x mu sigma )
 
 
  let infer ?(n = 1000) model data =
    let scores = Array.make n 0. in
    let mu = 0. and sigma = 1. in 
    let path_len = 1. and  step_size = 0.5 in
    let steps = int_of_float ( path_len /. step_size ) in
 
    let old_res = ref (model { id = 0; scores } data) in
    let momentum sth = normal sth mu sigma in
 
 
 
    (* returns true with probability "p" *)
    (* let decide p = Random.float 1. <= p in *)
    (* let min_s x = if 1. > x then x else  1. in *)
    (* H the hamiltonian discretized *)
 
    let ham p q gradient    = 
    let r_p = ref p in
    let r_q = ref q in
    for _ = 0 to steps do
      r_p :=  !r_p  +. step_size *. ( gradient /. 2.) ;
      r_q :=  !r_q  +. !r_p *. step_size ;
      r_p :=  !r_p  +. step_size *. ( gradient /. 2.) 
    done;
    !r_p , !r_q
    in 
 
    let exec i _ =  
      if i=0 then !old_res 
      else begin
        (* let s1 = scores.(i) in  *)
        let s_1 = scores.(i-1) in
        
        if s_1 = -.infinity then begin 
          (*the old score is -inf, then we accept the new execution *)
          old_res := model { id = i; scores } data;
          !old_res
        end
        else begin 

          let q0 =   s_1 in
          let p0 =  momentum (Random.float 1.0) in
          let gradient = -. (  q0 -. mu /.  sigma ** 2. ) in
          let p1, q1 = ham p0 q0 gradient in  
          let r = model { id = i; scores } data  in
 
          (* edit next line properly *)
 
 
          (* let pq = (exp ( -. ham   s1   s1   ))  /. (exp ( -. ham s_1 s_1)) in *)
          let q0_nlp = neg_log_prob q0 mu sigma in 
          let q1_nlp = neg_log_prob q1 mu sigma in
          let p0_nlp = neg_log_prob p0 0. 1. in
          let p1_nlp = neg_log_prob p1 0. 1. in
          let target = q0_nlp -. q1_nlp  (*P(q1)/P(q0)*) in
          let adj = p1_nlp -. p0_nlp  (*P(p0)/P(p1)*) in
          let alpha = target +. adj in (*temp line*)
          (* let alpha = min_s pq   in  *)
          let acc  = Random.float 1.0  in
          if acc < alpha
          (* if scores.(i) >= scores.(i-1) || decide (scores.(i-1) /. scores.(i) ) *)
          then old_res := r;
          !old_res 
        end
      end
    in 
    let values = Array.mapi exec scores in
    Distribution.support ~values ~logits:scores
end