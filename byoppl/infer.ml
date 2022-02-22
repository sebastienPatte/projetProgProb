module Gen = struct
  type 'a prob = 'a option
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob =
    let v = Distribution.draw d in
    k v prob

  let factor _s k prob = k () prob
  let observe d x = factor (Distribution.logpdf d x)
  let assume p = factor (if p then 0. else -.infinity)
  let exit v _prob = Some v

  let draw model data =
    let v = (model data) exit None in
    Option.get v
end

module Multi_MH = struct
  type 'a prob = { id : int; particles : 'a particle array }
  and 'a particle = { value : 'a option; score : float; k : 'a next }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with score = s +. particle.score };
    k () prob

  let factor_aux s prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with score = s +. particle.score }
  

  let sample d k prob =
    let v = Distribution.draw d in
    let s = (Distribution.logpdf d v) in
    factor_aux s prob;
    k v prob

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)

  (* returns true with probability "p" *)
  let decide p = Random.float 1. <= p 
  let run_next prob =
    
    if prob.id < Array.length prob.particles - 1 then begin
      
      let k = prob.particles.(prob.id + 1).k in
      let r = k { prob with id = prob.id + 1 } in
      
      if prob.id >= 0 
      && prob.particles.(prob.id+1).score < prob.particles.(prob.id).score
      && not (decide ( (prob.particles.(prob.id+1).score) /. (prob.particles.(prob.id).score)))
      then prob.particles.(prob.id+1) <- {prob.particles.(prob.id) with k};
      
      r        
    end
    else prob

  let exit v prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with value = Some v };
    run_next prob

  let infer ?(n = 1000) model data =
    let init_particle = { value = None; score = 0.; k = (model data) exit } in
    let prob = { id = -1; particles = Array.make n init_particle } in
    
    let prob = run_next prob in

    let values = Array.map (fun p -> Option.get p.value) prob.particles in
    let logits = Array.map (fun p -> p.score) prob.particles in
    Distribution.support ~values ~logits
end


module MH = struct
  type 'a prob = { id : int; particles : 'a particle array }
  and 'a particle = { mutable value : 'a option; mutable score : float; k : 'a next ; mutable control : 'a control array}
  and 'a control = { d : 'a Distribution.t; s : float; cont_k : 'a -> 'a next}
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with score = s +. particle.score };
    k () prob

  let factor_aux s prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with score = s +. particle.score }
  
  let factor_aux_i s prob i =
    let particle = prob.particles.(i) in
    prob.particles.(i) <- { particle with score = s +. particle.score }
  
  
    let sample d k prob =
    let v = Distribution.draw d in
    let s = (Distribution.logpdf d v) in
    prob.particles.(prob.id).control <- 
      Array.append prob.particles.(prob.id).control
      [|{d;s;cont_k=k}|];
    k v prob

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)

  (* returns true with probability "p" *)
  let decide p = Random.float 1. <= p 

  let run_next prob =
    
    if prob.id < Array.length prob.particles - 1 then begin
      
      let k = prob.particles.(prob.id + 1).k in
      
      if prob.id >= 1 
      then begin 
        let rdm = Random.int (Array.length prob.particles.(prob.id-1).control) in
        
        let ctrl = prob.particles.(prob.id-1).control.(rdm) in
        prob.particles.(prob.id).score <- 0.;
        prob.particles.(prob.id).control <- (Array.sub prob.particles.(prob.id-1).control 0 rdm );
      
        let v = (Distribution.draw ctrl.d) in
        let s = (Distribution.logpdf ctrl.d v) in
     
        prob.particles.(prob.id).control <- Array.append prob.particles.(prob.id).control [| {d=ctrl.d ; s=s;cont_k=ctrl.cont_k} |];

        let prob = (ctrl.cont_k (Distribution.draw ctrl.d) prob) in

        if not ( prob.particles.(prob.id).score >= prob.particles.(prob.id-1).score
        || (decide (prob.particles.(prob.id).score /. prob.particles.(prob.id-1).score)))
        then begin (* REJECT *)
          prob.particles.(prob.id).score <- prob.particles.(prob.id-1).score ;
          prob.particles.(prob.id).value <- prob.particles.(prob.id-1).value ;
        end;

        prob.particles.(prob.id+1).control <- [||];
        k { prob with id = prob.id + 1  }
      
      end else begin        
      
        prob.particles.(prob.id+1).control <- [||];
        k { prob with id = prob.id + 1  }
      end
    end
    else prob

  let exit v prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with value = Some v };
    if prob.id >= 0 
      then begin 
        (* Add all "sample" values (logits) to score *)
        Array.iteri (fun _i {s;_} -> factor_aux s prob) prob.particles.(prob.id).control;
      end;
     prob
    

  let infer ?(n = 1000) model data =
    let init_particle = { value = None; score = 0.; k = (model data) exit; control=[||] } in
    let prob = { id = -1; particles = Array.make n init_particle } in
    
    let rec exec i prob = 
      if i=0 then prob
      else exec (i-1) (run_next prob)
    in

    let prob = exec 1000 prob in

    let values = Array.map (fun p -> Option.get p.value) prob.particles in
    let logits = Array.map (fun p -> p.score) prob.particles in
    Distribution.support ~values ~logits
end


module Importance_sampling = struct
  type 'a prob = { id : int; particles : 'a particle array }
  and 'a particle = { value : 'a option; score : float; k : 'a next }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob =
    let v = Distribution.draw d in
    k v prob

  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with score = s +. particle.score };
    k () prob

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)

  let run_next prob =
    if prob.id < Array.length prob.particles - 1 then
      let k = prob.particles.(prob.id + 1).k in
      k { prob with id = prob.id + 1 }
    else prob

  let exit v prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- { particle with value = Some v };
    run_next prob

  let infer ?(n = 1000) model data =
    let init_particle = { value = None; score = 0.; k = (model data) exit } in
    let prob = { id = -1; particles = Array.make n init_particle } in
    let prob = run_next prob in
    let values = Array.map (fun p -> Option.get p.value) prob.particles in
    let logits = Array.map (fun p -> p.score) prob.particles in
    Distribution.support ~values ~logits
end

module Particle_filter = struct
  include Importance_sampling

  let resample particles =
    let logits = Array.map (fun x -> x.score) particles in
    let values = Array.map (fun x -> { x with score = 0. }) particles in
    let dist = Distribution.support ~values ~logits in
    Array.init (Array.length particles) (fun _ -> Distribution.draw dist)

  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <-
      { particle with k = k (); score = s +. particle.score };
    let prob =
      if prob.id < Array.length prob.particles - 1 then prob
      else { id = -1; particles = resample prob.particles }
    in
    run_next prob

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)
end
