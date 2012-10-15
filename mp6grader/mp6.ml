(* File: mp6-sol.ml *)

open Mp6common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp x ->
      (match lookup_env gamma x
       with None -> None
       | Some x_ty ->
	       (match unify [(tau, freshInstance(x_ty))]
	        with None -> None
	       | Some sigma -> Some(Proof([],judgment),sigma)))
    | BinOpAppExp (binop, e1, e2) ->
       let (tau1, tau2) = (fresh(),fresh()) in
       let tau' = binop_signature binop in
       (match (gather_exp_ty_substitution gamma e1 tau1)
        with None -> None
        | Some (proof1, sigma1) ->
          (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) e2 tau2
           with None -> None
           | Some (proof2, sigma2) ->
             let sigma21 = subst_compose sigma2 sigma1 in
             (match unify [(monoTy_lift_subst sigma21
                            (mk_fun_ty tau1 (mk_fun_ty tau2 tau)),
                           freshInstance tau')]
              with None -> None
              | Some sigma ->
                Some (Proof([proof1; proof2],judgment),subst_compose sigma sigma21))))
    | MonOpAppExp (monop, e1) ->
      let tau1 = fresh() in
      let tau' = monop_signature monop in
      (match gather_exp_ty_substitution gamma e1 tau1
       with None -> None
       | Some (proof, sigma) ->
         (match unify [(monoTy_lift_subst sigma (mk_fun_ty tau1 tau),
                        freshInstance(tau'))] with None -> None
              | Some unify_subst -> 
                Some (Proof([proof],judgment),subst_compose unify_subst sigma)
                ))
     | IfExp (bool_exp, then_exp, else_exp) ->
      (match
        gather_exp_ty_substitution gamma bool_exp bool_ty
        with None -> None
        | Some (bool_pf,sigma1) -> 
          (match
           gather_exp_ty_substitution (env_lift_subst sigma1 gamma) then_exp (monoTy_lift_subst sigma1 tau)
           with None -> None
           | Some (then_pf, sigma2) ->
             (let sigma21 = subst_compose sigma2 sigma1 in
              match
              gather_exp_ty_substitution (env_lift_subst sigma21 gamma)
              						else_exp (monoTy_lift_subst sigma21 tau)
              with None -> None
              | Some (else_pf, sigma3) ->
                Some(Proof([bool_pf; then_pf; else_pf],judgment), (subst_compose sigma3 sigma21)))
                ))
	 | AppExp (e1, e2) ->
      let tau1 = fresh() in
      (match
       gather_exp_ty_substitution gamma e1 (mk_fun_ty tau1 tau)
       with None -> None
       | Some (e1_pf, sigma1) ->
         (match 
          gather_exp_ty_substitution (env_lift_subst sigma1 gamma)
                                  e2 (monoTy_lift_subst sigma1 tau1)
          with None -> None
          | Some(e2_pf, sigma2) ->
          	Some(Proof([e1_pf; e2_pf],judgment),(subst_compose sigma2 sigma1))))
	| FnExp (x,e) ->
      let (tau1, tau2) = (fresh(), fresh()) in
      (match gather_exp_ty_substitution (ins_env gamma x ([],tau1)) e tau2
       with None -> None
       | Some (pf, sigma) ->
         (match unify[(monoTy_lift_subst sigma tau,
                       monoTy_lift_subst sigma (mk_fun_ty tau1 tau2))]
          with None -> None
          | Some unify_subst ->
         	Some(Proof([pf],judgment), (subst_compose unify_subst sigma))
            ))
    | RaiseExp e ->
      (match gather_exp_ty_substitution gamma e int_ty
       with None -> None
       | Some (pf, sigma) -> 
       		Some(Proof([pf],judgment), sigma))
    | LetExp (dec, e) ->
    	(match gather_dec_ty_substitution gamma dec 
    	with None -> None
    	| Some (e1_pf, newGamma, sigma1) ->
    		(match gather_exp_ty_substitution newGamma e (monoTy_lift_subst sigma1 tau)
    		with None -> None
    		| Some(e2_pf, sigma2) ->
    			Some(Proof([e1_pf; e2_pf],judgment), (subst_compose sigma2 sigma1))))
    | _ -> raise (Failure "Not implemented yet")

and gather_dec_ty_substitution gamma dec = 
	match dec
	with Val(x,e) ->
		let tau1 = fresh() in
      (match gather_exp_ty_substitution gamma e tau1
       with None -> None
       | Some(e_pf, sigma1) ->
         let sigma1_gamma = env_lift_subst sigma1 gamma in
         let x_ty = gen sigma1_gamma (monoTy_lift_subst sigma1 tau1) in
         let newGamma = (ins_env sigma1_gamma x x_ty) in
         let judgment = DecJudgment(gamma,dec,newGamma) in
         Some(Proof([e_pf],judgment), newGamma ,sigma1))
	| Rec(f,x,e) ->
		let (tau1, tau2) = (fresh(),fresh()) in
      (match gather_exp_ty_substitution
              (ins_env (ins_env gamma x ([],tau1)) f ([],(mk_fun_ty tau1 tau2)))
              e tau2
       with None -> None
       | Some (e1_pf, sigma1) ->
         (let sigma1_gamma = env_lift_subst sigma1 gamma in
          let f_ty =
            gen sigma1_gamma (monoTy_lift_subst sigma1 (mk_fun_ty tau1 tau2)) in
          let newGamma = (ins_env sigma1_gamma f f_ty) in
          let judgment = DecJudgment(gamma,dec,newGamma) in
          Some(Proof([e1_pf],judgment), newGamma ,sigma1)))
	| Seq(dec1,dec2) ->
		(match gather_dec_ty_substitution gamma dec1
		with None -> None
		| Some (e1_pf, newGamma, sigma1) ->
			(match gather_dec_ty_substitution newGamma dec2 with 
			None -> None
			| Some(e2_pf,newGamma2,sigma2) ->
				let judgment = DecJudgment(gamma,dec2,newGamma2) in
				Some(Proof([e1_pf;e2_pf],judgment), newGamma2 ,(subst_compose sigma2 sigma1))
			))
	| Local(dec1,dec2) -> raise (Failure "Not implemented yet")
