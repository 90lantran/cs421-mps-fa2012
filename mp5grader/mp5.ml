(* File: mp5-skeleton.ml *)

open Mp5common

(* Problem 1 *)
let rec import_list lst =
	List.fold_right (fun (a,i) l -> 
		BinOpAppExp(ConsOp, BinOpAppExp(CommaOp,ConstExp(StringConst a),ConstExp(IntConst i)), l)) 
		lst (ConstExp NilConst) ;;


(* Problem 2 *)
let list_all =  
	Rec("list_all","",FnExp("p",FnExp("xs",
		IfExp(BinOpAppExp(EqOp,VarExp "xs",ConstExp NilConst),
	         		ConstExp(BoolConst true),
	               IfExp(AppExp(VarExp "p", MonOpAppExp(HdOp,VarExp "xs")),
	               		IfExp(AppExp(AppExp(VarExp "list_all",VarExp "p"),MonOpAppExp(TlOp,VarExp "xs")),
	               			ConstExp(BoolConst true),
	               			ConstExp(BoolConst false)),
	               		ConstExp(BoolConst false))
	           )
		)))


(* Problem 3 *)
let max a b = if a >=b then a else b 
let max2 f a b = max (f a) (f b)
let max3 f a b c = max (max (f a) (f b)) (f c)
let rec cal_max_exp_height exp = 
	match exp
    with VarExp _ -> 1
       | ConstExp _ -> 1
       | MonOpAppExp(mon_op, mon_exp) -> 1 + (cal_max_exp_height mon_exp)
       | BinOpAppExp(bin_op,exp1,exp2) -> 1 + max2 cal_max_exp_height exp1 exp2
       | IfExp(exp1,exp2,exp3) -> 1 + max3 cal_max_exp_height exp1 exp2 exp3
       | AppExp(exp1,exp2) -> 1 + max2 cal_max_exp_height exp1 exp2
       | FnExp(str, exp1)-> 1 + cal_max_exp_height exp1
       | LetExp(dec1,exp1) -> 1+ max (cal_max_dec_height dec1) (cal_max_exp_height exp1)
and cal_max_dec_height dec = 
	match dec with
	Val(str,exp) -> 1 + cal_max_exp_height exp 
	| Rec(str1, str2,exp) -> cal_max_exp_height exp 
	| Seq(dec1, dec2) -> max2 cal_max_dec_height dec1 dec2


(* Problem 4 *)
let rec union l1 l2 = 
	let addL x l =
		if List.fold_right (fun a bo -> bo || (x=a) ) l false then l else l @ [x]
	in
	match l2 with
	[] -> l1
	| head::tail -> union (addL head l1) tail

let rec setminus l1 l2 = 
	let minusL l x =
		List.fold_right (fun a rl -> if a = x then rl else a::rl) l []
	in
	match l2 with
	[] -> l1
	| head::tail -> setminus (minusL l1 head) tail

let rec union_L f l = 
	match l with [] -> [] 
	| head::tail -> union (f head) (union_L f tail)

let rec freeVarsInExp exp = 
	match exp with
	VarExp(str) -> [str]
	| ConstExp _ -> []
	| MonOpAppExp(mon_op, exp1) -> union_L freeVarsInExp [exp1]
	| IfExp(exp1,exp2,exp3) -> union_L freeVarsInExp [exp1;exp2;exp3]
	| BinOpAppExp(bin_op,exp1,exp2) -> union_L freeVarsInExp [exp1;exp2]
	| AppExp(exp1,exp2) -> union_L freeVarsInExp [exp1;exp2]
	| FnExp(str, exp1) -> setminus (freeVarsInExp exp1) [str] 
	| LetExp(dec1,exp1) -> let (v,b) = freeAndBindingVarsInDec dec1 in
							union v (setminus (freeVarsInExp exp1) b)
and freeAndBindingVarsInDec dec =
	match dec with
	Val(str,exp) -> (freeVarsInExp exp,[str])
	| Rec(str1, str2, exp) -> (setminus (freeVarsInExp exp) [str1;str2],[str2])
	| Seq(dec1, dec2) -> let (v1,b1) = freeAndBindingVarsInDec dec1 in
						let (v2,b2) = freeAndBindingVarsInDec dec2 in
						(union v1 (setminus v2 b1),union b1 b2)

(* Problem 5 *)
let rec cps_exp e k kx =  raise (Failure "Not implemented yet.")

and cps_dec dec ecps kx =  raise (Failure "Not implemented yet.")
