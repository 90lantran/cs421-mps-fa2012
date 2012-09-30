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
let rec cal_max_exp_height exp = 

and cal_max_dec_height dec = 

(* Problem 4 *)
let rec freeVarsInExp exp = raise (Failure "Not implemented yet")

and freeAndBindingVarsInDec dec = raise (Failure "Not implemented yet")

(* Problem 5 *)
let rec cps_exp e k kx =  raise (Failure "Not implemented yet.")

and cps_dec dec ecps kx =  raise (Failure "Not implemented yet.")
