(*
 * File: mp7-skeleton.ml

When I code this homework, I carefully read mp6 of fall 2011. Part of the code, though
not directaily copied from mp6, has similar structures and implimentations as mp6 of fall 2011

 Reference:
  http://courses.engr.illinois.edu/cs421/fa2011/mps/MP6/


 *)

open Mp7common

(* Problem 1 *)
let asMonoTy1 () = 
  mk_fun_ty bool_ty (mk_list_ty int_ty);;
let asMonoTy2 () =
    mk_fun_ty (fresh()) (mk_fun_ty (fresh()) (mk_fun_ty (fresh()) (fresh())));;
let asMonoTy3 () = 
  mk_fun_ty (fresh()) (mk_list_ty (mk_pair_ty (fresh()) int_ty));;
let asMonoTy4 () = 
  mk_pair_ty string_ty (mk_fun_ty (mk_list_ty (fresh())) (fresh()));;

(* Problem 2 *)
let rec subst_fun subst m =
    match subst with 
    | [] -> TyVar m
    | (a,b) :: rest -> if a = m then b else subst_fun rest m

(* Problem 3 *)
let rec monoTy_lift_subst subst monoTy =
    match monoTy with 
    | TyVar m -> subst_fun subst m
    | TyConst(n, typel) -> TyConst(n, List.map (monoTy_lift_subst subst) typel)

(* Problem 4 *)
let rec occurs x ty =
    match ty with 
    | TyVar m -> x = m
    | TyConst(n, typel) -> List.exists (occurs x) typel

(* Problem 5 *)
let rec unify eqlst : substitution option =
  let rec newAdd l1 l2 m = match l1,l2 with 
      | [],[] -> Some m
      | h::tail, h'::tail' -> newAdd tail tail' ((h,h')::m)
      | _ -> None
  in
  match eqlst with 
  | [] -> Some([])
  | (s,t)::resteq ->
    if s = t then unify resteq
    else (match (s,t) 
          with 
          | (TyVar(n),t) -> if (occurs n t) then None
             else let newresteq = List.map
                (fun (x1,x2) -> 
                  (monoTy_lift_subst [(n,t)] x1, monoTy_lift_subst [(n,t)] x2)) 
                resteq in 
                (match unify newresteq with None -> None
                      | Some(p) -> Some((n, monoTy_lift_subst p t):: p))
          | (TyConst(h, tail), TyConst(h', tail')) -> 
              if h=h' then 
                (match (newAdd tail tail' resteq) with 
                      | None -> None 
                      | Some x -> unify x)
              else None
          | (TyConst(h, tail), TyVar(m)) -> 
              unify ((TyVar(m), TyConst(h, tail))::resteq)
            )

(* Problem 6 *)

let helpcanon inputty =
  let rec findfirst p l = match l with [] -> None
    | (x :: xs) -> if p x then Some x else findfirst p xs in
let rec rec_canon su m ty =
   match ty with 
   | TyVar n -> (match findfirst (fun p -> fst p = n) su with 
            | Some (l1,l2) -> (su, m, TyVar k)
            | None -> ((n,m)::su), m+1, TyVar m)
   | TyConst (c, tys) ->
     (match List.fold_left (fun (su, n, tyl) -> fun ty -> 
      (match rec_canon su n ty with (nsu, nn, nty) -> (nsu, nn, nty::tyl)))
       (su, m, []) tys
      with  (n_su, n_m, n_tys) -> (n_su, n_m, TyConst(c, List.rev n_tys)))
  in let (_, _, tty) = rec_canon [] 0 inputty in tty

let equiv_types ty1 ty2 =
   let ty1' = helpcanon ty1 in
   let ty2' = helpcanon ty2 in ty1' = ty2';;
