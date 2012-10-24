(*
 * File: mp7-skeleton.ml

When I code this homework, I carefully read mp6 of fall 2011. Part of the code, though
not directly copied from mp6, has similar structures and implimentations as mp6 of fall 2011

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
    match subst with [] -> TyVar m
    | (a,b) :: rest -> if a = m then b else subst_fun rest m

(* Problem 3 *)
let rec monoTy_lift_subst subst monoTy =
    match monoTy
    with TyVar m -> subst_fun subst m
    | TyConst(n, typel) -> TyConst(n, List.map (monoTy_lift_subst subst) typel)

(* Problem 4 *)
let rec occurs x ty =
    match ty with TyVar m -> x = m
    | TyConst(n, typel) -> List.exists (occurs x) typel

(* Problem 5 *)
let rec unify eqlst : substitution option =
  let rec newAdd l1 l2 m =
    match l1,l2 with [],[] -> Some m
    | h::tail, h'::tail' -> newAdd tail tail' ((h,h')::m)
    | _ -> None
  in
  match eqlst with [] -> Some([])
  | (s,t)::resteq ->
    if s = t then unify resteq
    else (match (s,t) 
          with (TyConst(c, tl), TyConst(c', tl')) ->
            if c=c' then (match (newAdd tl tl' resteq) with None -> None | Some l -> unify l)
            else None
          | (TyConst(c, tl), TyVar(m)) -> unify ((TyVar(m), TyConst(c, tl))::resteq)
          | (TyVar(n),t) ->
             if (occurs n t)
             then None
             else let resteq' =
                      List.map
                      (fun (t1,t2) ->
                           (monoTy_lift_subst [(n,t)] t1, monoTy_lift_subst [(n,t)] t2))
                      resteq
                   in (match unify resteq'
                       with None -> None
                       | Some(phi) -> Some((n, monoTy_lift_subst phi t):: phi)))

(* Problem 6 *)

let rec first p l =
    match l with [] -> None
    | (x :: xs) -> if p x then Some x else first p xs

let canonicalize ty =
let rec canon_type subst m ty =
   match ty
   with TyVar n ->
     (match first (fun p -> fst p = n) subst
      with Some (j,k) -> (subst, m, TyVar k)
      | None -> ((n,m)::subst), m+1, TyVar m)
   | TyConst (c, tys) ->
     (match
       List.fold_left
       (fun (subst, n, tyl) -> fun ty ->
        (match canon_type subst n ty
         with (subst', n', ty') -> (subst', n', ty'::tyl)))
       (subst, m, [])
       tys
      with (new_subst, new_m, new_tys) -> (new_subst, new_m, TyConst(c, List.rev new_tys)))
  in let (_, _, c_ty) = canon_type [] 0 ty in c_ty

let equiv_types ty1 ty2 =
   let new_ty1 = canonicalize ty1 in
   let new_ty2 = canonicalize ty2 in new_ty1 = new_ty2;;
