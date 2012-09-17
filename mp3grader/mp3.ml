(* CS 421 Fall 2012 MP3 *)

open Mp3common


(* Problem 1 *)
let rec split f lst =
  let combine (x1,y1) (x2,y2) = (x1@x2,y1@y2) in
    match lst with [] -> ([],[])
    | (l1::lx) -> if f l1 then combine ([l1],[]) (split f lx)
                  else combine ([],[l1]) (split f lx)

(* Problem 2 *)
let rec rle lst =
  let rec flip lst = match lst with [] -> [] | (l1::lx) -> flip lx @ [l1] in
  let inside x (y,z) = if x=y then true else false in
  let addone (y,z) = (y,z+1) in
  let addToResult x r =
    match r with [] -> [(x,1)]
    | (r1::rx) -> if inside x r1 then [(addone r1)]@rx
                  else [(x,1)] @ r
  in
  let rec rle_a lst result = 
    match lst with [] -> result
    | (l1::lx) -> rle_a lx (addToResult l1 result)
  in
    flip (rle_a lst [])
  

(* Problem 3 *)
let rec sub_list l1 l2 =
  match l1 with [] -> if l2 = [] then true else false
  | (x::xs) -> match l2 with [] -> true | (y::ys) ->
               ((x=y)&&(sub_list xs ys))|| (sub_list xs l2)
    
(* alternately 
let rec sub_list l1 =
  raise(Failure "Function not implemented yet.")*)

(* Problem 4 *)
let rec concat s list =
  match list with [] -> ""
  | (x::xs) -> if xs = [] then x else x ^ s ^ (concat s xs)

(* Problem 5 *)
let split_base = ([],[])
let split_step f first (yes, no) = 
  if f first then ([first]@yes,no) else (yes, ([first]@no))

(* List.fold_right (split_step (fun x -> x > 2)) [0;2;3;5;1;4] split_base;; *)
  

(* Problem 6 *)
let rle2 lst =
  let rle_step x ls = 
    match ls with [] -> [(x,1)]
    | (l1::lx) -> if (fun x (y,z) -> (x=y) ) x l1 
                  then [( (fun (x,y) -> (x,y+1)) l1 )]@lx
                  else [(x,1)]@ls
  in
  List.fold_right rle_step lst []

(* Problem 7 *)
let concat2 s list =
  let concat_step str first = 
    if str = "" then first else str ^ s ^ first
  in
  List.fold_left concat_step "" list

(* Problem 8 *)
let app_all fs list = 
  let app_all_help ls = List.map (fun f -> f ls) fs in
  List.map app_all_help list

(* Extra Credit, Problem 9 *)
let sub_list2 l1 l2 =
  raise(Failure "Function not implemented yet.")
  (*
  let findFirstLoc ls x = 
    (fun x -> match x with [] -> ([],false) | (x::xs) -> (xs,true)) (
      List.fold_left (fun l a -> if a = x then 
                      if l = [] then [a] else l @ [a]
                      else
                      if l = [] then [] else l @ [a] ) [] ls )
  in
  let testFromFirstLoc ls = 
    List.fold_left (fun ls a -> 
                  if ls = ([],false) || findFirstLoc ls a = ([],false) then ([],false)
                  else (findFirstLoc ls a,true)
                  ) (ls,true) l2
  in
  List.fold_left (fun no a ->  ) false l1 *)
