(* CS421 - Fall 2012
 * MP2 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(* Problem 1 *)
let com_mul ((x1,y1),(x2,y2)) = 
 	((x1 *. x2 -. y1 *. y2), (x1 *. y2 +. y1*.x2))

(* Problem 2 *)
let rec s n =
  if n <= 0 then 0
else if n == 1 then 1
else if n mod 2 == 0 then 2*s(n-2) + 2*n
else 3*s(n-2)+3*n

(* Problem 3 *)
let rec list_all p xs =
	match xs with [] -> true
	| (x::xs) -> p(x) & list_all p xs
  

(* Problem 4 *)
let rec is_less x xs =
	match xs with [] -> true
	| (y::xs) -> (x < y) & is_less x xs;;

(* Problem 5 *)
let rec interleave xs ys =
	match xs with [] -> ys
	| (x1::xs) -> (x1:: interleave ys xs );;



(* Problem 6 *)
let combine (x1,y1) (x2,y2) = ((x1 @ x2),(y1 @ y2))


let rec separate xs = 
	let rec sep xs n = 
		match xs with [] -> ([],[])
		| (x::xs) -> if n=0 then combine ([x],[]) (sep xs 1) 
					 else combine ([],[x]) (sep xs 0)
	in sep xs 0

(* Problem 7 *)
let rec sum xs = 
	match xs with [] -> 0
	| (x::xs) -> x + (sum xs)
let rec odds_sum xs n = 
	match xs with [] -> []
	| (x::xs) -> [(x,n)] @ odds_sum xs n 
let rec odds xs =
	odds_sum xs (sum xs)

(* Problem 8 *)
let rec neighbor a_l a = 
	match a_l with [] -> []
	| (x::xs) -> if a = 0 then x else if a < 0 then [] else neighbor xs (a-1)

let rec check_adj_nei adj_list (s_list,b) nei_set = 
	if List.exists (fun x -> x = b) s_list && List.length s_list > 1 then true
	else 
		match nei_set with [] -> false
		| (h::xs) -> if h = b then true
					else
						if List.exists (fun x -> x = h) s_list then
						check_adj_nei adj_list (s_list,b) xs
					 	else
					 	check_adj_nei adj_list (s_list,b) xs || (check_adj_nei adj_list ((h::s_list),b) (neighbor adj_list h))

let rec check_adj adj_list (a,b) =
	List.exists (fun x -> x = b) (neighbor adj_list a)
	


(* Extra credit, Problem 9 *)
let rec check_path adj_list (a,b) = 
	if a<0 || b<0 then false
	else check_adj_nei adj_list ([a],b) (neighbor adj_list a) 
  
