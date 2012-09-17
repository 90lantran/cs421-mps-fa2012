(* CS421 - Fall 2011
 * MP1 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp1common

(* Problem 1 *)
let greetings = "Hi there."

(* Problem 2 *)
let pi = 3.1415926  (* Agian, you will want to change this. *)

(* Problem 3 *)
let square n = n*n

(* Problem 4 *)
let plus_pi_times_3 y = (y+.pi)*.3.0

(* Problem 5 *)
let salute name =
  if name = "Elsa" then print_string "Halt! Who goes there!\n"
  else print_string ("Hail, " ^ name ^ ". We warmly welcome you!\n")

(* Problem 6 *)
let has_smallest_square m n =
	if m*m < n*n then m
	else if m*m > n*n then n
	else
		if m < n then m else n

(* Problem 7 *)
let pivot (x,y) = (x,y,x);;

(*Problem 8 *)
let app_pair (x,y) f = (f x, f y)
