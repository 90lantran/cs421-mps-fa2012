(*
 * grader for mp1
 * This file will be preprocessed to generate the actual OCaml file.
 *)
let rubric_version = "1.4"
let rubric_title = "CS421 Fall 2007 MP1"

open Grader
open Test
open Mp1common

(*
 * use a timeout of 4 seconds
 *)
let outputOk () =
  try (
  let len = String.length !output
  in let half1 = String.sub !output 0 (len / 2)
     and half2 = String.sub !output (len / 2) (len / 2)
     in half1=half2
  ) with e -> false

(* let len = List.length !output
  in if (len mod 2) != 0 then false else
    let rec aux slist half1 half2 idx =
      if idx = (len / 2) then half1 = half2
      else match slist with
           | x::y -> aux y (half1@[x]) y (idx + 1)
    in aux !output [] [] 0
*)


let isEq i j =
  (i = j) && (let res = outputOk() in output := ""; res)

let mp1test weight pair = compare isEq 4 weight pair

(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)

(* This list is for regular problems *)
let rubric =
[
     "greetings", mp1test 1 (ss_pair0 Solution.greetings Student.greetings) ;
     "pi", mp1test 1 (ss_pair0 Solution.pi Student.pi) ;
     "square"^" "^"7", mp1test 1 (ss_pair1 Solution.square Student.square 7) ;
     "plus_pi_times_3"^" "^"23.17", mp1test 1 (ss_pair1 Solution.plus_pi_times_3 Student.plus_pi_times_3 23.17) ;
     "salute"^" "^"\"Malisa\"", mp1test 1 (ss_pair1 Solution.salute Student.salute "Malisa") ;
     "has_smallest_square"^" "^"4"^" "^"6", mp1test 1 (ss_pair2 Solution.has_smallest_square Student.has_smallest_square 4 6) ;
     "pivot"^" "^"(3, \"hi\")", mp1test 1 (ss_pair1 Solution.pivot Student.pivot (3, "hi")) ;
     "app_pair"^" "^"(7,9)"^" "^"Solution.square", mp1test 1 (ss_pair2 Solution.app_pair Student.app_pair (7,9) Solution.square)
]
(* Note: the last entry should not be followed by a semicolon. *)

let extra_rubric = [ ]

let _ = Main.main rubric extra_rubric rubric_title rubric_version
