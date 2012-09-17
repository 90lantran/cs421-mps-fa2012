(*
 * This file will be preprocessed to generate the actual OCaml file.
 *)
open Grader
open Test

(*
 * use a timeout of 4 seconds
 *)

let mptest weight pair = compare (=) 4 weight pair

let rubric_version = "1.1"
(* Added some 0 pt tests for polymorphism checks *)
let rubric_title = "CS421 Fall 2012 MP2"
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
     "com_mul"^" "^"((4.,0.),(2.,3.))", mptest 1 (ss_pair1 Solution.com_mul Student.com_mul ((4.,0.),(2.,3.)));

     "s"^" "^"5", mptest 1 (ss_pair1 Solution.s Student.s 5);

     "list_all"^" "^"(fun x -> x < 0)"^" "^"[1;-1;0;4;-2;5]", mptest 1 (ss_pair2 Solution.list_all Student.list_all (fun x -> x < 0) [1;-1;0;4;-2;5]);
     "list_all"^" "^"(fun x -> x < 0.)"^" "^"[-1.;2.;3.]", mptest 1 (ss_pair2 Solution.list_all Student.list_all (fun x -> x < 0.) [-1.;2.;3.]);

     "is_less"^" "^"5"^" "^"[1;34;42;6]", mptest 1 (ss_pair2 Solution.is_less Student.is_less 5 [1;34;42;6]);
     "is_less"^" "^"5."^" "^"[1.;34.;42.;6.]", mptest 1 (ss_pair2 Solution.is_less Student.is_less 5. [1.;34.;42.;6.]);

     "interleave"^" "^"[1;2;5]"^" "^"[3;4]", mptest 1 (ss_pair2 Solution.interleave Student.interleave [1;2;5] [3;4]);
     "interleave"^" "^"[()]"^" "^"[()]", mptest 0 (ss_pair2 Solution.interleave Student.interleave [()] [()]);


     "separate"^" "^"[1; 3; 2; 4; 5]", mptest 1 (ss_pair1 Solution.separate Student.separate [1; 3; 2; 4; 5]);
     "separate"^" "^"[(); ()]", mptest 0 (ss_pair1 Solution.separate Student.separate [(); ()]);

     "odds"^" "^"[3;7;9]", mptest 1 (ss_pair1 Solution.odds Student.odds [3;7;9]);

     "check_adj"^" "^"[[1;2;3;4];[3;0;4;5];[1;4;3;5];[2;1];[1;2];[2;3;4]]"^" "^"(0,3)", mptest 1 (ss_pair2 Solution.check_adj Student.check_adj [[1;2;3;4];[3;0;4;5];[1;4;3;5];[2;1];[1;2];[2;3;4]] (0,3))
]
(* Note: the last entry should not be followed by a semicolon. *)

let extra_rubric =
[
     "check_path"^" "^"[[1;2;3;4];[3;0;4;5];[1;4;3;5];[2;1];[1;2];[2;3;4]]"^" "^"(0,5)", mptest 1 (ss_pair2 Solution.check_path Student.check_path [[1;2;3;4];[3;0;4;5];[1;4;3;5];[2;1];[1;2];[2;3;4]] (0,5))
]

let _ = Main.main rubric extra_rubric rubric_title rubric_version
