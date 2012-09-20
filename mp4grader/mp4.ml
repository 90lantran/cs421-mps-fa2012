open Mp4common

(* Problem 1 *)
let addk i j k =k (i+j)
let subk i j k =k (i-j)
let timesk i j k =k (i*j)
let plusk m n k =k (m+.n)
let take_awayk m n k =k (m-.n)
let multk m n k =k (m*.n)
let catk s t k =k (s^t)
let consk x l k =k (x :: l)
let lessk a b k =k (a < b)
let eqk x y k =k (x=y)

(* Problem 2 *)
let abcdk a b c d k =
  multk b c (fun bc -> plusk a bc ((fun abc -> 
      multk a d (fun ad -> plusk abc ad k)
  )));;
(*
  let bc = multk b c (fun x -> x) in
    let da = multk d a (fun x -> x) in
      let abc = plusk a bc (fun x -> x) in
        plusk abc da k;;
*)

(* Problem 3 *)
let rec fact_range n m = if n < m then 1 else n * (fact_range (n-1) m)

let rec fact_rangek n m k = 
  lessk n m (fun b -> if b then k 1 else 
          fact_rangek (n-1) m (fun x -> timesk x n k)
  )

(* Problem 4 *)
let duplicate_evens l = 
  let rec duplicate_evens_help l n = 
    match l with [] -> []
    | head::tail -> if n = 0 then head::(duplicate_evens_help tail 1) else
                    [head;head] @ (duplicate_evens_help tail 0)
  in
  duplicate_evens_help l 0 ;;

let duplicate_evensk l k = 
  let rec help l n k = 
    match l with [] -> k []
    | head::tail -> 
    eqk n 0 (fun b-> if b then help tail 1 (fun x -> consk head x k ) else
              help tail 0 (fun x ->  consk head x (fun t -> consk head t k) )
    )
  in help l 0 k;;

(* Problem 5 *)
let rec first_or_default p l d = 
  match l with [] -> d
  | head::tail -> if p head then head else first_or_default p tail d;;

let rec first_or_defaultk p l d k = 
  match l with [] -> k d
  | head::tail -> 
    p head (fun x -> if x then k head else 
                    first_or_defaultk p tail d k
    );;


(* Problem 6 *)
let rec app_all flst x =  
  match flst with [] -> []
  | head::tail -> [head x] @ app_all tail x;;

let rec app_allk flstk x k =
  match flstk with [] -> k []
  | head::tail -> head x (fun y -> 
        app_allk tail x (fun t -> consk y t k ) 
  );;

(* Problem 7 *)
let rec sum_wholesk l k xk = 
  match l with [] -> k 0
  | head::tail -> 
    lessk head 0 (fun b -> if b then xk head else
                sum_wholesk tail (fun y -> addk y head k ) xk
 )

    



(* Problem 8 *)
let dividek a b k = k (a/.b)
let rec sum_list l k = 
  match l with [] -> k 0.0
  | head::tail -> sum_list tail (fun y -> plusk head y k)

let rec lengthk l k = 
  match l with [] -> k 0.0
  | head::tail -> lengthk tail (fun y -> plusk y 1.0 k)

let averagek l xk k = 
  eqk l [] (fun b -> if b then xk() else
    sum_list l (fun s -> lengthk l (fun len -> dividek s len k) )
  ) 

let average_averagek ll k empty_list_list_xk empty_list_in_list_xk =
    eqk ll [] (fun b -> if b then empty_list_list_xk() else
      let rec a_list ll n k = 
        match ll with [] -> k []
        | head::tail -> 
          averagek head (fun ()-> empty_list_in_list_xk n) (fun h -> 
                a_list tail (n+1) (fun r -> consk h r k))
      in
      a_list ll 0 (fun r -> averagek r empty_list_list_xk k)
    )


    average_averagek [[1.; 2.; 3.]; [4.; 5.]; [6.; 7.; 8.]; [9.]]
(fun a -> print_string "Result: "; print_float a; print_newline())
(fun () -> print_string "Empty list!"; print_newline())
(fun n -> print_string "Empty list at position ";
print_int n; print_newline());;
