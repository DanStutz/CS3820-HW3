(*
   CS:3820 Programing Language Concepts

   Homework 3

   Team:  <Daniel Stutz, Kane Templeton>
*)


(* Part 1 *)

type oper1 = Neg | Not
type oper2 = Add | Mul | Sub | Gt | Eq | And

type expr =
  | Cst of int
  | OpUna of oper1 * expr
  | OpBin of oper2 * expr * expr
  | IfElse of expr * expr * expr

// drop : int -> 'a list -> 'a list
let rec drop i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::drop (i - 1) xs
    | i, [] -> failwith "index out of range"
    
 // Drop test cases
drop(2)


// size : expr -> int



// subexpressions : expr -> expr list






(* Part 2 *)

type sInstr =
  | SCst of int
  | SAdd
  | SSub
  | SMul
  | SNeg
  | SGt
  | SIfze of int
  | SJump of int

let rec eval (e : expr) : int =
  match e with
    | Cst n                -> n
    | IfElse (e1, e2, e3)  -> if (eval e1) = 0 then eval e3 else eval e2
    | OpUna (Neg, e1)      -> -(eval e1)
    | OpUna (Not, e1)      -> if (eval e1) = 0 then 1 else 0
    | OpBin (Add, e1, e2)  -> (eval e1) + (eval e2)
    | OpBin (Mul, e1, e2)  -> (eval e1) * (eval e2)
    | OpBin (Sub, e1, e2)  -> (eval e1) - (eval e2)
    | OpBin (Gt, e1, e2)   -> if (eval e1) > (eval e2) then 1 else 0
    | OpBin (Eq, e1, e2)   -> if (eval e1) = (eval e2) then 1 else 0
    | OpBin (And, e1, e2)  -> if (eval e1) = 0 || (eval e2) = 0 then 0 else 1


// scomp : expr -> sInstr list



// seval : sInstr list -> int list -> int



// run : sInstr list -> int
let run (p : sInstr list) : int = eval p []


// byteCode : sInstr list -> string



// beval : int list -> int list -> int



// parse : string -> int list
let parse (p : string) : int list =
  let l = Seq.toList (p.Split ' ') in
  List.map System.Int32.Parse l


(* Part 3 *)


//  1. (0*10*10*) + (0*10*) + (0*)

(*

    2. The regex expression which captures all numbers in exponential notation is: d(.) d^+ (E + e) (- + 2)d

    Example: Say d represents a numeral,
      
      d^+ = d* - 2

      d = 0,1,2,3,...,9
    
    3. (L + D)*(LD + DL)(L + D)*

    The outer two parts generate any combination of L and D in all order while the middle part generates at least one L and one D in two orders.
*)


