(*
   CS:3820 Programing Language Concepts

   Homework 3

   Team:  <Daniel Stutz, Kane Templeton>
   Daniel Stutz: 40
   Kane Templeton: 60
*)


(* Part 1 *)

type oper1 = Neg | Not
type oper2 = Add | Mul | Sub | Gt | Eq | And

type expr =
  | Cst of int
  | OpUna of oper1 * expr
  | OpBin of oper2 * expr * expr
  | IfElse of expr * expr * expr


// size : expr -> int
let rec size expr = 
  match expr with
  | Cst(n) -> 1
  | OpUna(op,xpr) -> 1+size(xpr)
  | OpBin(op,xp1,xp2) -> 1+size(xp1)+size(xp2);
  | IfElse(xp1,xp2,xp3) -> 1+size(xp1)+size(xp2)+size(xp3)

let testExpression = IfElse (Cst 4, OpBin (Add, Cst 1, Cst 2), Cst 9);
printfn "\nTest Expression: %A" testExpression
printfn "Size of Expression: %d" (size testExpression)



// subexpressions : expr -> expr list
let rec subexpressions expr =
  match expr with
  | Cst(n) -> []
  | OpUna(op,xpr) -> List.append [xpr] (subexpressions xpr)
  | OpBin(op,xp1,xp2) -> List.append [xp1;xp2] (List.append (subexpressions xp1) (subexpressions xp2))
  | IfElse(xp1,xp2,xp3) -> List.append [xp1;xp2;xp3] (List.append(subexpressions xp1) (List.append (subexpressions xp2) (subexpressions xp3)))

printfn "Subexpressions: %A\n" (subexpressions testExpression)
// drop : int -> 'a list -> 'a list
let rec drop n alist =
  match n with
  | 0 -> alist
  | 1 -> 
    match alist with
    | [h] -> []
    | h :: t -> t
    | _ -> failwith "n is greater than the length of input list"
  | x when x<0 -> failwith "negative value of n"
  | _ -> 
    match alist with //n > 1
    | h :: t -> (drop (n-1) t)
    | _ -> failwith "n is greater than the length of input list"

let testList = [1;2;3;4;5]
printfn "Test List: %A" testList
printfn "Dropped 5 elements: %A\n" (drop 5 testList)


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
let rec scomp expr =
  match expr with
  | Cst(n) -> [SCst(n)]
  | OpUna(op,x) ->
    match op with
    | Neg -> (scomp x)@[SNeg]
    | Not -> (scomp x)@[SIfze(4)]@[SCst(0)]@[SCst(1)]@[SMul]@[SJump(1)]@[SCst(1)]//ifz3,0,1,*,j1,1,...
  | OpBin(op,xp1,xp2) ->
    match op with
    | Add -> (scomp xp1)@(scomp xp2)@[SAdd]
    | Mul -> (scomp xp1)@(scomp xp2)@[SMul]
    | Sub -> (scomp xp2)@(scomp xp1)@[SSub]
    | Gt -> (scomp xp2)@(scomp xp1)@[SGt]
    | Eq -> (scomp (OpUna(Not,OpBin(Sub,xp1,xp2)))) //equal if x1-x2=0
    | And -> (scomp (OpBin(Eq,Cst(2),(OpBin(Add,OpBin(Gt,xp1,Cst(0)),OpBin(Gt,xp2,Cst(0))))))) //equal if both expressions > 0
  | IfElse(xp1,xp2,xp3) -> (scomp xp1)@[SIfze(2)]@(scomp xp2)@[SJump(1)]@(scomp xp3)@[SCst(1)]@[SMul]
  //if xp1==0 -> xp3 else xp2
  //[xp1,ifzN,...] []
  //[ifz2,xp2,j1,xp3,...] [xp1] 
  //[ifz,xp2,j1,xp3,*,1,] 
  //[xp2,j2,xp3,...] []
  //[j2,xp3,...] [xp2]
  //[...] [xp2]
  //[ifz2,xp2,j1,xp3,...] [xp1]
  //[xp3,...] []
  //[...] [xp3]

  //10<10<+eq(2)
  //2

//2+3*4 = 234*+
let testSourceExpression1 = OpBin(Add,Cst(2),OpBin(Mul,Cst(3),Cst(4))) //14
//2*(-3)+4 = 23neg*4+
let testSourceExpression2 = OpBin(Add,OpBin(Mul,Cst(2),OpUna(Neg,Cst(3))),Cst(4)) //-2
//2+not(4-4); 244-not*
//2,4,4,-,if0,4,*,0,j,1,1,*
//2
let testSourceExpression3 = OpBin(Add,Cst(2),OpUna(Not,OpBin(Sub,Cst(4),Cst(4)))) //evaluates to 3
//2*eq(4,4); 244eq*
//x1,x2,-,if0,
//02
let testSourceExpression4 = OpBin(Mul,Cst(2),OpBin(Eq,Cst(2),OpBin(Mul,Cst(2),Cst(1)))) //evaluates to 2

//2*and(1,5); 215and*
//eq(2,not(eq(5,0))+not(eq(1,0)))
//512
let testSourceExpression5 = OpBin(Mul,Cst(2),OpBin(And,Cst(1),Cst(9))) //evaluates to 2

let testSourceExpression6 = IfElse(OpUna(Not,OpBin(And,OpBin(Gt,Cst(20),Cst(10)), OpBin(Eq,OpBin(Sub,Cst(5),Cst(3)),OpBin(Mul,Cst(2),Cst(1))))),Cst(200),Cst(300)) //if not((20>10) and ((5-3)=(2*1))) then 200 else 300 -> 300


printfn "Test Source Expression 1: %A" testSourceExpression1
printfn "Compiled to Stack: %A\n" (scomp testSourceExpression1) //234*+
printfn "Test Source Expression 2: %A" testSourceExpression2
printfn "Compiled to Stack: %A" (scomp testSourceExpression2) //23*4+
printfn "Test Source Expression 3: %A" testSourceExpression3
printfn "Compiled to Stack: %A" (scomp testSourceExpression3) 
printfn "Test Source Expression 4: %A" testSourceExpression4
printfn "Compiled to Stack: %A" (scomp testSourceExpression4) 
printfn "Test Source Expression 5: %A" testSourceExpression5
printfn "Compiled to Stack: %A" (scomp testSourceExpression5) 
printfn "Test Source Expression 6: %A" testSourceExpression6
printfn "Compiled to Stack: %A" (scomp testSourceExpression6) 

// if (2+3) then (5+1) else (2+9)
// not (6-6) -> 1
//if0 3 0 j 1 0 1
// 66-

//SJump(n) : skip n instructions, leave stack unchanged?
//ex: 2+3*4 and 234*+ -> jump 2 -> 
//
//SIfze(n) : skip n instructions if top of stack is not zero. pop the first element of stack
//ex: 2+3*4 = (skip(2))234*+
//234*+   (skip2)
//34*+    (skip2)
//

//234*+ = stack instructions
//34*+ [2]
//*+ [4,3,2]




// seval : sInstr list -> int list -> int

let rec seval sInstructs stack =
  //printf "Instructions: %A\t" (sInstructs)
  //printfn "Stack: %A" (stack) 
  match sInstructs with
  | [] -> 
    match stack with
    | a::_ -> a
    | _ -> failwith "Invalid stack state"
  | h::t ->
    match h with
    | SCst(x) -> seval t ([x]@stack)
    | SAdd -> 
      match stack with
      | a::b ->
        match b with
        | c::d -> seval t ([a+c]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    | SMul -> 
      match stack with
      | a::b ->
        match b with
        | c::d -> seval t ([a*c]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    | SSub ->
      match stack with
      | a::b ->
        match b with
        | c::d -> seval t ([a-c]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    | SNeg ->
      match stack with
      | a::b -> seval t ([-a]@b)
      | _ -> failwith "Invalid stack state"
    | SGt ->
      match stack with
      | a::b ->
        match b with
        | c::d when a>c -> seval t ([1]@d)
        | c::d -> seval t ([0]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    | SIfze(n) ->
      match stack with
      | a::b when a=0 -> seval (drop n t) b
      | a::b -> seval t b
      | _ -> failwith "Invalid stack state"
    | SJump(n) -> seval (drop n t) stack

printfn "Evaluation of Test Source Expression 1: %A\n" (seval (scomp testSourceExpression1) [])
//seval 234*+ []
//seval 34*+ [2]
//seval 4*+ [3,2]
//seval *+ [4,3,2]
printfn "Evaluation of Test Source Expression 2: %A\n" (seval (scomp testSourceExpression2) [])
printfn "Evaluation of Test Source Expression 3: %A\n" (seval (scomp testSourceExpression3) [])
printfn "Evaluation of Test Source Expression 4: %A\n" (seval (scomp testSourceExpression4) [])
printfn "Evaluation of Test Source Expression 5: %A\n" (seval (scomp testSourceExpression5) [])
printfn "Evaluation of Test Source Expression 6: %A\n" (seval (scomp testSourceExpression6) [])



let testInstructions = [SCst(2);SCst(3);SCst(4);SJump(2);SCst(5);SCst(6);SMul;SAdd]
printfn "Test Instructions for Jump: %A" testInstructions
printfn "Evaluation of Test Instructions: %A\n" (seval testInstructions [])
//test input instructions: [2,3,4,Jump2,5,6,*,+]
//[3,4,Jump2,5,6,*,+] [2]
//[4,Jump2,5,6,*,+] [3,2]
//[Jump2,5,6,*,+] [4,3,2]
//[*,+] [4,3,2]
//evalutes to 14

let testInstructions2 = [SCst(1);SCst(2);SCst(3);SCst(4);SIfze(3);SMul;SAdd]
let testInstructions3 = [SCst(1);SCst(2);SCst(3);SCst(0);SIfze(3);SSub;SSub;SSub;SMul;SAdd;SNeg]
//[1,2,3,4,Sifz(3),*,+] []
//[2,3,4,Sifz(3),*,+] [1]
//[3,4,Sifz(3),*,+] [2,1]
//[4,Sifz(3),*,+] [3,2,1]
//[Sifz(3),*,+] [4,3,2,1]
//[*,+] [3,2,1]
//[+] [6,1]
// 7

//[1,2,3,0,Sifz(3),*,+,neg] []
//[2,3,0,Sifz(3),*,+,neg] [1]
//[3,0,Sifz(3),*,+,neg] [2,1]
//[0,Sifz(3),*,+,neg] [3,2,1]
//[Sifz(3),-,-,-,*,+,neg] [0,3,2,1]
//[*,+,neg] [3,2,1]
//[+,neg] [6,1]
//[neg] 7
//-7
printfn "Test Instructions 1 for SIfze: %A" testInstructions2
printfn "Evaluation of Test Instructions 1: %A "(seval testInstructions2 [])
printfn "Test Instructions 2 for SIfze: %A" testInstructions3
printfn "Evaluation of Test Instructions 2: %A "(seval testInstructions3 [])



// run : sInstr list -> int
let run (p : sInstr list) : int = seval p []

let trimEndSpace str = 
  match (String.length(str)) with
  | 0 -> ""
  | 1 ->
    match str with
    | " " -> ""
    | _ -> str
  | L ->
    match str.[L-1] with
    | ' ' -> str.[0..(L-2)]
    | _-> str

// byteCode : sInstr list -> string
let rec byteCode sInstructions =
  match sInstructions with
  | [] -> "" 
  | h::t -> 
    match h with
    | SCst(x) -> (trimEndSpace (String.concat " " ["0";string x;(byteCode t)]))
    | SAdd -> (trimEndSpace (String.concat " " ["1";(byteCode t)]))
    | SSub -> (trimEndSpace (String.concat " " ["2";(byteCode t)]))
    | SMul -> (trimEndSpace (String.concat " " ["3";(byteCode t)]))
    | SNeg -> (trimEndSpace (String.concat " " ["4";(byteCode t)]))
    | SGt -> (trimEndSpace (String.concat " " ["5";(byteCode t)]))
    | SIfze(n) -> (trimEndSpace (String.concat " " ["6";string n;(byteCode t)]))
    | SJump(n) -> (trimEndSpace (String.concat " " ["7";string n;(byteCode t)]))

  


let testStackInstructions = [SCst 10; SCst 2; SAdd; SCst 32; SCst 4; SCst 5; SAdd; SMul; SAdd]
printfn "\nTest Stack Instructions: %A" (testStackInstructions)
printfn "Bytecode: \"%s\"" (byteCode testStackInstructions)



// beval : int list -> int list -> int

let rec beval bytecode stack = 
  match bytecode with
  | [] ->
    match stack with
    | x::_ -> x
    | _ -> failwith "Invalid stack state"
  | byt::rbyt ->
    match byt with
    // Constant
    | 0 -> 
      match rbyt with
      | h::t -> beval t ([h]@stack)
      | _ -> failwith "Invalid stack state"
    // Addition
    | 1 -> 
      match stack with
      | h::t ->
        match t with
        | k::d -> beval rbyt ([h+k]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    // Subtraction
    | 2 -> 
      match stack with
      | h::t ->
        match t with
        | k::d -> beval rbyt ([h-k]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    // Multiplication
    | 3 -> 
      match stack with
      | h::t ->
        match t with
        | k::d -> beval rbyt ([h*k]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    // Negative
    | 
      4 -> 
      match stack with
      | h::t -> beval rbyt ([-h]@t)
      | _ -> failwith "Invalid stack state"
    // Greater Than
    | 5 -> 
      match stack with
      | h::t ->
        match t with
        | k::d when (k>h) -> beval rbyt ([1]@d)
        | k::d -> beval rbyt ([0]@d)
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    // If 0
    | 6 -> 
      match stack with
      | h::t when (h=0) -> //6 2 1 2 3 4] [0 3] -> [3 4] [3]
        match rbyt with
        | n::b -> beval (drop n b) t
        | _ -> failwith "Invalid stack state"
      | h::t -> //[6 2 1 2 3 4] [1 3] -> [1 2 3 4] [3]
        match rbyt with
        | n::b -> beval b t
        | _ -> failwith "Invalid stack state"
      | _ -> failwith "Invalid stack state"
    | 7 -> //j [7 2 1 2 3 4] -> [3 4]
      match rbyt with
      | n::b -> beval (drop n b) stack
      | _ -> failwith "Invalid stack state"
    | _ -> failwith "Invalid stack state" 
      

//[SCst 10; SCst 2; SAdd; SCst 32; SCst 4; SCst 5; SAdd; SMul; SAdd]
//0 10 0 2 1 0 32 0 4 0 5 1 3 1 []
//0 2 1 0 32 0 4 0 5 1 3 1 [10]
//1 0 32 0 4 0 5 1 3 1 [2,10]
//0 32 0 4 0 5 1 3 1 [12]
//0 4 0 5 1 3 1 [32,12]
//0 5 1 3 1 [4,32,12]
//1 3 1 [5,4,32,12]
//3 1 [9,32,12]
//1 [288,12]
// [300]

let parse (p : string) : int list =
  let l = Seq.toList (p.Split ' ') in
  List.map System.Int32.Parse l

printfn "Hard-Coded Bytecode Evaluation: %A" (beval (parse("0 10 0 2 1 0 32 0 4 0 5 1 3 1")) [])
printfn "Compiled Bytecode Evaluation: %A" (beval (parse(byteCode testStackInstructions)) [])




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