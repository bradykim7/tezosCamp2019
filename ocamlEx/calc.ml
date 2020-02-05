type exp = 
 Const of int
 |Minus of exp*exp
 |Plus of exp*exp
 |Mult of exp*exp

let rec calc e = 
  match e with
  | Const n -> n
  | Minus (e1,e2) -> (calc e1) -(calc e2)
  | Plus (e1, e2) -> (calc e1)*(calc e2)
  | Mult (e1, e2) -> (calc e1)*(calc e2)
