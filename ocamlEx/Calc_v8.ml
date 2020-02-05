module Calc_v= struct 
 type exp =
 | LEFT of string * string * exp * exp
 | CALL of string * exp
 | READ
 | EQUAL of exp * exp
 | LESS of exp * exp
 | NOT of exp
 | BOOL of bool
 | IF of exp * exp * exp
 | LET of string * exp * exp
 | VAR of string 
 | INT of int
 | ADD of exp * exp
 | SUB of exp * exp
 | MUL of exp * exp
 | DIV of exp * exp


 type value_of_func =
 | NUM of int
 | FUNC of string * exp

 type environment = (string * value_of_func) list 

let rec calc : exp -> environment -> int
 = fun e env ->
  match e with 
  | READ -> read_int()
  | LETF ( function, param, e1, e2) ->
	let env' = (function, Func(param, e1)) :: env 
	 in call e2 env'
  | CALL (f, arg) ->
	let (param, body) =
	 (match (List.assoc f env) with
	 | NUM n -> 
