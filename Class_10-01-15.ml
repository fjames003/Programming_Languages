(* 
	HW3: An ML interpreter in OCaml

	-involves two languages...
	
	ex: how do we interpret ("1 + 2")?
		meta-circular approach: Ocaml knows aout intefers and arithmetic. 
		Convert "1" and "2" to Ocaml integers, add them using Ocaml's operator.

 *)

let x = 1 + 1;;
let y = 2 + x;;
let z = x + y * 3;;

(* 
	parsing:
		"x + y * 3"
		-converty to a list of tokens:
			keywords, variables, integer literals, etc...

			[VAR "x"; PLUS; VAR "y"; TIMES; INT 3]
		-check that the list of tokens is syntactically legal
			[PLUS;PLUS;PLUS]
		-if so, produce a data structure that represents the program 
		as an *abstract syntax tree* (AST) sometimes called a parse tree
			-representing cade as data
			-unambiguous
			-takes into account precedence ad associativity

	Step 2: Typecheck the program by traversing the AST.
		-if error then halt and print an error message
	Step 3: Evaluate the program by traversing the AST
 *)

(* 
	Grammar of Expressions:
	E::= N | B | X | E && E | E + E | if E then E else E | let X = E in E

	ex:
		if true && false then 1 else 0

		let x = true in x && x

	USER-DEFINED TYPES!

 *)

type exp = 
	  Int of int
	| Bool of bool
	| Var of string
	| And of exp * exp
	| Plus of exp * exp
	| If of exp * exp * exp
	| Let of string * exp * exp
;;

(* if true && false then 1 else 0 *)

let e1 = If (And (Bool true, Bool false), Int 1, Int 0);;

let e2 = Let ("x", Bool true, AND (Var "x", Var "x"));;

(* Grammar for values:
	mlval ::= N | B
 *)

type mlval = IntVal of int | BoolVal of bool;;

let rec eval (e: exp) : mlval =
	match e with
	| Int i  -> IntVal i
	| Bool b -> BoolVal b
;;

let _ = eval (Int 5);;

exception EvalError of string;;

type mlenv = (string * mlval) list;;

let rec get (k : string) (env : mlenv) : mlval =
	match env with
	| [] -> raise (EvalError ("var not found: " ^ x))
	| (k', v) :: env' -> if k' = k then v else get k env'
;;

let rec eval (env : mlenv) (e: exp) : mlval =
	match e with
	| Int i       -> IntVal i
	| Bool b      -> BoolVal b
	| Var x       -> get x env
	| And (e1,e2) -> 
		(match (eval env e1, eval env e2) with
		| (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
		| _                        -> 
			raise (EvalError "&& expected two BoolVals")
		)
	| Plus (e1, e2) ->
		(match (eval env e1, eval env e2) with
		| (IntVal i1, Intval i2) -> IntVal (i1 + i2)
		| _                      -> 
			raise (EvalError "+ expected two IntVals")
		)
	| If (e1, e2, e3) ->
		(match eval env e1 with
		| BoolVal true  -> eval env e2
		| BoolVal false -> eval env e3
		| _             ->
			raise (EvalError "If expected a BoolVal as a condition")
		)
	| Let (nm, defn, body) -> 
		let defnVal = eval env defn in 
		let env' = (nm, defnVal) :: env in
		eval env' body
;;

let _ = eval (And (Bool true, Bool false));;
let _ = eval (And (Bool true, Int 5));;
