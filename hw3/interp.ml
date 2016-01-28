(* Name: Frankie James

   UID: 919840157

   Others With Whom I Discussed Things:
    Irakli Khizanishvili
   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* TESTING *)

(* We need to be able to test code that might throw an exception. In particular,
   we want to test that it does throw a particular exception when we expect it to.
 *)

type 'a or_exception = Value of 'a | Exception of exn

(* General-purpose testing function. You don't need to use this. I'll provide
   specialized test functions you can use instead.
 *)

let tester (nm : string) (thunk : unit -> 'a) (expected : 'a or_exception) =
  let got = try Value (thunk ()) with e -> Exception e in
  let msg = match (expected, got) with
      (e1,e2)              when e1 = e2   -> "OK"
    | (Value _,  Value _)                 -> "FAILED (value error)"
    | (Exception _, Value _)              -> "FAILED (expected exception)"
    | (_, Exception (ImplementMe msg))    -> "FAILED: ImplementMe(" ^ msg ^ ")"
    | (_, Exception (MatchFailure))       -> "FAILED: MatchFailure"
    | (_, Exception (DynamicTypeError s)) -> "FAILED: DynamicTypeError(" ^ s ^ ")"
    | (_, Exception e)                    -> "FAILED: " ^ Printexc.to_string e
  in
  print_string (nm ^ ": " ^ msg ^ "\n");
  flush stdout

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (IntPat(i), IntVal(j)) when i=j   -> Env.empty_env()
    | (BoolPat(b), BoolVal(d)) when b=d -> Env.empty_env()
    | (WildcardPat, _)                  -> Env.empty_env()
    | (VarPat(nm), _)                   -> Env.add_binding nm value (Env.empty_env())
    | (NilPat, NilVal)                  -> Env.empty_env()
    | (ConsPat(x,y), ConsVal(a,b))      -> Env.combine_envs (patMatch x a) (patMatch y b)   
    | _ -> raise MatchFailure


(* patMatchTest defines a test case for the patMatch function.
   inputs: 
     - nm: a name for the test, for the status report.
     - pat: a mini-OCaml pattern, the first input to patMatch
     - value: a mini-OCaml value, the second input to patMatch
     - expected: the expected result of running patMatch on these inputs.
 *)

let patMatchTest (nm,pat,value,expected) =
  tester ("patMatch:" ^ nm) (fun () -> patMatch pat value) expected

(* Tests for patMatch function. 
      ADD YOUR OWN! 
 *)
let patMatchTests = [
    (* integer literal pattern tests *)
    ("IntPat/1", IntPat 5, IntVal 5,      Value [])
  ; ("IntPat/2", IntPat 5, IntVal 6,      Exception MatchFailure)
  ; ("IntPat/3", IntPat 5, BoolVal false, Exception MatchFailure)

    (* boolean literal pattern tests *)   
  ; ("BoolPat/1", BoolPat true, BoolVal true,  Value [])
  ; ("BoolPat/2", BoolPat true, BoolVal false, Exception MatchFailure)
  ; ("BoolPat/3", BoolPat true, IntVal 0,      Exception MatchFailure)

    (* wildcard pattern *)
  ; ("WildcardPat/1", WildcardPat, IntVal 5,     Value [])
  ; ("WildcardPat/2", WildcardPat, BoolVal true, Value [])

    (* variable pattern *)
  ; ("VarPat/1", VarPat "x", IntVal 5,     Value [("x", IntVal 5)])
  ; ("VarPat/2", VarPat "y", BoolVal true, Value [("y", BoolVal true)])

    (* Nil pattern *)
  ; ("NilPat/1", NilPat, NilVal,       Value [])
  ; ("NilPat/2", NilPat, IntVal 5,     Exception MatchFailure)
  ; ("NilPat/3", NilPat, BoolVal true, Exception MatchFailure)

    (* cons pattern *)
  ; ("ConsPat/1", ConsPat(IntPat 5, NilPat), ConsVal(IntVal 5, NilVal), 
     Value [])
  ; ("ConsPat/2", ConsPat(IntPat 5, NilPat), ConsVal(BoolVal true, NilVal), 
     Exception MatchFailure)
  ; ("ConsPat/3", ConsPat(VarPat "hd", VarPat "tl"), ConsVal(IntVal 5, NilVal), 
     Value [("tl", NilVal); ("hd", IntVal 5)])
  ]
;;

(* Run all the tests *)
List.map patMatchTest patMatchTests;;

(* To evaluate a match expression, we need to choose which case to take.
   Here, match cases are represented by a pair of type (mopat * moexpr) --
   a pattern and the expression to be evaluated if the match succeeds.
   Try matching the input value with each pattern in the list. Return
   the environment produced by the first successful match (if any) along
   with the corresponding expression. If there is no successful match,
   raise the MatchFailure exception.
 *)
let rec matchCases (value : movalue) (cases : (mopat * moexpr) list) : moenv * moexpr =
  match cases with 
  | []           -> raise MatchFailure
  | (p1, e1)::tl -> try (patMatch p1 value, e1) with _ -> matchCases value tl

(* We'll use these cases for our tests.
   To make it easy to identify which case is selected, we make
 *)
let testCases : (mopat * moexpr) list =
  [(IntPat 1, Var "case 1");
   (IntPat 2, Var "case 2");
   (ConsPat (VarPat "head", VarPat "tail"), Var "case 3");
   (BoolPat true, Var "case 4")
  ]

(* matchCasesTest: defines a test for the matchCases function.
   inputs:
     - nm: a name for the test, for the status report.
     - value: a mini-OCaml value, the first input to matchCases
     - expected: the expected result of running (matchCases value testCases).
 *)
let matchCasesTest (nm, value, expected) =
  tester ("matchCases:" ^ nm) (fun () -> matchCases value testCases) expected

(* Tests for matchCases function. 
      ADD YOUR OWN! 
 *)
let matchCasesTests = [
    ("IntVal/1", IntVal 1, Value ([], Var "case 1"))
  ; ("IntVal/2", IntVal 2, Value ([], Var "case 2"))

  ; ("ConsVal", ConsVal(IntVal 1, ConsVal(IntVal 2, NilVal)), 
     Value ([("tail", ConsVal(IntVal 2, NilVal)); ("head", IntVal 1)], Var "case 3"))

  ; ("BoolVal/true",  BoolVal true,  Value ([], Var "case 4"))
  ; ("BoolVal/false", BoolVal false, Exception MatchFailure)
  ]
;;

List.map matchCasesTest matchCasesTests;;

(* "Tying the knot".
   Make a function value recursive by setting its name component.
 *)
let tieTheKnot nm v =
  match v with
  | FunVal(None,pat,def,env) -> FunVal(Some nm,pat,def,env)
  | _                        -> raise (DynamicTypeError "tieTheKnot expected a function")

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
    IntConst(i)                -> IntVal(i)
  | BoolConst(b)               -> BoolVal(b)
  | Nil                        -> NilVal
  | Var(v)                     -> (try (Env.lookup v env) with _ -> raise (DynamicTypeError ("Variable not found in environment: " ^ v) ))
  | BinOp(e1, op, e2)          -> (match (evalExpr e1 env, op, evalExpr e2 env) with
                                  | (IntVal i1, Plus, IntVal i2)  -> IntVal(i1 + i2)
                                  | (IntVal i1, Minus, IntVal i2) -> IntVal(i1 - i2)
                                  | (IntVal i1, Times, IntVal i2) -> IntVal(i1 * i2)
                                  | (IntVal i1, Eq, IntVal i2)    -> BoolVal(i1 = i2)
                                  | (IntVal i1, Gt, IntVal i2)    -> BoolVal(i1 > i2)
                                  | (a, Cons, NilVal)             -> ConsVal(a, NilVal)
                                  | (a, Cons, ConsVal(x,y))       -> ConsVal(a, ConsVal(x,y))
                                  | (_,_,_)                       -> raise (DynamicTypeError "Expressions for operator did not match.")
                                  )
  | Negate n                   -> (match evalExpr n env with
                                  | IntVal i -> IntVal(-i)
                                  | _        -> raise (DynamicTypeError "Can only negate integers")
                                  )
  | If (e1, e2, e3)            -> (match evalExpr e1 env with
                                  | BoolVal true  -> evalExpr e2 env
                                  | BoolVal false -> evalExpr e3 env
                                  | _             -> raise (DynamicTypeError "If expected a BoolVal as a condition")
                                  )
  | Fun (p1, e1)               -> FunVal(None, p1, e1, env)
  | FunCall (e1, e2)           -> (match (evalExpr e1 env, evalExpr e2 env) with
                                  | (FunVal(None, p1, fbody, fenv), mval)    -> let newEnv = Env.combine_envs fenv (patMatch p1 mval) in evalExpr fbody newEnv
                                  | (FunVal(Some nm, p1, fbody, fenv), mval) -> 
                                      let newEnv = Env.combine_envs 
                                      (Env.add_binding nm (FunVal(Some nm, p1, fbody, fenv)) fenv) 
                                      (patMatch p1 mval) 
                                      in evalExpr fbody newEnv
                                  | (_,_)                                    -> raise MatchFailure
                                  )
  | Match (e1, l1)             -> let (env', e2) = (matchCases (evalExpr e1 env) l1) in evalExpr e2 (Env.combine_envs env' env)
  | Let (nm, defn, body)       -> let env' = Env.combine_envs env (patMatch nm (evalExpr defn env)) in evalExpr body env'
  | LetRec (nm, f, e2)         -> let env' = Env.add_binding nm (tieTheKnot nm (evalExpr f env)) env in evalExpr e2 env'

(* evalExprTest defines a test case for the evalExpr function.
   inputs: 
     - nm: a name for the test, for the status report.
     - expr: a mini-OCaml expression to be evaluated
     - expected: the expected result of running (evalExpr expr [])
                 (either a value or an exception)
 *)

let evalExprTest (nm,expr,expected) = 
  tester ("evalExpr:" ^ nm) (fun () -> evalExpr expr []) expected

(* Tests for evalExpr function. 
      ADD YOUR OWN!
 *)
let evalExprTests = [
    ("IntConst",    IntConst 5,                                    Value (IntVal 5))
  ; ("BoolConst",   BoolConst true,                                Value (BoolVal true))
  ; ("Plus",        BinOp(IntConst 1, Plus, IntConst 1),           Value (IntVal 2))
  ; ("BadPlus",     BinOp(BoolConst true, Plus, IntConst 1),       Exception (DynamicTypeError "Expressions for operator did not match."))
  ; ("Negate",      Negate(IntConst 5),                            Value (IntVal(-5)))
  ; ("BadNegate",   Negate(BoolConst true),                        Exception (DynamicTypeError "Can only negate integers"))
  ; ("IfBranch1",   If(BoolConst true, IntConst 1, IntConst 2),    Value(IntVal 1))
  ; ("IfBranch2",   If(BoolConst false, IntConst 1, IntConst 2),   Value(IntVal 2))
  ; ("BadIf",       If(IntConst 1, IntConst 1, IntConst 1),        Exception (DynamicTypeError "If expected a BoolVal as a condition"))
  ; ("Let",         Let(VarPat "x", IntConst 1, Var "x"),          Value (IntVal 1))
  ; ("Fun",         FunCall(Fun(VarPat "x", Var "x"), IntConst 5), Value (IntVal 5))
  ; ("BadLetRec1", LetRec("foo", Var "foo", IntConst 5), Exception (DynamicTypeError "Variable not found in environment: foo"))
  ; ("BadLetRec2", LetRec("foo", IntConst 5, Var "foo"), Exception (DynamicTypeError "tieTheKnot expected a function"))
  ]
;;

List.map evalExprTest evalExprTests;;

(* See test.ml for a nicer way to write more tests! *)
  

