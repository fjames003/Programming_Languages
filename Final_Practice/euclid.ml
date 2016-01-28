(* a) Implement a recursive function gcd : (int * int) -> int in
      ocaml.  Use pattern matching whenever possible. *)

(* The Algorithm:
     If the two numbers are equal, the gcd is that number.
     If either number is zero, the gcd is the non-zero number.
     Otherwise, one number is greater than the other.
       the gcd of the two numbers is equal to the gcd of the smaller
       number and the difference between the two numbers. *)


let rec gcd = fun(x,y) -> match (x,y) with
	| (a,b) when a = b -> a
	| (a,0)            -> a
	| (0,b)            -> b
	| (a,b) when a > b -> gcd(b, a-b)
	| (a,b)            -> gcd(a, b-a)