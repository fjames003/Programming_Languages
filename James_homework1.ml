(* Name:        Frankie James
   Email:       fjames1@lion.lmu.edu
   Student ID:  919840157

   Others With Whom I Discussed Things:
    Irakli Khizanishvili
   Other Resources I Consulted:
    http://stackoverflow.com/questions/11193783/ocaml-strings-and-substrings
      -Used this for the idea of let len = String.length when it wasn't matching on it.
    http://www.ffconsultancy.com/ocaml/benefits/functional.html
      -Found this looking to see if ocaml could natively 'pow()', ended up basing my int_pow off of their float_pow.
*)

(* NOTE: for full credit, add unit tests for each problem.  You should
   define enough unit tests to provide full test coverage for your
   code; each subexpression should be evaluated for at least one
   test.

   Use OCaml's built-in assert function to define unit tests.
  
   run `ocaml hw1.ml` to typecheck and run all tests.
 *)

type 'a option = None | Some of 'a

(* Problem 1
Write a function to compute the nth Fibonacci number, where
the 0th Fibonacci number is 0, the 1st is 1, and the nth for n > 1 is
the sum of the (n-1)st and (n-2)nd Fibonacci numbers.
 *)

let rec fib (n:int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | index when index > 0 -> fib(index - 1) + fib(index - 2)
  | _ -> -1

let _ = assert (fib(0) = 0)
let _ = assert (fib(1) = 1)
let _ = assert (fib(10) = 55)
let _ = assert (fib(-10) = -1)

let _ = print_endline "1. Fibonacci Tests have completed..."

(* Problem 2	       
Write a function clone of type 'a * int -> 'a list.  The function
takes an item e and a nonnegative integer n and returns a list
containing n copies of e.  
 *)

let rec clone ((e,n) : ('a * int)) : 'a list =
  match n with
  | left when left > 0 -> e::clone (e, (left - 1))
  | _ -> []

let _ = assert (clone (1, 0) = [])
let _ = assert (clone ("Hello", 2) = ["Hello"; "Hello"]) 
let _ = assert (clone ("World", -2) = [])

let _ = print_endline "2. Clone Tests have completed..."


(* Problem 3
Write a recursive function to get the number of occurrences of an
element in a list. For example, there are 0 occurrences of 5 in [1;2;3].
There are 2 occurrences of 5 in [1;5;5;0].
 *)

let rec count ((v,l) : ('a * 'a list)) : int =
  match l with
  | [] -> 0
  | hd::tl when hd = v -> 1 + count (v, tl)
  | hd::tl -> 0 + count (v, tl)

let _ = assert (count (5, [1;2;3]) = 0)
let _ = assert (count (5, [1;5;5;0]) = 2)
let _ = assert (count (1, []) = 0)

let _ = print_endline "3. Count Tests have completed..."



(* Problem 4
Write a function that appends one list to the front of another.    
 *)

let rec append ((l1,l2) : ('a list * 'a list)) : 'a list =
  match l1 with
  | [] -> l2
  | hd::tl -> hd :: (append (tl, l2))

let _ = assert (append([1;2;3], [4;5;6]) = [1;2;3;4;5;6])
let _ = assert (append([], []) = [])
let _ = assert (append([1], []) = [1])
let _ = assert (append([], [1]) = [1])
let _ = print_endline "4. Append Tests have completed..."  


(* Problem 5
Use append to write a recursive function that reverses the elements in
a list.
 *)

let rec reverse (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | hd::tl -> append(reverse tl, [hd])

let _ = assert (reverse [] = [])
let _ = assert (reverse [1;2;3] = [3;2;1])

let _ = print_endline "5. Reverse Tests have completed..." 

(* Problem 6
Write a function "tails" of type 'a list -> 'a list list that takes a
list and returns a list of lists containing the original list along
with all tails of the list, from longest to shortest.
 *)	       

let rec tails (l : 'a list) : 'a list list = 
  match l with
  | [] -> [[]]
  | hd::tl -> append([hd::tl], tails(tl))

let _ = assert (tails [1;2;3] = [[1;2;3];[2;3];[3];[]])
let _ = assert (tails [] = [[]])

let _ = print_endline "6. Tails Tests have completed..."


(* Problem 7
Write a function split of type 'a list -> 'a list * 'a list that
separates out those elements of the given list in odd positions (that
is, the first, third, fifth, etc.) from those in even positions (that
is, the second, fourth, etc.). 
 *)	       

let rec split (l : 'a list) : 'a list * 'a list = 
  match l with
  | [] -> ([], [])
  | hd::[] -> ([hd], [])
  | hd::md::tl -> 
    let (odd, even) = split tl 
    in (hd::odd, md::even)

let _ = assert (split [1;2;3;4] = ([1;3], [2;4]))
let _ = assert (split [] = ([],[]))
let _ = assert (split [1] = ([1], []))

let _ = print_endline "7. Split Tests have completed..."

(* Problem 8
Flatten a list of lists.
 *)

let rec flatten (l: 'a list list) : 'a list =
  match l with
  | hd::tl -> append (hd, flatten tl)
  | _ -> []

let _ = assert (flatten [[2]; []; [3;4]] = [2; 3; 4])

let _ = print_endline "8. Flatten Tests have completed..."

			       
(* Problem 9
Write a function to return the last element of a list. To deal with
the case when the list is empty, the function should return a value of
the built-in option type, defined as follows:

type 'a option = None | Some of 'a
 *)

let rec last (l: 'a list) : 'a option =
  match l with
  | [] -> None
  | hd::[] -> Some hd
  | hd::tl -> last tl

let _ = assert (last [] = None)
let _ = assert (last [1;3;2] = Some 2)
let _ = assert (last [1] = Some 1)

let _ = print_endline "9. Last Tests have completed..."	       


(* Problem 10
Write a recursive function to return the longest prefix of a list --
another list containing all but the last element. For example, the
longest prefix of [1;2;3;4;5] is [1;2;3;4]
 *)

let rec longestPrefix (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | hd::[] -> []
  | hd::tl -> hd :: longestPrefix tl

let _ = assert (longestPrefix [] = [])
let _ = assert (longestPrefix [1] = [])
let _ = assert (longestPrefix [1;2;3;4;5] = [1;2;3;4])

let _ = print_endline "10. LongestPrefix Tests have completed..."
	  
      
(* Problem 11
Write a recursive function that checks whether a list is a
palindrome. A palindrome reads the same forward or backward;
e.g. ["x"; "a"; "m"; "a"; "x"]. Hint: use last and longestPrefix.
 *)
	       
let rec palindrome (l : 'a list) : bool =
  match l with
  | [] -> true
  | hd::[] -> true
  | hd::tl when Some hd = last tl -> palindrome(longestPrefix tl)
  | _ -> false

let _ = assert (palindrome [] = true)
let _ = assert (palindrome ["r";"a";"c";"e";"c";"a";"r"] = true)
let _ = assert (palindrome ["a"] = true)
let _ = assert (palindrome ["F"; "J"] = false)

let _ = print_endline "11. Palindrome Tests have completed..."

(*
Problem 12
The naive implementation of fib is wildly inefficient, because it does
a ton of redundant computation.  Perhaps surprisingly, we can make
things much more efficient by building a list of the first n Fibonacci
numbers. Write a function fibsFrom that takes a nonnegative number n
and returns a list of the first n Fibonacci numbers in reverse order
(i.e., from the nth to the 0th).  Recall that the 0th Fibonacci number
is 0, the 1st is 1, and the nth for n > 1 is the sum of the (n-1)st
and (n-2)nd Fibonacci numbers.  You should implement fibsFrom without
writing any helper functions.  A call like (fibsFrom 50) should be
noticeably faster than (fib 50).  Hint: Your function should make only
one recursive call. *)


let rec fibsFrom (n:int) : int list =
  match n with
  | 0 -> [0]
  | 1 -> [1;0]
  | n when n > 1 -> let hd::mid::tl = fibsFrom(n - 1) in (hd + mid) :: hd :: mid :: tl
  | _ -> []

let _ = assert (fibsFrom 1 = [1;0])
let _ = assert (fibsFrom 0 = [0])
let _ = assert (fibsFrom 5 = [5;3;2;1;1;0])

let _ = print_endline "12. FibsFrom Tests have completed..."
	  
  
(* Problem 13
The naive algorithm for reversing a list takes time that is quadratic
in the size of the argument list.  In this problem, you will implement
a more efficient algorithm for reversing a list: your solution should
only take linear time. Call this function fastRev. The key to fastRev
is that it builds the reversed list as we recurse over the input list,
rather than as we return from each recursive call.  This is similar to
how an iterative version of list reversal, as implemented in a
language like C, would naturally work.

To get the right behavior, your fastRev function should use a local
helper function revHelper to do most of the work.  The helper function
should take two arguments: (1) the suffix of the input list that
remains to be reversed; (2) the reversal of the first part of the
input list.  The helper function should return the complete reversed
list.  Therefore the reversal of an input list l can be performed via
the invocation revHelper(l, []).  I've already provided this setup for
you, so all you have to do is provide the implementation of revHelper
(which is defined as a nested function within fastRev) and invoke it
as listed above.  The call (fastRev (clone(0, 10000))) should be
noticeably faster than (reverse (clone(0, 10000))).
 *)
				       
let fastRev (l : 'a list) : 'a list =
  let rec revHelper (remain, sofar) =
    match remain with
    | [] -> sofar
    | hd::tl -> revHelper (tl, hd :: sofar)
in revHelper(l, [])
		
let _ = assert (fastRev [3;2;1] = [1;2;3])
let _ = assert (fastRev [] = [])
let _ = assert (fastRev (clone(0, 10000)) = clone(0, 10000))	

let _ = print_endline "13. FastRev Tests have completed..."

       
(* Problem 14
Strings in OCaml do not support pattern matching very well, so it is
sometimes necessary to convert them to something that we can match on
more easily: lists of characters.  Using OCaml's built-in functions
String.get and String.length, write a function chars that converts a
string to a char list.
 *)

let chars (s:string) : char list =
  let len = String.length s in
  let rec charsHelper (num, lst) = 
    match num with
    | -1 -> []
    | 0  -> (String.get s 0) :: lst
    | i when i > 0 -> charsHelper(num - 1, (String.get s num)::lst)
  in charsHelper ((len - 1), [])

let _ = assert (chars "asdf" = ['a';'s';'d';'f'])
let _ = assert (chars "" = [])

let _ = print_endline "14. Chars Tests have completed..."
    

(* Problem 15
Convert a list of digits (numbers between 0 and 9) into an integer.
 *)

(* 
let rec pow (num, p) = 
  match p with 
  | 0 -> 1
  | i when i > 0 -> num * pow(num, i - 1)
  | _ -> -1 
*)

let rec int_of_digits (ds : int list) : int =
  let len = List.length ds in
    match len with
    | 0 -> 0
    | 1 -> let hd::tl = ds in hd
    | _ -> let hd::tl = ds in
      let rec pow (num, p) = 
        match p with 
        | 0 -> 1
        | i when i > 0 -> num * pow(num, i - 1)
        | _ -> -1
      in hd * pow(10, (len - 1)) + int_of_digits(tl)

let _ = assert (int_of_digits [1;2;3] = 123)
let _ = assert (int_of_digits [0;1;0] = 10)
let _ = assert (int_of_digits [] = 0)

let _ = print_endline "15. Int_of_Digits Tests have completed..." 