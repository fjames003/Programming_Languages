(*
Since we didn't get through everything we needed to today, please read the Lecture 7 Preview before class Thursday.
Email me solutions to the following exercises before class Thursday for up to 15% extra credit on homework 2. Partial solutions will receive partial credit.
Exercise: Concatenate a list of strings using a single call to fold_right and again using a single call to or fold_right. Note: you can concatenate two strings using the operator ^
"Single call to fold_right/fold_left" means replace E1 through E6 with appropriate expressions below:
let concat (l : string list) : string = fold_right E1 E2 E3
let concat (l : string list) : string = fold_left E4 E5 E6
Exercise: Same as above but for reverse.
Exercise: Same as above but for map.
Exercise: Same as above but for filter.
Exercise: Implement fold_right as a recursive function based on the rules given in Lecture 7 Preview.
Exercise: Same as above but for fold_left.
*)

let fold_right = List.fold_right;;

let concat (l : string list) : string = 
	fold_right (fun x y -> x ^ y) l ""
;;

let fold_left = List.fold_left;;

let concat (l : string list) : string = 
	fold_left (fun x y -> x ^ y) "" l
;;

(* --------------------------------------- *)

let reverse (l : 'a list) : 'a list = 
	fold_left (fun x y -> y :: x) [] l
;;

(* --------------------------------------- *)

let map f l =
	fold_right (fun x y -> (f x)::y) l []
;;


(* --------------------------------------- *)

let filter f l =
	fold_right (
		fun x y -> 
		if f x 
		then x::y 
		else y
				) l []
;;

(* --------------------------------------- *)

let rec fold_right f lst initial = 
	match lst with
	| [] -> initial
	| hd::tl -> f hd (fold_right f tl initial)

(* --------------------------------------- *)

let rec fold_left f initial lst = 
	match lst with
	| [] -> initial
	| hd::tl -> fold_left f (f initial hd) tl