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

let _ = assert (concat ["Hello, "; "World!"] = "Hello, World!")

let fold_left = List.fold_left;;

let concat (l : string list) : string = 
	fold_left (fun x y -> x ^ y) "" l
;;

let _ = assert (concat ["Hello, "; "World!"] = "Hello, World!")

(* --------------------------------------- *)

let reverse (l : 'a list) : 'a list = 
	fold_left (fun x y -> y :: x) [] l
;;

let _ = assert (reverse [1;2;3] = [3;2;1])

let reverse (l : 'a list) : 'a list = 
	fold_right (fun x y -> y @ [x]) l []

let _ = assert (reverse [1;2;3] = [3;2;1])

(* --------------------------------------- *)

let map f l =
	fold_right (fun x y -> (f x)::y) l []
;;

let _ = assert (map (fun x -> x + 1) [1;2;3] = [2;3;4])

let map f l = 
	fold_left (fun x y -> x @ [(f y)]) [] l

let _ = assert (map (fun x -> x + 1) [1;2;3] = [2;3;4])


(* --------------------------------------- *)

let filter f l =
	fold_right (
		fun x y -> 
		if f x 
		then x::y 
		else y
				) l []
;;

let _ = assert (filter (fun x -> x > 0) [-1;0;1;2] = [1;2])

let filter f l =
	fold_left (
		fun x y ->
		if f y
		then x @ [y]
		else x
	) [] l

let _ = assert (filter (fun x -> x > 0) [-1;0;1;2] = [1;2])

(* --------------------------------------- *)

(* Exercise: Same as above but for "uniqify",
 which removes duplicate elements from a sorted int list.
 You can assume the input list is sorted, 
 and you must output a sorted list. 
 HINT: one of your implementations may need to use last from hw1.
*)

let uniqify : int list -> int list = fun l -> fold_right (fun x y -> match y with
	| [] -> x::[]
	| hd::_ when hd = x -> y
	| _ -> x::y 
	) l []

let _ = assert (uniqify [1;2;2;2;3;4] = [1;2;3;4])

let uniqify : int list -> int list = fun l -> fold_left (fun x y ->
	let rec last (l: 'a list) : 'a option =
	  match l with
	  | [] -> None
	  | hd::[] -> Some hd
	  | hd::tl -> last tl in
 match x with
	| [] -> x @ [y]
	| _::_ when Some y = last x -> x
	| _ -> x @ [y]
	) [] l

let _ = assert (uniqify [1;2;2;2;3;4] = [1;2;3;4])

(* --------------------------------------- *)

let rec fold_right f lst initial = 
	match lst with
	| [] -> initial
	| hd::tl -> f hd (fold_right f tl initial)

let _ = assert (fold_right (+) [1;2;3] 0 = 6)

(* --------------------------------------- *)

let rec fold_left f initial lst = 
	match lst with
	| [] -> initial
	| hd::tl -> fold_left f (f initial hd) tl

let _ = assert (fold_left (+) 0 [1;2;3] = 6)