let map = List.map
let filter = List.filter

let rec prependTo : 'a list -> 'a list -> 'a list = 
	function l1 -> function l2 -> l2 @ l1

let _ = assert (prependTo [] [1] = [1]) 
let _ = assert (prependTo [2;3] [1;2] = [1;2;2;3])

let mapBinOp : ('a -> 'b -> 'c) -> ('a * 'b) list -> 'c list = 
	fun f l ->
	map (fun (x,y) -> f x y) l

let _ = assert(mapBinOp (fun x y -> x+y) [(1,2); (3,4); (5,6)] = [3;7;11])