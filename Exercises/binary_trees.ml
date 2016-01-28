type tree = Leaf
			| TreeNode of tree * int * tree

let t1 = TreeNode(Leaf, 1, Leaf)
let t3 = TreeNode (Leaf, 3, Leaf)
let t123 = TreeNode (t1, 2, t3)

let t5 = TreeNode (Leaf, 5, Leaf)
let t7 = TreeNode (Leaf, 7, Leaf)
let t567 = TreeNode (t5, 6, t7)

let rec tree_to_list (t:tree) : int list = 
	match t with
	| Leaf -> []
	| TreeNode (Leaf, e, r) -> e :: tree_to_list r
	| TreeNode (l, e, r) -> tree_to_list l @ [e] @ tree_to_list r

let _ = assert (tree_to_list Leaf = [])
let _ =	assert (tree_to_list (TreeNode (Leaf, 1, Leaf)) = [1])
let _ = assert (tree_to_list (TreeNode (t123, 4, t567)) = [1;2;3;4;5;6;7])

let rec is_binary_search_tree (t:tree) : bool = 
	let rec all_gt (t, num) = 
	match t with
	| Leaf -> true
	| TreeNode (l, e, r) when e > num -> all_gt(l, num) && all_gt(r, num)
	| _ -> false 
	in
	let rec all_leq (t, num) =
	match t with
	| Leaf -> true
	| TreeNode (l, e, r) when e <= num -> all_leq(l, num) && all_leq(r, num)
	| _ -> false
	in
	match t with
	| Leaf -> true
	| TreeNode (Leaf, e, r) -> all_gt(r, e)
	| TreeNode (l, e, Leaf) -> all_leq(l, e)
	| TreeNode (l,e,r) -> all_leq(l, e) && all_gt(r, e)

let _ = assert (is_binary_search_tree Leaf = true)
let _ = assert (is_binary_search_tree t567 = true)
let _ = assert (is_binary_search_tree (TreeNode (t7, 6, t5)) = false)

let rec insert ((x, t) : int * tree) : tree = 
	match t with
	| Leaf -> TreeNode(Leaf, x, Leaf)
	| TreeNode (l, e, r) when x <= e -> TreeNode(insert(x, l), e, r)
	| TreeNode (l, e, r) when x > e -> TreeNode(l, e, insert(x, r))
	| _ -> Leaf

let _ = assert (insert (1, Leaf) = TreeNode(Leaf, 1, Leaf))
let _ = assert (insert (1, TreeNode(Leaf, 1, Leaf)) = TreeNode(TreeNode(Leaf, 1, Leaf), 1, Leaf))
let _ = assert (insert (2, TreeNode(Leaf, 1, Leaf)) = TreeNode(Leaf, 1, TreeNode(Leaf, 2, Leaf)))


let sort(l : 'a list) : 'a list = 
   let rec helper (lst, t) = 
   	match lst with
   	| [] -> tree_to_list(t)
   	| hd::tl -> helper(tl, insert(hd, t))
   in helper(l, Leaf)

 let _ = assert (sort([7;4;3;6;2;1]) = [1;2;3;4;6;7])