type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec preorder = function
        | Empty -> []
        | Node (v, l, r) -> v :: ((preorder l) @ (preorder r));;

let rec inorder = function
        | Empty -> []
        | Node (v, l, r) -> (inorder l) @ (v :: (inorder r));;

let rec split_on r1 x = function
        | [] -> raise Not_found
        | h :: t -> if h = x then List.rev r1, t else split_on (h::r1) x t;;

let contains x = List.exists (fun y -> x = y);;

let rec pre_in_tree p i = match p with
        | [] -> Empty
        | h :: t ->
                let li, ri = split_on [] h i in
                let lp, rp = List.partition (fun x -> contains x li) t in
                Node (h, pre_in_tree lp li, pre_in_tree rp ri);;

preorder (Node (1, Node (2, Empty, Empty), Empty));;

let x = Node (1, Node (2, Empty, Node (3, Empty, Empty)), Empty);;

pre_in_tree (preorder x) (inorder x);;
