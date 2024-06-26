type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec is_mirror a b = match a, b with
        | Empty, Empty -> true
        | Node (_, aleft, aright), Node (_, bleft, bright) -> (is_mirror aleft bright) && (is_mirror aright bleft)
        | _ -> false;;

let is_symmetric = function
        | Empty -> true
        | Node (_, left, right) -> is_mirror left right;;

let rec insert_btree tree v = match tree with
        | Empty -> Node (v, Empty, Empty)
        | Node (vt, l, r) ->
                if v = vt then tree
                else if v < vt then Node (vt, insert_btree l v, r)
                else Node (vt, l, insert_btree r v);;

let rec construct l = List.fold_left insert_btree Empty l;;

let rec node_all_inner b = function
        | [] -> []
        | h::t -> Node ('x', h, b) :: node_all_inner b t;;

let rec node_all a = function
        | [] -> []
        | h::t -> (node_all_inner h a) @ (node_all a t);;

let rec cbal_tree n =
        if n = 0 then [Empty]
        else if n mod 2 = 1 then
                let nover2 = cbal_tree ((n-1)/2) in
                node_all nover2 nover2
        else
                let nm1over2 = cbal_tree ((n-1-1)/2) in
                let np1over2 = cbal_tree ((n-1+1)/2) in
                (node_all nm1over2 np1over2) @
                (node_all np1over2 nm1over2);;

let rec sym_cbal_trees n = List.filter is_symmetric (cbal_tree n);;

let rec cbal_tree_cnt n =
        if n = 0 then 1
        else if n mod 2 = 0 then 2 * cbal_tree_cnt ((n-2)/2) * cbal_tree_cnt (n/2)
        else let x = cbal_tree_cnt ((n-1)/2) in x * x;;

let sym_cbal_tree_cnt n = if n mod 2 = 0 then 0 else cbal_tree_cnt ((n-1)/2);;

sym_cbal_trees 5;;
List.length (sym_cbal_trees 57);;
sym_cbal_tree_cnt 57;;
