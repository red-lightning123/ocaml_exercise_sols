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

construct [3; 2; 5; 7; 1];;

is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]);;
not (is_symmetric (construct [3; 2; 5; 7; 4]));;
