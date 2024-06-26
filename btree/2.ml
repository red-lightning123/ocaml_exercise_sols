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
