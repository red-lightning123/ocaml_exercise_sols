type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let leaves tree =
        let rec aux tree r = match tree with
        | Empty -> r
        | Node (v, Empty, Empty) -> v::r
        | Node (_, left, right) -> aux left (aux right r) in
        aux tree [];;

leaves Empty;;
