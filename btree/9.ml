type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let internals tree =
        let rec aux tree r = match tree with
        | Empty | Node (_, Empty, Empty) -> r
        | Node (v, left, right) -> aux left (aux right (v :: r)) in
        aux tree [];;

internals (Node ('a', Empty, Empty));;
