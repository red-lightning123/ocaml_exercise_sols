type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let at_level tree level =
        let rec aux tree level r = match tree with
        | Empty -> r
        | Node (v, left, right) -> if level = 1 then v :: r else aux left (level - 1) (aux right (level - 1) r) in
        aux tree level [];;

let example_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
       Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;

at_level example_tree 2;;
