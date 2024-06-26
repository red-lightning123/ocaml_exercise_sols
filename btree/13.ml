type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec height = function
        | Empty -> 0
        | Node(_, l, r) -> 1 + max (height l) (height r);;

let layout_binary_tree_2 tree =
        let rec aux level jump x = function
                | Empty -> Empty
                | Node (v, left, right) ->
                        let tleft = aux (level + 1) (jump / 2) (x - jump) left in
                        let tright = aux (level + 1) (jump / 2) (x + jump) right in
                        Node ((v, x, level), tleft, tright) in
        let initial_jump = 1 lsl (height tree - 2)
        in aux 1 initial_jump 15 tree;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('e', leaf 'd', leaf 'g')),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, leaf 'q'), Empty));;

layout_binary_tree_2 example_layout_tree;;
