type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;



let layout_binary_tree_1 tree =
        let rec aux level x = function
                | Empty -> (Empty, x)
                | Node (v, left, right) ->
                        let tleft, xl = aux (level + 1) x left in
                        let tright, xr = aux (level + 1) (xl + 1) right in
                        (Node ((v, xl + 1, level), tleft, tright), xr)
        in
        let t, _ = aux 1 0 tree in t;;


let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty));;

layout_binary_tree_1 example_layout_tree;;
