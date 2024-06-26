type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* inefficient implementation based on string concatenation *)
let rec tree_dotstring_to_concat = function
        | Empty -> "."
        | Node (v, l, r) -> (String.make 1 v) ^ (tree_dotstring_to_concat l) ^ (tree_dotstring_to_concat r);;

let tree_dotstring_to tree =
        let rec aux b = function
                | Empty -> Buffer.add_char b '.';
                | Node (v, l, r) ->
                                Buffer.add_char b v;
                                aux b l;
                                aux b r
        in let b = Buffer.create 0 in aux b tree; Buffer.contents b;;

let tree_dotstring_from s =
        let parse_dot s at =
                if s.[at] = '.' then true, at + 1 else false, at
        in
        let parse_val s at =
                s.[at], at + 1
        in
        let rec parse_tree s at =
                let succ, at = parse_dot s at in
                if succ then Empty, at
                else
                        let v, at = parse_val s at in
                        let l, at = parse_tree s at in
                        let r, at = parse_tree s at in
                        Node (v, l, r), at
        in
        let tree, _ = parse_tree s 0 in tree;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('e', leaf 'd', leaf 'g')),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, leaf 'q'), Empty));;

tree_dotstring_from (tree_dotstring_to example_layout_tree);;
