type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec string_of_tree = function
        | Empty -> ""
        | Node (v, l, r) ->
                        let v = String.make 1 v in
                        match l, r with
                                | Empty, Empty -> v
                                | l, r -> v ^ "(" ^ string_of_tree l ^ "," ^ string_of_tree r ^ ")";;

let rec tree_of_string s =
        let rec read_value s at = match s.[at] with
                | '(' | ')' | ',' -> (None, at)
                | value -> (Some(value), at + 1)
        in
        let rec read c s at = if s.[at] = c then (true, at + 1) else (false, at)
        in
        let rec aux s at =
                match read_value s at with
                | None, at -> (Empty, at)
                | Some(value), at ->
                        match read '(' s at with
                                | false, at -> (Node (value, Empty, Empty), at)
                                | true, at ->
                                        let l, at = aux s at in
                                        let _, at = read ',' s at in
                                        let r, at = aux s at in
                                        let _, at = read ')' s at in
                                        Node(value, l, r) , at
        in
        let tree, _ = aux s 0 in tree;;

(* doesn't seem like a solution is necessary for the rest of the problem *)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
    (Node ('a', Node ('b', leaf 'd', leaf 'e'),
     Node ('c', Empty, Node ('f', leaf 'g', Empty))));;
