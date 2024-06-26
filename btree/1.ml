type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

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

let example_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
       Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;

cbal_tree 4;;
