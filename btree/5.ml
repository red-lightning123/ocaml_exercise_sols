type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec node_all_inner b = function
        | [] -> []
        | h::t -> Node ('x', h, b) :: node_all_inner b t;;

let rec node_all a = function
        | [] -> []
        | h::t -> (node_all_inner h a) @ (node_all a t);;

let rec hbal_tree h =
        if h < 0 then []
        else if h = 0 then [Empty]
        else
                let hm2 = hbal_tree (h-2) in
                let hm1 = hbal_tree (h-1) in
                (node_all hm1 hm1) @
                (node_all hm1 hm2) @ (node_all hm2 hm1);;

let t = hbal_tree 3;;
