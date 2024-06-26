type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let max_nodes h = 1 lsl h - 1;;
let rec min_nodes h =
        if h = 0 then 0
        else if h = 1 then 1
        else 1 + min_nodes (h-1) + min_nodes (h-2);;

let rec level n =
        if n = 1 then 0
        else level (n/2);;

let min_height n =
        let rec aux h =
                if n <= max_nodes h then h
                else aux (h + 1)
        in
        aux 0;;

let max_height n =
        let rec aux h =
                if n < min_nodes (h+1) then h
                else aux (h + 1)
        in
        aux 0;;

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

let rec count_nodes = function
        | Empty -> 0
        | Node(_, left, right) -> 1 + (count_nodes left) + (count_nodes right);;

let hbal_tree_nodes n =
        let min = min_height n in
        let max = max_height n in
        let rec aux h =
                if h > max then []
                else (aux (h+1)) @ (List.filter (fun tree -> count_nodes tree = n) (hbal_tree h))
        in
        aux min;;

List.length (hbal_tree_nodes 15);;
