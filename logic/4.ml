type node =
        | Leaf of string
        | Tree of node * node;;

let compare_freqs (a, fa) (b, fb) = compare fa fb;;

let merge (a, fa) (b, fb) t =
        let new_node = (Tree (a, b), fa+fb) in
        let rec aux r = function
                | [] -> (new_node::r)
                | (h :: t) as l ->
                        let c = compare_freqs new_node h in
                        if c = 1 then aux (h::r) t
                        else (List.rev l) @ (new_node::r)
        in
        List.rev (aux [] t);;

let rec huff_tree = function
        | [] -> raise Not_found
        | [(a, fa)] -> a
        | h1 :: h2 :: t -> huff_tree (merge h1 h2 t);;

let prepend_all a l = List.map (fun (x, s) -> (x, a ^ s)) l;;
let rec decode_huff_tree = function
        | Leaf s -> [(s, "")]
        | Tree (a, b) -> (prepend_all "0" (decode_huff_tree a)) @ (prepend_all "1" (decode_huff_tree b));;

let huffman fs =
        let fs = List.sort compare_freqs fs in
        let fs = List.map (fun (a, fa) -> (Leaf a, fa)) fs in
        let tree = huff_tree fs in
        decode_huff_tree tree;;


let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16);
          ("e", 9); ("f", 5)];;
huffman fs;;
