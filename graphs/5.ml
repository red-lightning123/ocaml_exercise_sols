type ('a, 'b) labeled_graph = {nodes : 'a list;
                               labeled_edges : ('a * 'a * 'b) list};;


(* union find helper functions *)

let uf_create len = Array.init len (fun x -> x);;

let uf_find i uf =
        let rec aux i =
                let parent = uf.(i) in
                if parent = i then
                        i
                else
                        let root = aux parent in
                        uf.(i) <- root;
                        root
        in aux i;;

let uf_union a b uf =
        uf.(uf_find a uf) <- uf_find b uf;;


let strip_node_names g =
        let find_index c l =
                let rec aux i = function
                        | [] -> raise Not_found
                        | h :: t -> if h = c then i else aux (i + 1) t
                in aux 0 l
        in
        List.map (fun (a, b, v) -> (find_index a g.nodes, find_index b g.nodes, v)) g.labeled_edges;;

let enrich_node_names labeled_edges nodes =
        List.map (fun (a, b, v) -> (List.nth nodes a, List.nth nodes b, v)) labeled_edges;;

(*
an alternative less sophisticated implementation would be simply to take
the set of edges with the smallest sum among the results of s_tree
*)
let ms_tree g =
        let uf = uf_create (List.length g.nodes)
        in
        let rec aux left_edges selected_edges = match left_edges with
        | [] -> selected_edges
        | (a, b, v) :: t ->
                if uf_find a uf <> uf_find b uf then
                        begin
                        uf_union a b uf;
                        aux t ((a, b, v)::selected_edges)
                        end
                else
                        aux t selected_edges
        in
        let labeled_edges = strip_node_names g in
        let sorted_edges = List.sort (fun (_, _, v1) (_, _, v2) -> compare v1 v2) labeled_edges
        in enrich_node_names (aux sorted_edges []) g.nodes;;

let g = {nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
         labeled_edges = [('a', 'b', 5); ('a', 'd', 3); ('b', 'c', 2);
                          ('b', 'e', 4); ('c', 'e', 6); ('d', 'e', 7);
                          ('d', 'f', 4); ('d', 'g', 3); ('e', 'h', 5);
                          ('f', 'g', 4); ('g', 'h', 1)]};;

ms_tree g;;
List.fold_left (fun acc (_, _, v) -> acc+v) 0 (ms_tree g);;
