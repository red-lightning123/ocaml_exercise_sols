type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

let strip_node_names g =
        let find_index c l =
                let rec aux i = function
                        | [] -> raise Not_found
                        | h :: t -> if h = c then i else aux (i + 1) t
                in aux 0 l
        in
        List.map (fun (a, b) -> (find_index a g.nodes, find_index b g.nodes)) g.edges;;

let enrich_node_names edges nodes =
        List.map (fun (a, b) -> (List.nth nodes a, List.nth nodes b)) edges;;

(* current version *)
module Edge = struct
        type t = int * int
        let compare = compare
end
module EdgeSet = Set.Make (Edge);;
let isomorphic g1 g2 =
        let g1_node_cnt = List.length g1.nodes in
        let g2_node_cnt = List.length g2.nodes in
        let node_cnt = max g1_node_cnt g2_node_cnt in
        let nodes = List.init node_cnt (fun x -> x) in
        let g1_edges = strip_node_names g1 in
        let g2_edges = strip_node_names g2 in
        if List.length g1_edges = List.length g2_edges then
                let normalize_edge (a, b) =
                        if a < b then a, b else b, a in
                let g2_edges = List.map normalize_edge g2_edges in
                let g2_edges = EdgeSet.of_list g2_edges in
                let check_iso f v =
                        let f = List.rev f in
                        let edges = List.filter (fun (a, b) -> a = v || b = v) g1_edges in
                        let edges = List.filter_map (fun (a, b) -> match List.nth_opt f a, List.nth_opt f b with | Some fa, Some fb -> Some (fa, fb) | _ -> None) edges in
                        let edges = List.map normalize_edge edges in
                        List.for_all (fun edge -> EdgeSet.mem edge g2_edges) edges
                in
                (* assumes the node iterator is of the form 0..len *)
                let rec aux f = function
                        | [] -> List.for_all (check_iso f) nodes
                        | (h :: t) as left ->
                                        let succ, _, _ =
                                                List.fold_left (
                                                        fun (ans, prev_rev, next) curr ->
                                                                let ans =
                                                                        if ans then true
                                                                        else if check_iso (curr::f) h then aux (curr::f) (List.rev_append prev_rev (List.tl next))
                                                                        else false
                                                                in (ans, curr::prev_rev, List.tl next)) (false, [], left) left in succ
                in aux [] nodes
        else false;;

(* slightly improved version *)
let isomorphic2 g1 g2 =
        let g1_node_cnt = List.length g1.nodes in
        let g2_node_cnt = List.length g2.nodes in
        let node_cnt = max g1_node_cnt g2_node_cnt in
        let nodes = List.init node_cnt (fun x -> x) in
        let g1_edges = strip_node_names g1 in
        let g2_edges = strip_node_names g2 in
        let normalize_edge (a, b) =
                if a < b then a, b else b, a in
        let g2_edges = List.sort compare (List.map (fun (a, b) -> normalize_edge (a, b)) g2_edges) in
        let check_iso f =
                let g1_edges = List.sort compare (List.map (fun (a, b) -> normalize_edge (List.nth f a, List.nth f b)) g1_edges) in
                g1_edges = g2_edges
        in
        let rec aux f = function
                | [] -> check_iso f
                | (_ :: t) as left -> let succ, _, _ = List.fold_left (fun (ans, prev_rev, next) curr -> let ans = if ans then true else aux (curr::f) (List.rev_append prev_rev (List.tl next)) in (ans, curr::prev_rev, List.tl next)) (false, [], left) left in succ
        in aux [] nodes;;

(* original inefficient version *)
let isomorphic3 g1 g2 =
        let g1_node_cnt = List.length g1.nodes in
        let g2_node_cnt = List.length g2.nodes in
        let node_cnt = max g1_node_cnt g2_node_cnt in
        let nodes = List.init node_cnt (fun x -> x) in
        let g1_edges = strip_node_names g1 in
        let g2_edges = strip_node_names g2 in
        let adjacent edges a b =
                List.exists (fun (ea, eb) -> (ea = a && eb = b) || (ea = b && eb = a)) edges
        in
        let find_index c l =
                let rec aux i = function
                        | [] -> raise Not_found
                        | h :: t -> if h = c then i else aux (i + 1) t
                in aux 0 l
        in
        let check_iso f =
                List.for_all (fun (a, b) -> adjacent g2_edges (List.nth f a) (List.nth f b)) g1_edges &&
                List.for_all (fun (a, b) -> adjacent g1_edges (find_index a f) (find_index b f)) g2_edges
        in
        let rec aux f = function
                | [] -> check_iso f
                | (_ :: t) as left -> let succ, _, _ = List.fold_left (fun (ans, prev_rev, next) curr -> let ans = if ans then true else aux (curr::f) (List.rev_append prev_rev (List.tl next)) in (ans, curr::prev_rev, List.tl next)) (false, [], left) left in succ
        in aux [] nodes;;

(* for testing *)
let perm_graph p g =
        let find_index c l =
                let rec aux i = function
                        | [] -> raise Not_found
                        | h :: t -> if h = c then i else aux (i + 1) t
                in aux 0 l
        in
        let map_perm p a =
                List.nth g.nodes (List.nth p (find_index a g.nodes)) in
        let nodes = List.map (fun node -> map_perm p node) g.nodes in
        let edges = List.map (fun (a, b) -> (map_perm p a, map_perm p b)) g.edges in
        { nodes; edges };;

let g1 = {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
         edges = [(1, 5); (1, 6); (1, 7); (2, 5); (2, 6); (2, 8); (3, 5);
                  (3, 7); (3, 8); (4, 6); (4, 7); (4, 8)]};;

let g2 = {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
         edges = [(1, 4); (1, 6); (1, 7); (2, 5); (2, 6); (2, 8); (3, 5);
                  (3, 7); (3, 8); (4, 6); (4, 7); (4, 8)]};;
let g3 = perm_graph [1; 3; 5; 6; 4; 2; 7; 0] g1;; (* somewhat random permutation of g1 - isomorphic to it *)

isomorphic g1 g2;;
isomorphic g1 g3;;
isomorphic g2 g3;;
