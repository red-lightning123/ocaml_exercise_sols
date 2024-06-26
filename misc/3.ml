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

(* should receive a tree as input *)
let von_koch g =
        let node_cnt = List.length g.nodes in
        let node_label_range = List.init node_cnt (fun x -> x + 1) in
        let edges = strip_node_names g in
        let add node label node_labels edge_labels =
                let neighbor_label neighbor =
                        List.find_map (fun (n_node, n_label) -> if n_node = neighbor then Some n_label else None) node_labels in
                let neighbors = List.filter_map (fun (a, b) -> if node = a then Some b else if node = b then Some a else None) edges in
                let edge_diffs =
                        List.filter_map
                        (fun neighbor ->
                                Option.map (fun neighbor_label -> Int.abs (label - neighbor_label)) (neighbor_label neighbor))
                        neighbors in
                let merged_edge_labels = List.rev_append edge_diffs edge_labels in
                if List.exists (fun diff -> List.fold_left (fun acc lab -> if diff = lab then acc + 1 else acc) 0 merged_edge_labels > 1) edge_diffs then
                        None
                else
                        Some ((node, label)::node_labels, List.rev_append edge_diffs edge_labels)
        in
        let rec aux node node_labels edge_labels =
                let fold_step (ans, prev_rev, next) curr =
                        let ans =
                                if Option.is_some ans then ans
                                else
                                        match add node curr node_labels edge_labels with
                                        | None -> None
                                        | Some (node_labels, edge_labels) -> aux (node + 1) node_labels edge_labels (List.rev_append prev_rev (List.tl next))
                        in (ans, curr::prev_rev, List.tl next)
                in
                function
                | [] -> Some node_labels
                | (_ :: t) as left -> let ans, _, _ = List.fold_left fold_step (None, [], left) left in ans
        in
        Option.map (fun node_labels -> List.map (fun (node, label) -> (List.nth g.nodes node, label)) node_labels) (aux 0 [] [] node_label_range);;

let g1 = {
        nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'];
        edges = [('a', 'b'); ('a', 'd'); ('a', 'g'); ('b', 'e'); ('b', 'c'); ('e', 'f')]
};;

let g2 = {
        nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k'; 'm'; 'n'; 'p'; 'q'];
        edges = [('a', 'b'); ('a', 'c'); ('a', 'h'); ('a', 'i'); ('a', 'g'); ('d', 'c'); ('d', 'k'); ('e', 'q'); ('e', 'c'); ('f', 'c'); ('q', 'm'); ('q', 'n'); ('p', 'n')]
};;

von_koch g1;;
von_koch g2;;
