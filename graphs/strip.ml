(* this is just helper code, it's not an actual question *)

type ('a, 'b) labeled_graph = {nodes : 'a list;
                               labeled_edges : ('a * 'a * 'b) list};;
(* not labeled *)

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

(* labeled *)

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
