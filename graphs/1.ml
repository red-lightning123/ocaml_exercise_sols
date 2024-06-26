type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;
type 'a adj_node =
        | N of 'a * 'a list;;

module Edge = struct
        type t = char * char
        let compare = compare
end

module Node = struct
        type t = char
        let compare = compare
end

module NodeSet = Set.Make (Node);;
module EdgeSet = Set.Make (Edge);;
module EntryMap = Map.Make (Node);;

(*

ec = edge clause
tg = term graph
al = adjacency list
hf = human friendly

there are conversion functions between tg and every other format, in both directions, therefore
any other conversion may be done through tg

*)


let ec_to_tg l =
        let collect_edge_nodes acc (a, b) =
                let acc = NodeSet.add a acc in
                let acc = NodeSet.add b acc in
                acc
        in
        let nodes = List.fold_left collect_edge_nodes NodeSet.empty l in
        let edges = List.sort compare (List.map (fun (a, b) -> if a < b then a, b else b, a) l) in
        { nodes = NodeSet.elements nodes; edges };;

let try_tg_to_ec graph =
        let collect_edge_nodes acc (a, b) =
                let acc = NodeSet.add a acc in
                let acc = NodeSet.add b acc in
                acc
        in
        let edge_nodes = List.fold_left collect_edge_nodes NodeSet.empty graph.edges in
        List.iter (fun node -> if Option.is_none (NodeSet.find_opt node edge_nodes) then raise Not_found) graph.nodes;
        graph.edges;;

let al_to_tg l =
        let collect_entry_nodes acc (N (n, conns)) =
                NodeSet.add n acc
        in
        let collect_entry_edges acc (N (n, conns)) =
                let rec collect_edge acc v = EdgeSet.add (if n < v then (n, v) else (v, n)) acc
                in
                List.fold_left collect_edge acc conns
        in
        let nodes = List.fold_left collect_entry_nodes NodeSet.empty l in
        let edges = List.fold_left collect_entry_edges EdgeSet.empty l in
        { nodes = NodeSet.elements nodes; edges = EdgeSet.elements edges }

let tg_to_al graph =
        let collect_node_entries acc node =
                EntryMap.add node NodeSet.empty acc
        in
        let collect_edge_conns acc (a, b) =
                let acc = EntryMap.update a (fun x -> Some (NodeSet.add b (Option.get x))) acc in
                let acc = EntryMap.update b (fun x -> Some (NodeSet.add a (Option.get x))) acc in
                acc
        in
        let entries = List.fold_left collect_node_entries EntryMap.empty graph.nodes in
        let entries = List.fold_left collect_edge_conns entries graph.edges in
        List.map (fun (k, v) -> N (k, NodeSet.elements v)) (EntryMap.bindings entries)

let hf_to_tg s =
        let collect_nodes acc item =
                match String.split_on_char '-' item with
                        | [a] -> NodeSet.add a.[0] acc
                        | [a; b] -> NodeSet.add b.[0] (NodeSet.add a.[0] acc)
                        | _ -> raise Not_found
        in
        let collect_edges acc item =
                match String.split_on_char '-' item with
                        | [a] -> acc
                        | [a; b] -> if a.[0] < b.[0] then EdgeSet.add (a.[0], b.[0]) acc else EdgeSet.add (b.[0], a.[0]) acc
                        | _ -> raise Not_found
        in
        let l = String.split_on_char ' ' s in
        let nodes = List.fold_left collect_nodes NodeSet.empty l in
        let edges = List.fold_left collect_edges EdgeSet.empty l in
        { nodes = NodeSet.elements nodes; edges = EdgeSet.elements edges };;

let tg_to_hf graph =
        let collect_edge_nodes acc (a, b) =
                let acc = NodeSet.add a acc in
                let acc = NodeSet.add b acc in
                acc
        in
        let edge_nodes = List.fold_left collect_edge_nodes NodeSet.empty graph.edges in
        let rec maybe_add_node_str buf node =
                let add_node_str buf node =
                        Buffer.add_char buf node;
                        Buffer.add_char buf ' '
                in
                if Option.is_none (NodeSet.find_opt node edge_nodes) then
                        add_node_str buf node
        in
        let rec add_edge_str buf (a, b) =
                Buffer.add_char buf a;
                Buffer.add_char buf '-';
                Buffer.add_char buf b;
                Buffer.add_char buf ' '
        in
        let aux buf =
                List.iter (maybe_add_node_str buf) graph.nodes;
                List.iter (add_edge_str buf) graph.edges
        in let buf = Buffer.create 0 in aux buf; String.trim (Buffer.contents buf);; (* trim to remove trailing space *)

let ec = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')];;
let tg =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;
let al =
        [N ('b', ['c'; 'f']); N ('c', ['b'; 'f']); N ('d', []); N ('f', ['b'; 'c'; 'k']); N ('g', ['h']); N ('h', ['g']); N ('k', ['f'])];;
let hf = "b-c f-c g-h d f-b k-f h-g";;

tg_to_hf ( ec_to_tg (try_tg_to_ec (ec_to_tg ec)) );;
tg_to_hf ( al_to_tg (tg_to_al (al_to_tg al)) );;
tg_to_hf (hf_to_tg hf);;
