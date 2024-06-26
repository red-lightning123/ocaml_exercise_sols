module type GRAPH = sig
    type node = char
    type t
    val of_adjacency : (node * node list) list -> t
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
  end;;

module M : GRAPH = struct
    type node = char
    module AdjMap = Map.Make (Char)
    type t = node list AdjMap.t
    let of_adjacency l =
            List.fold_left (fun m (k, v) -> AdjMap.add k v m) AdjMap.empty l
    let dfs_list g n =
            let rec aux acc n =
                    let visited =
                            List.exists (fun x -> x = n) acc in
                    if visited then
                            acc
                    else
                            let children =
                                    AdjMap.find n g in
                            List.fold_left aux (n::acc) children
             in List.rev (aux [] n)
    let dfs_fold g n f a =
            List.fold_left f a (dfs_list g n)
    end;;


let g = M.of_adjacency
          ['u', ['v'; 'x'];
           'v',      ['y'];
           'w', ['z'; 'y'];
           'x',      ['v'];
           'y',      ['x'];
           'z',      ['z'];
          ];;

let cnt_reachable = M.dfs_fold g 'w' (fun a x -> Printf.printf "%c" x; a + 1) 0;;
