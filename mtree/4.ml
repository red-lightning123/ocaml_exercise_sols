type 'a mult_tree = T of 'a * 'a mult_tree list;;

let bottom_up tree =
        let rec aux (T (v, children)) l = v :: (List.fold_left (fun acc child -> aux child acc) l children)
        in List.rev (aux tree []);;

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
          T ('b', [T ('d', []); T ('e', [])])]);;

bottom_up (T ('a', [T ('b', [])]));;
bottom_up t;;
