type 'a mult_tree = T of 'a * 'a mult_tree list;;

let ipl tree =
        let rec aux depth (T (_, children)) = depth + List.fold_left (fun acc child -> acc + aux (depth + 1) child) 0 children
        in aux 0 tree;;

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
          T ('b', [T ('d', []); T ('e', [])])]);;

ipl t;;
