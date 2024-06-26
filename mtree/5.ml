type 'a mult_tree = T of 'a * 'a mult_tree list;;

let lispy tree =
        let rec add_child buf t = Buffer.add_char buf ' '; aux buf t
        and
        aux buf (T (v, children)) = match children with
                | [] -> Buffer.add_char buf v
                | children ->
                        Buffer.add_char buf '(';
                        Buffer.add_char buf v;
                        List.iter (add_child buf) children;
                        (*List.fold_left (fun acc child -> aux child acc) l children;*)
                        Buffer.add_char buf ')'
        in let buf = Buffer.create 0 in aux buf tree; Buffer.contents buf;;

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
          T ('b', [T ('d', []); T ('e', [])])]);;

lispy (T ('a', []));;
lispy (T ('a', [T ('b', [])]));;
lispy t;;
