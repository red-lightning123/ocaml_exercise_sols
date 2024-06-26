type 'a mult_tree = T of 'a * 'a mult_tree list;;

let string_of_tree tree =
        let rec aux b (T(v, children)) =
                Buffer.add_char b v;
                List.iter (aux b) children;
                Buffer.add_char b '^'
        in let b = Buffer.create 0 in aux b tree; Buffer.contents b;;

let tree_of_string s =
        let parse_caret s at =
                if String.length s = at then false, at
                else if s.[at] = '^' then true, at + 1
                else false, at in
        let parse_val s at =
                if String.length s = at then None, at
                else if s.[at] = '^' then None, at
                else Some (s.[at]), at + 1 in
        let rec parse_children s at =
                let tree, at = parse_tree s at in
                match tree with
                | None -> [], at
                | Some (tree) -> let next, at = parse_children s at in (tree::next), at
        and
        parse_tree s at =
                let v, at = parse_val s at in
                match v with
                | None -> None, at
                | Some v ->
                        let children, at = parse_children s at in
                        let succ, at = parse_caret s at in
                        if succ then
                                Some (T (v, children)), at
                        else
                                None, at
        in
        let tree, at = parse_tree s 0 in Option.get tree;;

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
          T ('b', [T ('d', []); T ('e', [])])]);;

tree_of_string (string_of_tree t);;
