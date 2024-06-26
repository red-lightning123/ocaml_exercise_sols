(*let extract n l =
        let rec aux n current = function
                | [] ->
                        if n = 0 then [List.rev (current)]
                        else []
                | h :: t ->
                        if n = 0 then [List.rev (current)]
                        else (aux n current t) @ (aux (n-1) (h :: current) t)
        in List.rev (aux n [] l);;*)

let rec extract k list =
        if k <= 0 then [[]]
        else match list with
                | [] -> []
                | h :: t ->
                        let with_h = List.map (fun x -> h::x) (extract (k-1) t)
                        in
                        let without_h = extract k t
                        in
                        with_h @ without_h;;

extract 2 ["a"; "b"; "c"; "d"];;
