let split l n =
        let reorder a b = (List.rev a, b)
        in
        let rec aux c a = function
                | [] -> reorder a []
                | h :: t -> if c = 0 then reorder a (h::t) else aux (c-1) (h::a) t
        in
        aux n [] l;;
split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
split ["a"; "b"; "c"; "d"] 5;;
