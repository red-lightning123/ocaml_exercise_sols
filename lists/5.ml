let rev l =
        let rec aux p = function
                | [] -> p
                | h :: t -> aux (h :: p) t
        in aux [] l;;
rev ["a"; "b"; "c"];;
