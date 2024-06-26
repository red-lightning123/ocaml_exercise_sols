let duplicate l =
        let rec aux b = function
                | [] -> b
                | h :: t -> aux (h :: h :: b) t
        in
        List.rev (aux [] l);;
duplicate ["a"; "b"; "c"; "c"; "d"];;
