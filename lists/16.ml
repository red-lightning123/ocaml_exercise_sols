let drop l n =
        let rec aux c b = function
                | [] -> b
                | h :: t -> if c = 1 then aux n b t else aux (c-1) (h::b) t
        in
        List.rev (aux n [] l);;
drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
