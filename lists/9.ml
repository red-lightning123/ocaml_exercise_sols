let pack l =
        let rec aux a b = function
                | [] -> b
                | [h] -> (h::a) :: b
                | h1 :: h2 :: t -> if h1 = h2 then aux (h1::a) b (h2::t) else aux [] ((h1::a)::b) (h2::t)
        in List.rev (aux [] [] l);;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
