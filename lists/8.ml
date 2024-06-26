let compress l =
        let rec aux a = function
                | [] -> a
                | [h1] -> h1::a
                | h1 :: h2 :: t -> if h1 = h2 then aux a (h2 :: t) else aux (h1 :: a) (h2 :: t)
        in List.rev (aux [] l);;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
