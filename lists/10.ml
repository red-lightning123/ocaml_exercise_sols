let encode l =
        let rec aux a b = function
                | [] -> b
                | [h] -> (a+1, h) :: b
                | h1 :: h2 :: t ->
                                if h1 = h2 then aux (a+1) b (h2::t)
                                else aux 0 ((a+1, h1)::b) (h2::t)
        in List.rev (aux 0 [] l);;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
