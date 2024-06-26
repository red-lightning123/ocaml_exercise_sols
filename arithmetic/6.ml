let encode l =
        let rec aux a b = function
                | [] -> b
                | [h] -> (h, a+1) :: b
                | h1 :: h2 :: t ->
                                if h1 = h2 then aux (a+1) b (h2::t)
                                else aux 0 ((h1, a+1)::b) (h2::t)
        in List.rev (aux 0 [] l);;

let rec factors n =
        let rec aux n d =
                if n = 1 then []
                else if n mod d = 0 then d :: (aux (n / d) d)
                else aux n (d+1) in
        encode (aux n 2);;

factors 315;;
