let rec factors n =
        let rec aux n d =
                if n = 1 then []
                else if n mod d = 0 then d :: (aux (n / d) d)
                else aux n (d+1) in
        aux n 2;;

factors 315;;
