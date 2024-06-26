let range left right =
        let rec aux left right r =
                if left = right then left :: r else aux (left + 1) right (left :: r)
        in
        if left < right then List.rev (aux left right [])
        else aux right left [];;

range 4 9;;
