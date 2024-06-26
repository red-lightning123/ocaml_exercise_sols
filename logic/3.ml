let rec gray n =
        let prepend_all a l = List.map (fun x -> a ^ x) l in
        if n = 0 then [""]
        else
                let next = gray (n-1) in
                (prepend_all "0" next) @ (prepend_all "1" (List.rev next));;


gray 1;;
gray 2;;
gray 3;;
