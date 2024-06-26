let rec fold_until f i r = function
        | [] -> (r, [])
        | h :: t as l -> if i = 0 then (r, l) else fold_until f (i-1) (f h r) t;;
let slice l i k =
        let _, dropped = fold_until (fun h r -> []) i [] l
        in
        let sliced, _ = fold_until (fun h r -> h::r) (k - i + 1) [] dropped
        in
        List.rev sliced;;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
