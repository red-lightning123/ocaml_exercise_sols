let rec last_two = function
        | [] | [_] -> None
        | [a; b] -> Some (a, b)
        | _ :: t -> last_two t ;;

last_two ["a"; "b"; "c"; "d"];;
last_two ["a"];;
