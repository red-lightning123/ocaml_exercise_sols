type 'a node =
        | One of 'a
        | Many of 'a node list;;

let flatten l =
        let rec aux a = function
                | [] -> a
                | (One h) :: t -> aux (h::a) t
                | (Many l) :: t -> aux (aux a l) t
        in List.rev (aux [] l);;

flatten [One "a"; Many [One "b"; Many[One "c"; One "d"]; One "e"]];;
