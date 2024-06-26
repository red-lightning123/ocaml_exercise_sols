let rec gcd a b = if a = 0 then b else gcd (b mod a) a;;
let coprime a b = gcd a b = 1;;
let phi n =
        let rec aux i r = if i = 0 then r else aux (i-1) (r + (if coprime n i then 1 else 0)) in
        aux n 0;;

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

let rec pow n t = if t = 0 then 1 else n * (pow n (t-1));;

let phi_improved n =
        let rec aux = function
                | [] -> 1
                | (p, m) :: t -> (p-1) * (pow p (m-1)) * (aux t) in
        aux (factors n);;

#load "unix.cma";;

let timeit f n =
        let t0 = Unix.gettimeofday() in
        ignore(f n);
        let t1 = Unix.gettimeofday() in
        t1 -. t0;;

timeit phi 10090;;
timeit phi_improved 10090;;
