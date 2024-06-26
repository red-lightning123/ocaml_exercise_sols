(* requires ocaml version 4.14 minimum *)

type 'a cons = Cons of 'a * 'a stream and
'a stream = unit -> 'a cons;;

let hd stream =
        let Cons (hd, _) = stream () in hd;;
let tl stream =
        let Cons (_, tl) = stream () in tl;;

let take n stream =
        let rec aux acc n stream =
                if n = 0 then
                        acc
                else
                        let Cons (hd, tl) = stream () in
                        aux (hd::acc) (n - 1) tl in
        List.rev (aux [] n stream);;

let rec unfold f init =
        fun () ->
                let hd, state = f init in
                Cons (hd, unfold f state);;

let bang x = unfold (fun () -> (x, ())) ();;

let ints n = unfold (fun n -> (n, n + 1)) n;;

let map f stream =
        unfold (fun stream -> let Cons (hd, tl) = stream () in f hd, tl) stream;;

let filter f stream =
        let rec next_filtered f stream =
                let Cons (hd, tl) = stream () in
                if f hd then
                        hd, tl
                else
                        next_filtered f tl in
        unfold (next_filtered f) stream;;

let iter f stream =
        let rec next stream =
                let Cons (hd, tl) = stream () in
                f hd;
                next tl in
        next stream;;

let to_seq stream =
        Seq.unfold (fun stream -> let Cons (hd, tl) = stream () in Some (hd, tl)) stream;;

let of_seq seq =
        unfold (
                fun seq ->
                        match Seq.uncons seq with
                        | None -> raise (Failure "Seq passed to stream.of_seq was finite")
                        | Some (hd, tl) -> hd, tl
        ) seq;;

let rec s = unfold (fun n -> n * n, n + 1) 0;;

hd s;;

tl s;;
hd (tl s);;

take 10 s;;

take 10 (bang 13);;

take 10 (ints 24);;

take 10 (map (fun x -> 2 * x) s);;

(* commented out because iter triggers an infinite loop *)
(* iter (fun n -> Printf.printf "this is %i" n) s;; *)

take 10 (filter (fun x -> x mod 25 = 0) s);;

take 10 (of_seq (to_seq s));;

(* should raise a Failure since the Seq is finite *)
take 10 (of_seq (Seq.unfold (fun n -> if n = 5 then None else Some (n, n + 1)) 0));;
