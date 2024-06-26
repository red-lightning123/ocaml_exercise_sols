(* requires ocaml version 4.14 minimum *)

let rec points n =
        let aux x =
                Seq.unfold (fun y -> Some ((x, y), y + 1)) 0 in
        Seq.unfold (fun n -> Some (aux n, n + 1)) n;;

let seq = points 0;;

List.map (fun seq -> List.of_seq (Seq.take 5 seq)) (List.of_seq (Seq.take 5 seq));;

let diag seq =
        Seq.unfold (
                fun (seq, n) ->
                        match Seq.uncons seq with 
                        | None -> None
                        | Some (hd_seq, tl) ->
                                let hd_seq = Seq.drop n hd_seq in
                                match Seq.uncons hd_seq with
                                | None -> None
                                | Some (hd, _) -> Some (hd, (tl, n + 1))
        ) (seq, 0);;


List.of_seq (Seq.take 5 (diag seq));;
