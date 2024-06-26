type 'a expr_tree =
  | Term of float
  | Node of 'a * 'a expr_tree * 'a expr_tree;;

(* although the requirement was to find one solution, this finds all of them *)
let arithmetic_puzzle l =
        let expr_possibilities e =
                let rec aux (acc, prev, next) _ =
                        match next with
                        | [] -> raise Not_found
                        | h :: t ->
                                let a = gen_rec (List.rev (h::prev)) in
                                let b = gen_rec t in
                                let merged = List.fold_left (fun acc av -> List.fold_left (fun acc bv -> (av, bv)::acc) acc b) [] a in
                                let acc = acc @ List.map (fun (a, b) -> Node ('+', a, b)) merged in
                                let acc = acc @ List.map (fun (a, b) -> Node ('-', a, b)) merged in
                                let acc = acc @ List.map (fun (a, b) -> Node ('*', a, b)) merged in
                                let acc = acc @ List.map (fun (a, b) -> Node ('/', a, b)) merged in
                                let prev = h::prev in
                                let next = t in
                                acc, prev, next
                and
                gen_rec e =
                        match e with
                        | [] -> raise Not_found
                        | [h] -> [h]
                        | _ :: _ ->
                                let acc, _, _ = List.fold_left aux ([], [], e) (List.init (List.length e - 1) (fun x -> x)) in
                                acc
                in
                let e = List.map (fun x -> Term (float_of_int x)) e in
                gen_rec e
        in
        let rec expr_value e =
                match e with
                | Term x -> Some x
                | Node (op, e1, e2) ->
                        match op, expr_value e1, expr_value e2 with
                                | (_, None, _) -> None
                                | (_, _, None) -> None
                                | ('+', Some e1_val, Some e2_val) -> Some (e1_val +. e2_val)
                                | ('-', Some e1_val, Some e2_val) -> Some (e1_val -. e2_val)
                                | ('*', Some e1_val, Some e2_val) -> Some (e1_val *. e2_val)
                                | ('/', Some e1_val, Some e2_val) -> if e2_val = 0.0 then None else Some (e1_val /. e2_val)
                                | _ -> raise Not_found
        in
        let filter_val e =
                match expr_value e with
                | None -> None
                | Some e_val -> Some (e, e_val)
        in
        let exprs e1 e2 =
                let e1 = expr_possibilities e1 in
                let e2 = expr_possibilities e2 in
                let e1_with_values = List.filter_map filter_val e1 in
                let e2_with_values = List.filter_map filter_val e2 in
                List.concat (List.map (fun (e1, e1_val) -> List.filter_map (fun (e2, e2_val) -> if e1_val = e2_val then Some (e1, e2) else None) e2_with_values) e1_with_values)
        in
        let rec stringify_expr prec e =
                match e with
                | Term x -> string_of_int (int_of_float x)
                | Node (op, e1, e2) ->
                        let implied_order = match op, prec with
                                | (_, ('=', _)) -> true
                                | ('*', ('+', _)) | ('*', ('-', _)) | ('*', ('*', _)) -> true
                                | ('/', ('+', _)) | ('/', ('-', _)) -> true
                                | ('+', ('+', _)) | ('+', ('-', false)) -> true
                                | ('-', ('+', _)) | ('-', ('-', false)) -> true
                                | _ -> false in
                        if implied_order then
                                (stringify_expr (op, false) e1) ^ (String.make 1 op) ^ (stringify_expr (op, true) e2)
                        else
                                "(" ^ (stringify_expr (op, false) e1) ^ (String.make 1 op) ^ (stringify_expr (op, true) e2) ^ ")"
        in
        let stringify_eq (expr1, expr2) =
                (stringify_expr ('=', false) expr1) ^ "=" ^ (stringify_expr ('=', true) expr2) in
        let count x l =
                List.fold_left (fun acc y -> if x = y then acc + 1 else acc) 0 l in
        let equal_sign l =
                let aux (acc, prev, next) _ =
                        match next with
                        | [] -> raise Not_found
                        | h :: t ->
                                let prev = h::prev in
                                let next = t in
                                let acc = List.rev_append acc (exprs (List.rev prev) next) in (* order doesn't matter *)
                                acc, prev, next
                in
                let statements, _, _ = List.fold_left aux ([], [], l) (List.init (List.length l - 1) (fun x -> x)) in
                let statements = List.map (fun s -> stringify_eq s) statements in
                let statements = List.filter (fun s -> count s statements = 1) statements in
                statements
        in equal_sign l;;


arithmetic_puzzle [2; 3; 5; 7; 11];;
