let identifier s =
        let parse_letter s at =
                let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in
                if at = String.length s then None, at
                else if is_letter s.[at] then Some (), at + 1
                else None, at in
        let parse_digit s at =
                let is_digit c = ('0' <= c && c <= '9') in
                if at = String.length s then None, at
                else if is_digit s.[at] then Some (), at + 1
                else None, at in
        let parse_dash s at =
                let is_dash c = c = '-' in
                if at = String.length s then None, at
                else if is_dash s.[at] then Some (), at + 1
                else None, at in
        let parse_letter_or_digit s _at =
                match parse_letter s _at with
                | None, at ->
                        (match parse_digit s at with
                        | None, at -> None, _at
                        | Some (), at -> Some (), at)
                | Some (), at -> Some (), at in
        let parse_v s _at =
                let _, at = parse_dash s _at in
                match parse_letter_or_digit s at with
                | None, at -> None, _at
                | Some (), at -> Some (), at in
        let rec parse_aux s _at =
                match parse_v s _at with
                | None, at -> None, _at
                | Some (), at ->
                        let _, at = parse_aux s at in Some (), at in
        let parse_ident s _at =
                match parse_letter s _at with
                | None, at -> None, _at
                | Some (), at ->
                        match parse_aux s at with
                        | None, at -> Some (), at
                        | Some (), at -> Some (), at
        in
        match parse_ident s 0 with
        | None, _ -> false
        | Some (), at -> at = String.length s;;

identifier "T";;
identifier "-";;
identifier "t-";;
identifier "t-T4";;
identifier "-4-T4";;
identifier "4-4-T4";;
identifier "t-4-T4";;
identifier "this-is-a-long-identifier";;
