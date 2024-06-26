(* this is just helper code, it's not an actual question *)

let uf_create len = Array.init len (fun x -> x);;

let uf_find i uf =
        let rec aux i =
                let parent = uf.(i) in
                if parent = i then
                        i
                else
                        let root = aux parent in
                        uf.(i) <- root;
                        root
        in aux i;;

let uf_union a b uf =
        uf.(uf_find a uf) <- uf_find b uf;;
