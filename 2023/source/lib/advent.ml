(* my prelude *)
module P : sig
    val (|>) : 'a -> ('a -> 'b) -> 'b
    val (>>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
    val (%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
end = struct
    let (|>) x f = f x
    let (>>) f g x = g (f x)
    let (%) f g x = f (g x)
end
open P

(* list helpers *)
module L : sig
    val hd : 'a list -> 'a option
end = struct
    let hd x = match x with
        | [] -> None
        | x :: _ -> Some x
end

(* option helpers *)
module O : sig
    val map2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
    val or_else : 'a option -> 'a option -> 'a option
end = struct
    let map2 f a b = Option.bind a (fun a -> Option.map (f a) b)
    let or_else a b = match a with
        | None -> b
        | x -> x
end

(* string helpers *)
module S : sig
    val reverse : string -> string
    val to_list : string -> char list
end = struct
    let to_list s = List.init (String.length s) (String.get s)
    let reverse = to_list
        >> List.rev
        >> List.map Char.escaped
        >> String.concat ""
end

(* tuple helpers *)
module T : sig
    val pair : 'a -> 'b -> ('a * 'b)
end = struct
    let pair a b = (a, b)
end

(* aoc helpers *)
module A : sig
    val lines : string -> string list
    val read_char : char -> int option
    val read_string : string -> int option
    val input : int -> string list
end = struct
    let lines file = let rec lines_inner channel =
        try
            let line = input_line channel in
            line :: lines_inner channel
        with e ->
            match e with
                | End_of_file -> []
                | e -> close_in_noerr channel; raise e
    in lines_inner (open_in file)

    let read_char c = if '0' <= c && c <= '9'
        then Some (Char.code c - Char.code '0')
        else None

    let read_string s = List.fold_left
        (O.map2 (fun x y -> 10 * x + y))
        (Some 0)
        (List.map read_char (S.to_list s))

    (* this is pretty bad *)
    let input = Printf.sprintf "../inputs/day-%02d" >> lines
end
