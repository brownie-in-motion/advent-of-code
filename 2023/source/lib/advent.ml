(* my prelude *)
module P : sig
    val (|>) : 'a -> ('a -> 'b) -> 'b
    val (>>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
    val (%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
    val (let*) : 'a option -> ('a -> 'b option) -> 'b option
    val test : int
end = struct
    let (|>) x f = f x
    let (>>) f g x = g (f x)
    let (%) f g x = f (g x)
    let (let*) x f = Option.bind x f
    let test = 1
end

open P

(* function helpers*)
module F : sig
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
end = struct
    let uncurry f (a, b) = f a b
end

(* list helpers *)
module L : sig
    val hd : 'a list -> 'a option
    val span : ('a -> bool) -> 'a list -> 'a list * 'a list
end = struct
    let hd x = match x with
        | [] -> None
        | x :: _ -> Some x
    let rec span f x = match x with
        | [] -> ([], [])
        | c :: cs -> if f c
            then let (l, r) = span f cs in (c :: l, r)
            else ([], c :: cs)
end

(* option helpers *)
module O : sig
    val map2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
    val or_else : 'a option -> 'a option -> 'a option
    val sequence : ('a option) list -> ('a list) option
end = struct
    let map2 f a b = Option.bind a (fun a -> Option.map (f a) b)
    let or_else a b = match a with
        | None -> b
        | x -> x
    let rec sequence l = match l with
        | [] -> Some []
        | Some x :: xs -> let* next = sequence xs in Some (x :: next)
        | None :: _ -> None
end

(* string helpers *)
module S : sig
    val reverse : string -> string
    val to_list : string -> char list
    val from_list : char list -> string
end = struct
    let to_list s = List.init (String.length s) (String.get s)
    let from_list = List.map Char.escaped >> String.concat ""
    let reverse = to_list
        >> List.rev
        >> List.map Char.escaped
        >> String.concat ""
end

(* tuple helpers *)
module T : sig
    val apply3
        : ('a -> 'b -> 'c)
        -> ('a * 'a * 'a)
        -> ('b * 'b * 'b)
        -> ('c * 'c * 'c)
    val pair : 'a -> 'b -> ('a * 'b)
end = struct
    let apply3 f (a, b, c) (x, y, z) = (f a x, f b y, f c z)
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
