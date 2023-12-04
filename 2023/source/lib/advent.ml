(* my prelude *)
module P : sig
    val (|>) : 'a -> ('a -> 'b) -> 'b
    val (>>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
    val (%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
    val (let*) : 'a option -> ('a -> 'b option) -> 'b option
end = struct
    let (|>) x f = f x
    let (>>) f g x = g (f x)
    let (%) f g x = f (g x)
    let (let*) x f = Option.bind x f
end

open P

(* function helpers*)
module F : sig
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
end = struct
    let uncurry f (a, b) = f a b
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
        | Some x :: xs -> Option.map (List.cons x) (sequence xs)
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

(* list helpers *)
module L : sig
    val hd : 'a list -> 'a option
    val product : 'a list -> 'b list -> ('a * 'b) list
    val range : int -> int -> int list
    val span : ('a -> bool) -> 'a list -> 'a list * 'a list
    val sum : (int list) -> int
    val flatmap : 'a list -> ('a -> 'b list) -> 'b list
end = struct
    let hd x = match x with
        | [] -> None
        | x :: _ -> Some x
    let product xs ys =
        let fst_all l x = List.map (T.pair x) l in
        List.concat_map (fst_all ys) xs
    let rec range (x : int) (y : int) = if x >= y
        then []
        else x :: range (x + 1) y
    let rec span f x = match x with
        | [] -> ([], [])
        | c :: cs -> if f c
            then let (l, r) = span f cs in (c :: l, r)
            else ([], c :: cs)
    let sum = List.fold_left (+) 0
    let flatmap l f = List.concat (List.map f l)
end

(* aoc helpers *)
module A : sig
    val lines : string -> string list
    val is_digit : char -> bool
    val pow : int -> int -> int
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

    let is_digit c = '0' <= c && c <= '9'

    let rec pow x = function
        | 0 -> 1
        | n -> if n mod 2 == 0
            then pow (x * x) (n / 2)
            else x * pow (x * x) ((n - 1) / 2)

    let read_char c = if is_digit c
        then Some (Char.code c - Char.code '0')
        else None

    let read_string s = List.fold_left
        (O.map2 (fun x y -> 10 * x + y))
        (Some 0)
        (List.map read_char (S.to_list s))

    (* this is pretty bad *)
    let input = Printf.sprintf "../inputs/day-%02d" >> lines
end

module Parsing : sig
    type ('a, 'b) parser = 'a list -> ('b * 'a list) list

    (* pure *)
    val yield : 'b -> ('a, 'b) parser
    (* fmap *)
    val map : ('a, 'b) parser -> ('b -> 'c) -> ('a, 'c) parser
    (* bind *)
    val and_then
        : ('a, 'b) parser
        -> ('b -> ('a, 'c) parser)
        -> ('a, 'c) parser
    val (let>)
        : ('a, 'b) parser
        -> ('b -> ('a, 'c) parser)
        -> ('a, 'c) parser
    (* empty *)
    val nothing : ('a, 'b) parser
    (* applicative *)
    val (<|>) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser
    (* many *)
    val repeat : ('a, 'b) parser -> ('a, 'b list) parser
    (* some *)
    val non_zero : ('a, 'b) parser -> ('a, 'b list) parser

    val (&>) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'c) parser
    val (<&) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b) parser

    (* utility functions *)
    val eat : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'c) parser
    val item : ('a, 'a) parser
    val sat : ('a -> bool) -> ('a, 'a) parser
    val char : 'a -> ('a, 'a) parser
    val sep : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b list) parser
    val finish : ('a, unit) parser
    val exact : ('a list) -> ('a, 'a list) parser
    val from_option : ('b option) -> ('a, 'b) parser
    val next_is_not : ('a -> bool) -> ('a, unit) parser
    val while_greedy : ('b -> bool) -> ('b, 'b list) parser

    (* string specific stuff *)
    val read_text : string -> (char, string) parser
    val read_digit : (char, int) parser
    val read_number : (char, int) parser

    (* using the parser *)
    val run : (char, 'b) parser -> string -> 'b list
    val unique : (char, 'b) parser -> string -> 'b option
end = struct
    type ('a, 'b) parser = 'a list -> ('b * 'a list) list

    let yield a s = [(a, s)]
    let map p f s = List.map (fun (a, s) -> (f a, s)) (p s)
    let and_then p f s = L.flatmap (p s) (fun (a, s) -> f a s)
    let (let>) = and_then
    let nothing _ = []
    let (<|>) a b s = List.append (a s) (b s)
    let rec non_zero p =
        let> x = p in
        let> xs = repeat p in
        yield (x :: xs)
    and repeat p = non_zero p <|> yield []

    let (<&) a b =
        let> x = a in
        let> _ = b in
        yield x

    let (&>) a b =
        let> _ = a in
        let> x = b in
        yield x

    let eat a b = and_then a (Fun.const b)
    let item x = match x with
        | [] -> []
        | x :: xs -> [(x, xs)]
    let sat (f : 'a -> bool) : ('a, 'a) parser =
        let> c = item in
        if f c then yield c else nothing
    let char x = sat ((==) x)
    let sep a b =
        let> first = a in
        let> next = repeat (eat b a) in
        yield (first :: next)
    let finish s = match s with
        | [] -> [((), [])]
        | _ -> []
    let rec exact s = match s with
        | [] -> yield []
        | x :: xs -> let> b = item in
            if b == x
                then map (exact xs) (List.cons b)
                else nothing
    let from_option o = match o with
        | Some x -> yield x
        | None -> nothing
    let next_is_not f s = match s with
        | [] -> [((), [])]
        | x :: xs -> if f x then [] else [((), x :: xs)]
    let while_greedy f =
        let> s = non_zero (sat f) in
        let> _ = next_is_not f in
        yield s


    let read_text s = map (exact (S.to_list s)) S.from_list
    let read_digit = and_then item (A.read_char >> from_option)
    let read_number = and_then
        (non_zero (sat A.is_digit))
        (S.from_list >> A.read_string >> from_option)

    let run p s = List.map fst (p (S.to_list s))
    let unique p s = let result = run p s in
        if List.length result > 1
            then None
            else L.hd result
end
