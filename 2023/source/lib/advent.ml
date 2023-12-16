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
    val bimap : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
    val pair : 'a -> 'b -> ('a * 'b)
end = struct
    let apply3 f (a, b, c) (x, y, z) = (f a x, f b y, f c z)
    let bimap f g (a, b) = (f a, g b)
    let pair a b = (a, b)
end

(* list helpers *)
module L : sig
    val hd : 'a list -> 'a option
    val tl : 'a list -> 'a list option
    val last : 'a list -> 'a option
    val product : 'a list -> 'b list -> ('a * 'b) list
    val range : int -> int -> int list
    val span : ('a -> bool) -> 'a list -> 'a list * 'a list
    val sum : (int list) -> int
    val flatmap : 'a list -> ('a -> 'b list) -> 'b list
    val zip : 'a list -> 'b list -> ('a * 'b) list
    val break : ('a -> bool) -> 'a list -> 'a list * 'a list
    val split : ('a -> bool) -> 'a list -> 'a list list
    val enum2 : ('a list list) -> ((int * int) * 'a) list
    val unzip : ('a * 'b) list -> 'a list * 'b list
end = struct
    let hd x = match x with
        | [] -> None
        | x :: _ -> Some x
    let tl x = match x with
        | [] -> None
        | _ :: xs -> Some xs
    let rec last x = match x with
        | [x] -> Some x
        | _ :: xs -> last xs
        | [] -> None
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
    let rec zip a b = match (a, b) with
        | (x :: xs, y :: ys) -> (x, y) :: zip xs ys
        | ([], _) -> []
        | (_, []) -> []
    let rec break f = function
        | x :: xs -> if f x
            then ([], x :: xs)
            else let (l, r) = break f xs in (x :: l, r)
        | [] -> ([], [])
    let rec split f l =
        match break f l with
            | (left, []) -> [left]
            | (left, right) -> left :: split f (List.tl right)
    let enum2 l =
        let f i l = List.mapi (fun j c -> (i, j), c) l in
        List.mapi f l |> List.concat
    let unzip l = (List.map fst l, List.map snd l)
end

(* aoc helpers *)
module A : sig
    val lines : string -> string list
    val is_digit : char -> bool
    val pow : int -> int -> int
    val read_char : char -> int option
    val read_string : string -> int option
    val input : int -> string list
    val display_int : string -> int option -> unit
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
        | n -> if n mod 2 = 0
            then pow (x * x) (n / 2)
            else x * pow (x * x) ((n - 1) / 2)

    let read_char c = if is_digit c
        then Some (Char.code c - Char.code '0')
        else None

    let read_string_inner s = List.fold_left
        (O.map2 (fun x y -> 10 * x + y))
        (Some 0)
        (List.map read_char s)

    let read_string s = match S.to_list s with
        | '-' :: s -> Option.map (fun x -> -1 * x) (read_string_inner s)
        | s -> read_string_inner s

    (* this is pretty bad *)
    let input = Printf.sprintf "../inputs/day-%02d" >> lines

    let display_int s x = match x with
        | Some x -> Printf.printf "%s: %d\n" s x
        | None -> Printf.printf "invalid input\n"
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
    val sequence : ('a, 'b) parser list -> ('a, 'b list) parser
    val count : int -> ('a, 'b) parser -> ('a, 'b list) parser

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
    let char x = sat ((=) x)
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
            if b = x
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
    let rec sequence = function
        | [] -> yield []
        | p :: ps ->
            let> x = p in
            let> xs = sequence ps in
            yield (x :: xs)
    let count n p = List.map (Fun.const p) (L.range 0 n) |> sequence

    let read_text s = map (exact (S.to_list s)) S.from_list
    let read_digit = and_then item (A.read_char >> from_option)
    let read_number = let read x = A.is_digit x || x = '-' in
        and_then
            (non_zero (sat read))
            (S.from_list >> A.read_string >> from_option)

    let run p s = List.map fst (p (S.to_list s))
    let unique p s = let result = run p s in
        if List.length result > 1
            then None
            else L.hd result
end

module type Day = sig
    val day : int
    type problem_t
    type solution_t
    val parse : string list -> problem_t option
    val display : solution_t -> string
    val part_1 : problem_t -> solution_t option
    val part_2 : problem_t -> solution_t option
end

module Solution (D : Day) : sig
    val run_1 : unit -> unit
    val run_2 : unit -> unit
    val run : unit -> unit

    val test : (D.problem_t -> 'a option) -> 'a option
end = struct
    let run d part =
        let data = A.input D.day |> D.parse in
        let wrapped = Option.to_result ~none:"invalid input" data in
        let solution = Result.bind
            wrapped
            (Option.to_result ~none:"no solution" % part)
        in
        let message = match solution with
            | Ok x -> D.display x
            | Error e -> e
        in
        Printf.printf "part %d: %s\n" d message

    let run_1 () = run 1 D.part_1
    let run_2 () = run 2 D.part_2
    let run () = run_1 (); run_2 ()

    let test f = A.input D.day |> D.parse |> Fun.flip Option.bind f
end
