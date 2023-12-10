open Advent
open Advent.P
open Advent.Parsing

let parse_card =
    let> card = item in
    match card with
        | 'T' -> yield 10
        | 'J' -> yield 11
        | 'Q' -> yield 12
        | 'K' -> yield 13
        | 'A' -> yield 14
        | _ -> nothing

let parse_hand = non_zero (parse_card <|> read_digit)

let parse_row =
    let> hand = parse_hand in
    let> _ = read_text " " in
    let> bet = read_number in
    let> _ = finish in
    yield (hand, bet)

let compare_card x y = x - y

(* raw lexicographic comparison *)
let rec compare_hand f (l : int list) (r : int list) : int =
    match l, r with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: xs, y :: ys ->
            let c = f x y in
            if c == 0
                then compare_hand f xs ys
                else f x y

let get_equal (v : int) (l : int list) : int * int list =
    let left, right = List.partition ((==) v) l in
    (List.length left, right)

let rec chunked (l : int list) : (int * int) list =
    match l with
        | x :: _ ->
            let count, remain = get_equal x l in
            (count, x) :: chunked remain
        | [] -> []

let hand_rank (l : int list) =
    match l with
        | 5 :: _ -> 7
        | 4 :: _ -> 6
        | 3 :: 2 :: _ -> 5
        | 3 :: _ -> 4
        | 2 :: 2 :: _ -> 3
        | 2 :: _ -> 2
        | _ -> 1

let part_1_value (l : int list) : int list =
    let counts = chunked (List.sort compare_card l) |> List.map fst in
    let rank = hand_rank (List.sort (fun x y -> y - x) counts) in
    rank :: l

let part_2_value (l : int list) : int list =
    let f = List.filter ((<>) 11) l in
    let jokers = List.length l - List.length f in
    let counts = chunked (List.sort compare_card f) |> List.map fst in
    let sorted = List.sort (fun x y -> y - x) counts in
    let boosted = match sorted with
        | x :: xs -> (x + jokers) :: xs
        | [] -> [jokers] in
    let rank = hand_rank boosted in
    rank :: (List.map (fun x -> if x == 11 then 0 else x) l)

let run f input =
    let ranked = List.map (fun (h, b) -> (f h, b)) input in
    let compare x y = compare_hand compare_card (fst x) (fst y) in
    let sorted = List.sort compare ranked in
    let winnings = List.mapi (fun i (_, bet) -> bet * (i + 1)) sorted in
    L.sum winnings

module Day_07 : Day = struct
    let day = 7
    type problem_t = (int list * int) list
    type solution_t = int

    let parse = List.map (unique parse_row) >> O.sequence
    let display = string_of_int

    let part_1 = run part_1_value >> Option.some
    let part_2 = run part_2_value >> Option.some
end

module Solve = Solution (Day_07)

let () = Solve.run ()
