open Advent
open Advent.P
open Advent.Parsing

module IMap = Map.Make (Int)

type card = {
    _id : int;
    numbers : int list;
    winning : int list;
}

let token (p : (char, 'b) parser) : (char, 'b) parser =
    p <& repeat (read_text " ")

let parse_card : (char, card) parser =
    let> _ = token (read_text "Card") in
    let> id = token read_number in let> _ = token (read_text ":") in
    let> numbers = token (sep read_number (non_zero (read_text " "))) in
    let> _ = token (read_text "|") in
    let> winning = token (sep read_number (non_zero (read_text " "))) in
    let> _ = finish in
    yield { _id = id ; numbers ; winning }

let read_card : string -> card option = unique parse_card

let matches (a : 'a list) (b : 'a list) : 'a list =
    let sort a b = if a = b then 0 else if a < b then -1 else 1 in
    let rec matches_inner = function
        | (x :: xs), (y :: ys) ->
            if x = y then x :: matches_inner (xs, ys)
            else if x < y then matches_inner (xs, (y :: ys))
            else matches_inner ((x :: xs), ys)
        | _ -> []
    in matches_inner (List.sort_uniq sort a, List.sort_uniq sort b)

let all_scores (f : int -> int) (c : card list) : int list =
    let score { numbers ; winning ; _ } = List.length (matches numbers winning)
    in List.map (f % score) c

module Day_04 : Day = struct
    let day = 4
    type problem_t = card list
    type solution_t = int

    let parse = List.map read_card >> O.sequence
    let display = string_of_int

    let part_1 =
        let score x = if x = 0 then 0 else A.pow 2 (x - 1) in
        all_scores score >> L.sum >> Option.some

    let part_2 = Option.some % function rows ->
        let scores = all_scores Fun.id rows in
        let update state (card, score) =
            let current = Option.value (IMap.find_opt card state) ~default:0 in
            let new_cards = L.range (card + 1) (card + score + 1) in
            let increase c s k =
                let amount = Option.value (IMap.find_opt k s) ~default:0 in
                IMap.add k (amount + c) s
            in List.fold_left (increase current) state new_cards
        in
        let start = IMap.of_list (List.mapi (fun i _ -> (i, 1)) scores) in
        let state = List.fold_left update start (List.mapi T.pair scores) in
        List.map snd (IMap.to_list state) |> L.sum
end

module Solve = Solution (Day_04)

let () = Solve.run ()
