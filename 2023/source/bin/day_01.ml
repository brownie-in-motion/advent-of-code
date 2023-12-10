open Advent
open Advent.P

let numbers = [
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine"
]

let calibration_1 (x : string) : (int * int) option =
    let values = List.filter_map A.read_char (S.to_list x) in
    O.map2 T.pair (L.hd values) (L.hd (List.rev values))

let calibration_2 (x : string) : (int * int) option =
    let rec find n s = match String.length s with
        | 0 -> None
        | l -> let inc = Option.map ((+) 1) in List.fold_left O.or_else None [
            A.read_char (String.get s 0);
            inc (List.find_index (fun p -> String.starts_with ~prefix:p s) n);
            find n (String.sub s 1 (l - 1))
        ]
    in O.map2 T.pair
        (find numbers x)
        (find (List.map S.reverse numbers) (S.reverse x))

let part (c : string -> (int * int) option) (x : string list) : int =
    let combine = fun (x, y) -> 10 * x + y in
    L.sum (List.map combine (List.filter_map c x))

module Day_01 : Day = struct
    let day = 1
    type problem_t = string list
    type solution_t = int

    let parse = Option.some
    let display = string_of_int

    let part_1 = part calibration_1 >> Option.some
    let part_2 = part calibration_2 >> Option.some
end

module Solve = Solution (Day_01)

let () = Solve.run ()
