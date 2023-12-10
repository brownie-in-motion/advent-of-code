open Advent
open Advent.P
open Advent.Parsing

let rec fix f x = let x' = f x in if x = x' then x else fix f x'

(* newton's method *)
let f t d x = - x * x + t * x - (d + 1)
let iteration t d x = x - (f t d x) / (- 2 * x + t)
let check t d trials = L.hd (List.filter (f t d >> ((<) 0)) trials)

let margin t d =
    let left = fix (iteration t d) 0 in
    let right = fix (iteration t d) t in
    let* x = check t d [left ; left + 1] in
    let* y = check t d [right ; right - 1] in
    Some (y - x + 1)

(* part 1 input rows are lists of numbers *)
let parse_1 = repeat (non_zero (sat ((==) ' ')) &> read_number)

(* part 2 input rows are numbrs, ignoring spaces *)
let parse_2 : (char, int) parser = and_then
    (repeat (non_zero (sat ((==) ' ')) &> non_zero (sat A.is_digit)))
    (List.concat >> S.from_list >> A.read_string >> from_option)

let parse_in (p : (char, 'a) parser) : string list -> ('a * 'a) option =
    let read_row s = unique (read_text s &> p <& finish) in
    function
        | [t; d] ->
            let* times = read_row "Time:" t in
            let* distances = read_row "Distance:" d in
            Some (times, distances)
        | _ -> None

module Day_06 : Day = struct
    let day = 6
    type problem_t = string list
    type solution_t = int

    let parse = Option.some
    let display = string_of_int

    let part_1 p =
        let* t, d = parse_in parse_1 p in
        let* widths = List.map (F.uncurry margin) (L.zip t d) |> O.sequence in
        Some (List.fold_left (fun x y -> x * y) 1 widths)

    let part_2 p = Option.bind (parse_in parse_2 p) (F.uncurry margin)
end

module Solve = Solution (Day_06)

let () = Solve.run ()
