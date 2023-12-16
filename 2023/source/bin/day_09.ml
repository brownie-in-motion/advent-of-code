open Advent
open Advent.P
open Advent.Parsing

module Day_09 = struct
    let day = 9
    type problem_t = int list list
    type solution_t = int

    let read_line = sep read_number (read_text " ") <& finish
    let parse = List.map (unique read_line) >> O.sequence

    let display = string_of_int

    let derivatives (x : int list) : int list option =
        let rec inner x =
            if List.fold_left (&&) true (List.map ((==) 0) x)
                then Some []
                else
                    let* tail = L.tl x in
                    let* last = L.last x in
                    let diffs = List.map (F.uncurry (-)) (L.zip tail x) in
                    let* rest = inner diffs in
                    Some (last :: rest)
        in
        Option.map List.rev (inner x)

    let part_1 x =
        let* deltas = O.sequence (List.map derivatives x) in
        Some (L.sum (List.map L.sum deltas))

    let part_2 = List.map List.rev >> part_1
end

module Solve = Solution (Day_09)

let () = Solve.run ()
