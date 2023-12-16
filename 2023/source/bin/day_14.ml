open Advent

module Day_14 = struct
    let day = 14
    type problem_t = string list
    type solution_t = int

    let parse _ = None
    let display = string_of_int

    let part_1 _ = None
    let part_2 _ = None
end

module Solve = Solution (Day_14)

let () = Solve.run ()
