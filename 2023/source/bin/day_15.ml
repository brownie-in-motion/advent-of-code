open Advent

module Day_15 = struct
    let day = 15
    type problem_t = string list
    type solution_t = int

    let parse _ = None
    let display = string_of_int

    let part_1 _ = None
    let part_2 _ = None
end

module Solve = Solution (Day_15)

let () = Solve.run ()
