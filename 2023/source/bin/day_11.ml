open Advent
open Advent.P

type position = int * int

module IMap = Map.Make (Int)

module Day_11 = struct
    let day = 11
    type problem_t = position list
    type solution_t = int

    let parse = List.map S.to_list
        >> L.enum2
        >> List.filter (fun (_, x) -> x == '#')
        >> List.map fst
        >> Option.some

    let display = string_of_int

    let expand n (l : problem_t) : problem_t =
        let expand_one (l : int list) : int list =
            let occupied =
                List.mapi
                    (fun i x -> (x, i))
                    (List.sort_uniq compare l)
                |> IMap.of_list
            in
            let adjust x =
                let taken = IMap.find x occupied in
                n * (x - taken) + taken
            in
            List.map adjust l
        in
        let left, right = L.unzip l in
        L.zip (expand_one left) (expand_one right)

    let part n l =
        let dist (a, b) (c, d) = abs (a - c) + abs (b - d) in
        let l = expand n l in
        List.map (F.uncurry dist) (L.product l l)
        |> L.sum
        |> (Fun.flip (/) 2)
        |> Option.some

    let part_1 = part 2
    let part_2 = part 1000000
end

module Solve = Solution (Day_11)

let () = Solve.run ()
