open Advent
open Advent.P

module Day_13 = struct
    let day = 13
    type problem_t = char list list list
    type solution_t = int

    let parse = L.split ((=) "")
        >> List.map (List.map S.to_list)
        >> Option.some

    let count_mistakes a b =
        let count_row a b = L.zip a b
            |> List.filter (F.uncurry (<>))
            |> List.length
        in
        L.sum (List.map (F.uncurry count_row) (L.zip a b))

    let check_horizontal (m : int) (i : 'a list list) =
        let rec inner x y = match (x, y) with
            | ([], _) -> None
            | (b :: bs, []) -> inner bs [b]
            | (b :: bs, cs) -> if count_mistakes x y = m
                then Some (List.length y)
                else inner bs (b :: cs)
        in
        inner i []

    let display = string_of_int

    let run n p =
        let inner i = O.or_else
            (check_horizontal n (L.transpose i))
            (Option.map (fun x -> x * 100) (check_horizontal n i))
        in
        let* offsets = O.sequence (List.map inner p) in
        Some (L.sum offsets)

    let part_1 = run 0
    let part_2 = run 1
end

module Solve = Solution (Day_13)

let () = Solve.run ()
