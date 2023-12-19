open Advent
open Advent.P
open Advent.Parsing

module IMap = Map.Make (Int)

module Day_15 = struct
    let day = 15
    type problem_t = char list list
    type solution_t = int

    let parse = function
        | [s] -> Some (L.split ((=) ',') (S.to_list s))
        | _ -> None

    let display = string_of_int

    let hash = List.fold_left (fun a x -> (a + Char.code x) * 17 mod 256) 0
    let part_1 = List.map hash >> L.sum >> Option.some

    type instruction = In of char list * int | Out of char list
    let read_instruction =
        let read_in =
            let> label = repeat item in
            let> _ = read_text "=" in
            let> value = read_number in
            yield (In (label, value))
        in
        let read_out =
            let> label = repeat item in
            let> _ = read_text "-" in
            yield (Out label)
        in
        (read_in <|> read_out) <& finish

    let update_box column label value =
        let replace (l, v) = if l = label then (l, value) else (l, v) in
        if List.exists (fun (l, _) -> l = label) column
            then List.map replace column
            else (label, value) :: column

    let rec run_instructions (table : (char list * int) list IMap.t) = function
        | [] -> table
        | In (label, value) :: rest ->
            let i = hash label in
            let c = IMap.find_opt i table |> Option.value ~default:[] in
            let t = IMap.add i (update_box c label value) table in
            run_instructions t rest
        | Out label :: rest ->
            let i = hash label in
            let c = IMap.find_opt i table
                |> Option.value ~default:[]
                |> List.filter (fun (l, _) -> l <> label)
            in
            let t = IMap.add i c table in
            run_instructions t rest

    let part_2 i =
        let* instructions = i
            |> List.map (S.from_list >> unique read_instruction)
            |> O.sequence
        in
        let table = run_instructions IMap.empty instructions in
        let row_score = List.map snd
            >> List.rev
            >> List.mapi ((fun x y -> (x + 1) * y))
            >> L.sum
        in
        let entries = IMap.to_list table
            |> List.map (fun (i, v) -> (i + 1) * row_score v)
            |> L.sum
        in
        Some entries
end

module Solve = Solution (Day_15)

let () = Solve.run ()
