open Advent
open Advent.P

type rock = O | C | E

let read_rock = function
    | 'O' -> Some O
    | '#' -> Some C
    | '.' -> Some E
    | _ -> None

module PMap = Map.Make (struct
    type t = rock list list
    let compare = compare
end)

module Day_14 = struct
    let day = 14
    type problem_t = rock list list
    type solution_t = int

    let parse = List.map S.to_list
        >> List.map (List.map read_rock >> O.sequence)
        >> O.sequence

    let display = string_of_int

    let rec north_load (map : problem_t) (status : (int * int) list list) =
        let height = List.length map in
        match map with
            | [] -> status
            | r :: rs ->
                let process column = function
                    | C -> (height - 1, 0) :: column
                    | E -> column
                    | O -> match column with
                        | [] -> [(height, 1)]
                        | (h, n) :: cs -> (h, n + 1) :: cs
                in
                north_load rs (List.map (F.uncurry process) (L.zip status r))

    let reverse_triangle (t : int) (n : int) =
        (t * (t + 1) / 2) - ((t - n) * (t - n + 1) / 2)

    let part_1 m =
        let* width = Option.map List.length (L.hd m) in
        let length = List.length m in
        let status = List.init width (Fun.const [(length, 0)]) in
        let counts = north_load m status in
        let result = List.map (List.map (F.uncurry reverse_triangle)) counts in
        Some (List.map L.sum result |> L.sum)

    let simulate (map : problem_t) : problem_t option =
        let* width = Option.map List.length (L.hd map) in
        let length = List.length map in
        let status = List.init width (Fun.const [(length, 0)]) in
        let counts = List.map List.rev (north_load map status) in
        let rec fill column counts = match column, counts with
            | [], _ -> []
            | c :: cs, [] -> (if c = O then E else c) :: fill cs []
            | cs, (_, 0) :: ns -> fill cs ns
            | c :: cs, (h, n) :: ns -> if h = List.length column
                then O :: fill cs ((h - 1, n - 1) :: ns)
                else (if c = O then E else c) :: fill cs counts
        in
        List.map (F.uncurry fill) (L.zip (L.transpose map) counts)
            |> L.transpose
            |> Option.some

    let cycle (map : problem_t) : problem_t option =
        let rotate = L.transpose >> List.map List.rev in
        let* north = simulate map in
        let* west = simulate (rotate north) in
        let* south = simulate (rotate west) in
        let* east = simulate (rotate south) in
        Some (rotate east)

    let find_repeat (map : problem_t) : (int * int * problem_t) option =
        let rec inner h m i = match PMap.find_opt m h with
            | Some j -> Some (j, i - j, m)
            | None ->
                let* next = cycle m in
                inner (PMap.add m i h) next (i + 1)
        in
        inner PMap.empty map 0

    let sequence_n (n : int) (f : 'a -> 'a option) (x : 'a) : 'a option =
        List.fold_left (fun x _ -> Option.bind x f) (Some x) (L.range 0 n)

    let rec load : problem_t -> int = function
        | row :: remaining ->
            let multiplier = 1 + List.length remaining in
            let count = List.length (List.filter ((=) O) row) in
            let next = load remaining in
            count * multiplier + next
        | [] -> 0

    let part_2 m =
        let* prefix, loop, map = find_repeat m in
        let remaining = (1000000000 - prefix) mod loop in
        let* board = sequence_n remaining cycle map in
        Some (load board)

end

module Solve = Solution (Day_14)

let () = Solve.run ()
