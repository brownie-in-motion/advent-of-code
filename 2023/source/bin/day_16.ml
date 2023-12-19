open Advent
open Advent.P

type direction = L | R | U | D
type cell = (direction * direction) list

module PMap = Map.Make (struct
    type t = int * int
    let compare = compare
end)

module DSet = Set.Make (struct
    type t = direction
    let compare = compare
end)

let read_entry = function
    | '.' -> Some [(L, L); (R, R); (U, U); (D, D)]
    | '|' -> Some [(L, D); (L, U); (R, D); (R, U); (U, U); (D, D)]
    | '-' -> Some [(U, L); (U, R); (D, L); (D, R); (L, L); (R, R)]
    | '/' -> Some [(L, D); (R, U); (U, R); (D, L)]
    | '\\' -> Some [(L, U); (R, D); (U, L); (D, R)]
    | _ -> None

type map = cell PMap.t
type cursor = int * int * direction
type visited = DSet.t PMap.t

let delta = function
    | L -> (0, -1)
    | R -> (0, 1)
    | U -> (-1, 0)
    | D -> (1, 0)

let propogate (map : map) ((x, y, d) : cursor) : cursor list =
    match PMap.find_opt (x, y) map with
        | None -> []
        | Some cell ->
            let matches = List.filter ((=) d % fst) cell in
            let directions = List.map snd matches in
            let move (x, y) d =
                let dx, dy = delta d in
                (x + dx, y + dy, d)
            in
            let next = List.map (move (x, y)) directions in
            List.filter (fun (x, y, _) -> PMap.mem (x, y) map) next

let rec fill (m : map) (v : visited) : cursor list -> visited = function
    | [] -> v
    | f ->
        let exists (x, y, d) = match PMap.find_opt (x, y) v with
            | None -> false
            | Some dset -> DSet.mem d dset
        in
        let mark visited (x, y, d) =
            let current = visited
                |> PMap.find_opt (x, y)
                |> Option.value ~default:DSet.empty
            in
            PMap.add (x, y) (DSet.add d current) visited
        in
        let step = List.concat_map (propogate m) f in
        let filtered = List.filter (not % exists) step in
        let visited = List.fold_left mark v filtered in
        fill m visited filtered

module Day_16 = struct
    let day = 16
    type problem_t = map
    type solution_t = int

    let parse m =
        let* data = List.map S.to_list m
            |> L.enum2
            |> List.map (fun (p, c) -> let* v = read_entry c in Some (p, v))
            |> O.sequence
        in
        Some (PMap.of_list data)

    let display = string_of_int

    let flood m (x, y, d) =
        let start = (x, y, d) in
        let initial = PMap.singleton (x, y) (DSet.singleton d) in
        let visited = fill m initial [start] in
        PMap.to_list visited |> List.map fst |> List.length

    let part_1 = Fun.flip flood (0, 0, R) >> Option.some

    let boundaries m =
        let keys = PMap.to_list m |> List.map fst in
        let max_x = List.map fst keys |> List.fold_left max 0 in
        let max_y = List.map snd keys |> List.fold_left max 0 in
        let a, b = max_x
            |> L.range 0
            |> List.map (fun x -> ((x, 0, R), (x, max_y, L)))
            |> L.unzip
        in
        let c, d = max_y
            |> L.range 0
            |> List.map (fun x -> ((0, x, D), (max_x, x, U)))
            |> L.unzip
        in
        List.concat [a; b; c; d]

    let part_2 m =
        let flood = List.map (flood m) (boundaries m) in
        List.fold_left max 0 flood |> Option.some
end

module Solve = Solution (Day_16)

let () = Solve.run ()
