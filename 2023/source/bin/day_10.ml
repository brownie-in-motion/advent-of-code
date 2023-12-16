open Advent
open Advent.P

type direction = L | R | U | D
type pipe = direction * direction
type position = int * int

module Position = struct
    type t = position
    let compare = compare
end

module PMap = Map.Make (Position)
module PSet = Set.Make (Position)

let read_pipe = function
    | '|' -> Some (U, D)
    | '-' -> Some (L, R)
    | 'L' -> Some (R, U)
    | 'J' -> Some (L, U)
    | '7' -> Some (L, D)
    | 'F' -> Some (R, D)
    | '.' -> None
    | 'S' -> None
    | _ -> None

let delta = function
    | L -> (0, -1)
    | R -> (0, 1)
    | U -> (-1, 0)
    | D -> (1, 0)

let rotate = function
    | L -> D
    | R -> U
    | U -> L
    | D -> R

let undelta = function
    | (0, -1) -> Some L
    | (0, 1) -> Some R
    | (-1, 0) -> Some U
    | (1, 0) -> Some D
    | _ -> None

let add (i, j) (di, dj) = (i + di, j + dj)
let neg (i, j) = (-i, -j)

let neighbors pos pipe = List.map
    (delta >> add pos)
    [fst pipe ; snd pipe]

let around pos = List.map
    (delta >> add pos)
    [L ; R ; U ; D]

let insert k v m = PMap.add k (Some v) m
let mark k m = PMap.add k None m
let find k m = Option.join (PMap.find_opt k m)
let marked k m = Option.is_some (PMap.find_opt k m)

module Day_10 = struct
    let day = 10
    type problem_t = (position * (pipe option) PMap.t)
    type solution_t = int

    let parse lines =
        let array = List.map (S.to_list) lines in
        let* (start, map) = List.fold_left (fun acc ((i, j), c) ->
            let* (s, m) = acc in
            match read_pipe c with
                | Some pipe -> Some (s, insert (i, j) pipe m)
                | None ->
                    let m = mark (i, j) m in
                    if c != 'S' then Some (s, m) else match s with
                        | Some _ -> None
                        | None -> Some (Some (i, j), m)
        ) (Some (None, PMap.empty)) (L.enum2 array)
        in
        Option.map (fun s -> (s, map)) start

    let display = string_of_int

    let loop_walk start map (f : 'a -> position -> direction -> 'a) (a : 'a) =
        let frontier = start |> around |> List.filter (fun p ->
            match find p map with
                | Some pipe -> List.mem start (neighbors p pipe)
                | None -> false
            )
        in
        let* base = match frontier with
            | [a ; _] -> Some a
            | _ -> None
        in
        let rec fold a c p =
            let* d = undelta (add c (neg p)) in
            let a = f a c d in
            match find c map with
                | Some pipe ->
                    let* next = List.find_opt
                        ((=) p >> not)
                        (neighbors c pipe)
                    in
                    let* next_direction = undelta (add next (neg c)) in
                    let a = if next_direction != d
                        then f a c next_direction
                        else a
                    in
                    fold a next c
                | None -> Some a
        in
        fold a base start

    let part_1 (start, map) =
        let* loop_set = loop_walk start map
            (fun a p _ -> PSet.add p a)
            PSet.empty
        in
        Some (((PSet.cardinal loop_set - 1) / 2) + 1)

    let part_2 (start, map) =
        let* loop_set = loop_walk start map
            (fun a p _ -> PSet.add p a)
            PSet.empty
        in
        let* rim_set = loop_walk start map
            (fun a p d -> PSet.add (add p (delta (rotate d))) a)
            PSet.empty
        in
        let rec flood (frontier : PSet.t) (visited : PSet.t) =
            let visited = PSet.union visited frontier in
            let next = PSet.fold
                (fun p -> PSet.union (PSet.of_list (around p)))
                frontier
                PSet.empty
            in
            let frontier = next
                |> Fun.flip PSet.diff visited
                |> PSet.filter (Fun.flip marked map)
            in
            if PSet.is_empty frontier
                then visited
                else flood frontier visited
        in
        let entire = PMap.fold (fun p _ a -> PSet.add p a) map PSet.empty in
        let region = flood (PSet.diff rim_set loop_set) loop_set in
        let enclosed = PSet.diff region loop_set in
        let is_edge = PSet.exists (fun p ->
            let neighbors = around p in
            List.exists (Fun.flip marked map >> not) neighbors
        ) enclosed in
        let inside = if is_edge
            then PSet.diff entire region
            else enclosed
        in Some (PSet.cardinal inside)
end

module Solve = Solution (Day_10)

let () = Solve.run ()
