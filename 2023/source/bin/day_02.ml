open Advent
open Advent.P

(* some ad hoc parsing for today. combinators tomorrow! *)

type ('a, 'b) parse = ('a list * 'b) option

type game_data = {
    id : int;
    games : (int * int * int) list;
}

let rec exact (p : 'a list) (s : 'a list) : ('a, unit) parse =
    match (p, s) with
        | (x :: xs, y :: ys) -> if x == y
            then exact xs ys
            else None
        | [], y -> Some (y, ())
        | _, _ -> None

let number (s : char list) : (char, int) parse =
    match L.span (fun x -> '0' <= x && x <= '9') s with
        | ([], _) -> None
        | (x, s) -> match x |> S.from_list |> A.read_string with
            | None -> None
            | Some x -> Some (s, x)

let parse_id (s : char list) : (char, int) parse =
    let* s, _ = exact (S.to_list "Game ") s in
    let* s, n = number s in
    let* s, _ = exact (S.to_list ": ") s in
    Some (s, n)

let parse_color (s : char list) : (char, int * int * int) parse =
    let* s, n = number s in
    let* s, _ = exact [' '] s in
    let place = [
        ((fun (x, _) -> (x, (n, 0, 0))), "red");
        ((fun (x, _) -> (x, (0, n, 0))), "green");
        ((fun (x, _) -> (x, (0, 0, n))), "blue");
    ] in
    let parse (f, color) = Option.map f (exact (S.to_list color) s) in
    List.filter_map parse place |> L.hd

let rec parse_colors (s : char list) : (char, int * int * int) parse =
    match s with
        | [] -> Some ([], (0, 0, 0))
        | x ->
            let* (s, a) = parse_color x in
            let (_, s) = L.span (fun x -> x < '0' || x > '9') s in
            let* (s, b) = parse_colors s in
            Some (s, (T.apply3 (+) a b))

let rec parse_games (s : char list) : (char, (int * int * int) list) parse =
    match L.span (fun x -> x != ';') s with
        | ([], _) -> Some ([], [])
        | (l, r) ->
            let* _, b = parse_colors l in
            let _, r = L.span (fun x -> x == ';') r in
            let _, r = L.span (fun x -> x == ' ') r in
            let* r, bs = parse_games r in
            Some (r, b :: bs)

let parse_row (s : string) : (char, game_data) parse =
    let* s, id = parse_id (S.to_list s) in
    let* s, games = parse_games s in
    Some (s, { id; games; })

let part_1 ({ id ; games } : game_data) : int =
    let each (x, y, z) = x && y && z in
    let allowed game = each (T.apply3 (<=) game (12, 13, 14)) in
    if List.fold_left (&&) true (List.map allowed games) then id else 0

let part_2 ({ id = _; games } : game_data) : int =
    let max = (T.apply3 (fun x y -> if x < y then y else x)) in
    let (r, g, b) = List.fold_left max (0, 0, 0) games in r * g * b

module Day_02 : Day = struct
    let day = 2
    type problem_t = game_data list
    type solution_t = int

    let parse = Option.map (List.map snd) % O.sequence % List.map parse_row
    let display = string_of_int

    let part_1 = L.sum % List.map part_1 >> Option.some
    let part_2 = L.sum % List.map part_2 >> Option.some
end

module S = Solution (Day_02)

let () = S.run ()
