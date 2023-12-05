open Advent
open Advent.P
open Advent.Parsing

let rec break (f : 'a -> bool) : 'a list -> ('a list * 'a list) = function
    | x :: xs -> if f x
        then ([], x :: xs)
        else let (l, r) = break f xs in (x :: l, r)
    | [] -> ([], [])

let rec split (f : 'a -> bool) (l : 'a list) : 'a list list =
    match break f l with
        | (left, []) -> [left]
        | (left, right) -> left :: split f (List.tl right)

(* assumption: the maps are given to us in order *)
(* that is, no need to look at the 'a-to-b' headers *)

type almanac_map = (int * int * int) list
type problem = {
    seeds : int list;
    maps : almanac_map list;
}

(* parsing *)

let read_seeds : string list -> int list option =
    let parser : (char, int list) parser =
        let> _ = read_text "seeds: " in
        let> values = sep read_number (read_text " ") in
        let> _ = finish in
        yield values
    in function
        | [s] -> unique parser s
        | _ -> None

let read_map : string list -> almanac_map option =
    let parse_header : (char, unit) parser =
        let> _ = repeat item in
        let> _ = read_text "-to-" in
        let> _ = repeat item in
        let> _ = read_text " map:" in
        let> _ = finish in
        yield ()
    in
    let parse_line : (char, (int * int * int)) parser =
        let> a = read_number in
        let> _ = read_text " " in
        let> b = read_number in
        let> _ = read_text " " in
        let> c = read_number in
        let> _ = finish in
        yield (a, b, c)
    in function
        | x :: xs ->
            let* _ = unique parse_header x in
            let* map = List.map (unique parse_line) xs |> O.sequence in
            Some map
        | [] -> None

let read_problem : string list list -> problem option = function
    | x :: xs ->
        let* seeds = read_seeds x in
        let* maps = List.map read_map xs |> O.sequence in
        Some { seeds; maps }
    | [] -> None

(* part 1 *)

let map_function (m : almanac_map) (x : int) : int =
    let row_function x (d, s, l) = if s <= x && x <= s + l
        then Some (d + x - s)
        else None
    in
    L.hd (List.filter_map (row_function x) m) |> Option.value ~default:x 

let almanac_function : almanac_map list -> int -> int =
    List.rev >> List.map map_function >> List.fold_left (%) Fun.id

let min_list (l : 'a list) : 'a option =
    let smaller y = Option.some % function
        | Some x -> if x < y then x else y
        | None -> y
    in
    List.fold_left (Fun.flip smaller) None l

let part_1 ({ seeds; maps } : problem) : int option =
    List.map (almanac_function maps) seeds |> min_list

(* part 2 *)

type interval = int * int
type range = interval list

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let shift (i : int) : range -> range =
    let inner (x, y) = (x + i, y + i) in
    List.map inner

let clean : range -> range = List.filter (fun (a, b) -> a <= b)

(* (part of (a, b) captured in (x, y), remaining of (a, b)) *)
let intersect (x, y : interval) (a, b : interval) : range * range =
    if a > y || x > b
        then ([], [(a, b)])
        else
            let l_max = max x a in
            let r_min = min y b in
            ([(l_max, r_min)], clean [(a, l_max - 1); (r_min + 1, b)])

let partial_image (d, s, l : int * int * int) (i : interval) : range * range =
    let a, x = intersect (s, s + l) i in (shift (d - s) a, x)

let rec map_image (m : almanac_map) (i : interval) : range = match m with
    | [] -> [i]
    | e :: es ->
        let hit, remain = partial_image e i in
        List.append hit (List.concat_map (map_image es) remain)

let almanac_image (a : almanac_map list) (i : interval) : range =
    List.fold_left (fun c m -> List.concat_map (map_image m) c) [i] a

let rec chunked : 'a list -> ('a * 'a) list option = function
    | a :: b :: cs ->
        let* remaining = chunked cs in
        Some ((a, b) :: remaining)
    | [] -> Some []
    | _ -> None

let part_2 ({ seeds; maps } : problem) : int option =
    let* pairs = chunked seeds in
    let intervals = List.map (fun (x, y) -> (x, x + y)) pairs in
    let result = List.concat_map (almanac_image maps) intervals in
    List.map fst result |> min_list

let run_part = Option.bind (read_problem (split (String.equal "") (A.input 5)))
let () =
    A.display_int "part 1" (run_part part_1);
    A.display_int "part 2" (run_part part_2);
