open Advent
open Advent.P
open Advent.Parsing

let is_symbol x = not (A.is_digit x) && x != '.'

(* parsing *)

type row_data = Value of (int * int) | Gap of int | Symbol of char

let parse_symbol : (char, row_data) parser =
    let to_symbol x = Symbol x in
    map (sat is_symbol) to_symbol

let parse_number : (char, row_data) parser =
    let> s = while_greedy A.is_digit in
    let> number = from_option (s |> S.from_list |> A.read_string) in
    yield (Value (List.length s, number))

let parse_gap : (char, row_data) parser =
    let to_gap x = Gap x in
    map (while_greedy ((=) '.')) (List.length >> to_gap)

let parse_row : (char, row_data list) parser =
    let> x = repeat (parse_symbol <|> parse_number <|> parse_gap) in
    let> _ = finish in
    yield x

let read_grid : string list -> row_data list list option =
    List.map (unique parse_row) >> O.sequence

type value_data = {
    position : int * int;
    length : int;
    value : int
}
let get_value { value ; _ } = value

(* iterate over a grid with the positions of objects *)
let position_map
    (f : (int * int) -> row_data -> 'a option)
    (rows : row_data list list)
    : 'a list
    = let process_inner i j x =
        let next = match x with
            | Value (l, _) -> j + l
            | Gap n -> j + n
            | Symbol _ -> j + 1
        in (next , f (i, j) x)
    in
    let process_row i row = snd (List.fold_left_map (process_inner i) 0 row) in
    List.concat (List.mapi process_row rows) |> List.filter_map Fun.id

(* turn a grid of data to a list of values *)
let value_list : row_data list list -> value_data list =
    let f c = function
        | Value (l, v) -> Some {
                position = c ;
                length = l ;
                value = v ;
            }
        | _ -> None
    in position_map f

(* turn a grid of data into a list of symbol positions *)
let symbol_list : row_data list list -> (int * int) list =
    let f c = function
        | Symbol _ -> Some c
        | _ -> None
    in position_map f

(* turn a grid of data into a list of gear positions *)
let gear_list : row_data list list -> (int * int) list =
    let f c = function
        | Symbol '*' -> Some c
        | _ -> None
    in position_map f

module Position = struct
    type t = (int * int)
    let compare (a, b) (x, y) = if a = x then b - y else a - x
end
module Table = Map.Make (Position)
module IntSet = Set.Make (Int)

(* turn a list of values to a table of position -> index *)
let value_table (l : value_data list) : int Table.t =
    let expand i { position = (x, y) ; length ; value = _ } = List.map
        (fun k -> ((x, y + k), i))
        (L.range 0 length)
    in
    Table.of_list (List.concat (List.mapi expand l))

(* given a symbol, figure out what indices it touches *)
let touched_ids (t : int Table.t) ((i, j) : int * int) : IntSet.t =
    let surround x = [x - 1 ; x ; x + 1] in
    let neighbors = L.product (surround i) (surround j) in
    List.filter_map (Fun.flip Table.find_opt t) neighbors |> IntSet.of_list

module Day_03 : Day = struct
    let day = 3
    type problem_t = row_data list list
    type solution_t = int

    let parse = read_grid
    let display = string_of_int

    let part_1 = Option.some % function grid ->
        let data = value_list grid in
        let combine s = IntSet.union s % touched_ids (value_table data) in
        let touched = List.fold_left combine IntSet.empty (symbol_list grid) in
        let filter i { value ; _ } = Option.map
            (Fun.const value)
            (IntSet.find_opt i touched)
        in List.filter_map Fun.id (List.mapi filter data) |> L.sum

    let part_2 = Option.some % function grid ->
        let data = value_list grid in
        let values = Array.of_list (List.map get_value data) in
        let gears = gear_list grid in
        let ratio s = match IntSet.to_list s with
            | [a ; b] -> Some (Array.get values a * Array.get values b)
            | _ -> None
        in
        let touched = List.map (touched_ids (value_table data)) gears in
        List.filter_map ratio touched |> L.sum
end

module Solve = Solution (Day_03)

let () = Solve.run ()
