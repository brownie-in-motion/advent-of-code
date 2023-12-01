open Advent

let numbers = [
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine"
]

let calibration_1 (x : string) : (int * int) option =
    let values = List.filter_map A.read_char (S.to_list x) in
    O.map2 T.pair (L.hd values) (L.hd (List.rev values))

let calibration_2 (x : string) : (int * int) option =
    let rec find n s = match String.length s with
        | 0 -> None
        | l -> let inc = Option.map ((+) 1) in List.fold_left O.or_else None [
            A.read_char (String.get s 0);
            inc (List.find_index (fun p -> String.starts_with ~prefix:p s) n);
            find n (String.sub s 1 (l - 1))
        ]
    in O.map2 T.pair
        (find numbers x)
        (find (List.map S.reverse numbers) (S.reverse x))

let part (c : string -> (int * int) option) (x : string list) : int =
    let combine = fun (x, y) -> 10 * x + y in
    List.fold_left (+) 0 (List.map combine (List.filter_map c x))

let part_1 = part calibration_1
let part_2 = part calibration_2

let () =
    Printf.printf "part 1: %d\n" (part_1 (A.input 1));
    Printf.printf "part 2: %d\n" (part_2 (A.input 1))
