(* more general stuff i'll organize later *)

let map2 (a : 'a option) (b : 'b option) (f : ('a -> 'b -> 'c)) : 'c option
    = Option.bind a (fun x -> Option.map (fun y -> f x y) b)

let section (a : 'a) (b : 'b) : ('a * 'b) = (a, b)

let hd_opt : 'a list -> 'a option = (Fun.flip List.nth_opt) 0

let or_else (a : 'a option) (b : 'a option) : 'a option = match a with
    | None -> b
    | x -> x

let file_lines (file : string) : string list =
    let rec lines_inner (channel : in_channel) : string list =
        try
            let line = input_line channel in
            line :: lines_inner channel
        with e ->
            match e with
            | End_of_file -> []
            | e -> close_in_noerr channel; raise e
    in lines_inner (open_in file)

let string_to_list (s : string) : char list =
    List.init (String.length s) (String.get s)

let read_char (c : char) : int option = if '0' <= c && c <= '9'
    then Some (Char.code c - Char.code '0')
    else None

let reverse (x : string) : string =
    String.concat "" (List.map (Char.escaped) (List.rev (string_to_list x)))

(* day 01 specific stuff *)

let calibration_1 (x : string) : (int * int) option =
    let values = List.filter_map read_char (string_to_list x) in
    map2 (hd_opt values) (hd_opt (List.rev values)) section

let calibration_2 (x : string) : (int * int) option =
    let rec find n s = match String.length s with
        | 0 -> None
        | l -> let inc = Option.map ((+) 1) in List.fold_left or_else None [
            read_char (String.get s 0);
            inc (List.find_index (fun p -> String.starts_with ~prefix:p s) n);
            find n (String.sub s 1 (l - 1))
        ]
    in
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
    ] in
    map2 (find numbers x) (find (List.map reverse numbers) (reverse x)) section

let part (c : string -> (int * int) option) (x : string list) : int =
    let combine = fun (x, y) -> 10 * x + y in
    List.fold_left (+) 0 (List.map combine (List.filter_map c x))

let part_1 = part calibration_1
let part_2 = part calibration_2

let () =
    Printf.printf "part 1: %d\n" (part_1 (file_lines "inputs/day-01"));
    Printf.printf "part 2: %d\n" (part_2 (file_lines "inputs/day-01"))
