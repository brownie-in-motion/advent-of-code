open Advent
open Advent.P
open Advent.Parsing

(*
    okay it's dp time
    we have three tables for simplicitly

    1. the 'can a region fit' table
       this is calculated directly.
       for width k, current_table_1[i] = true if a contiguous region of k cells
       could exactly end at i
    2. the 'ways to end exactly here' table
       current_table_2[i] = current_table_1[i] * prev_table_2[i - k - 1]
    3. the 'ways to assign prefix' table
       this is partial sums of current_table_2 except we reset at index i if
       input[i + 1] is '#' and current_table_2[i] = 0. that is, if we see an
       upcoming '#' and there is no way to use it, then we must start over

    ideally we only have to look at the previous array
    so for now we will keep it simple and have a stack of arrays
*)

type dp_state = {
    fit : bool array list ;
    exact : int array list ;
    upto : int array list * int;
}

type status = T | F | U
type instance = status array

let new_state (i : instance) : dp_state =
    let length = Array.length i in
    let rec inner (x : int) (d : bool) : int list =
        if x == length
            then []
            else if i.(x) = T
                then 0 :: inner (x + 1) true
                else (if d then 0 else 1) :: inner (x + 1) (false || d)
    in
    {
        fit = [Array.make length false];
        exact = [Array.make length 0];
        upto = Array.of_list (inner 0 false) :: [], 1;
    }

let compute_fit_i (a : instance) (k : int) (x : int) : bool =
    let length = Array.length a in
    if x + 1 > length then false
    else if x + 1 < length && a.(x + 1) = T then false
    else if x - k + 1 < 0 then false
    else if x - k + 1 > 0 && a.(x - k) = T then false
    else
        let range = L.range (x - k + 1) (x + 1) in
        List.for_all (fun i -> a.(i) <> F) range

let compute_fit (i : instance) (k : int) (s : dp_state) : dp_state =
    let length = Array.length i in
    let list = List.map (compute_fit_i i k) (L.range 0 length) in
    {
        fit = Array.of_list list :: s.fit ;
        exact = s.exact ;
        upto = s.upto ;
    }

let compute_exact (a : instance) (k : int) (s : dp_state) : dp_state =
    let length = Array.length a in
    let my_fit = List.hd s.fit in
    let upto, default = s.upto in
    let last_upto = List.hd upto in
    let compute_exact_i (x : int) : int =
        if my_fit.(x)
            then if x - k - 1 < 0
                then default
                else last_upto.(x - k - 1)
            else 0
    in
    let list = List.map compute_exact_i (L.range 0 length) in
    {
        fit = s.fit ;
        exact = Array.of_list list :: s.exact ;
        upto = s.upto ;
    }

let compute_upto (a : instance) (s : dp_state) : dp_state =
    let length = Array.length a in
    let my_exact = List.hd s.exact in
    let rec inner (prev : int) (x : int) : int list =
        if x == length
            then []
            else
                if x + 1 < length && a.(x + 1) = T && my_exact.(x) = 0
                    then 0 :: inner 0 (x + 1)
                    else
                        let next = prev + my_exact.(x) in
                        next :: inner next (x + 1)
    in
    let list = inner 0 0 in
    let upto, _ = s.upto in
    {
        fit = s.fit ;
        exact = s.exact ;
        upto = Array.of_list list :: upto, 0 ;
    }

let run_stage (a : instance) (k : int) : dp_state -> dp_state = Fun.id
    >> compute_fit a k
    >> compute_exact a k
    >> compute_upto a

module Day_12 = struct
    let day = 12
    type problem_t = (instance * int list) list
    type solution_t = int

    let read_state = function
        | '#' -> Some T
        | '.' -> Some F
        | '?' -> Some U
        | _ -> None

    let read_line : (char, instance * int list) parser =
        let> springs = non_zero (and_then item (read_state >> from_option)) in
        let> _ = read_text " " in
        let> widths = sep read_number (read_text ",") in
        let> _ = finish in
        yield (Array.of_list springs, widths)

    let parse = List.map (unique read_line) >> O.sequence
    let display = string_of_int

    let run_one (instance, widths) : solution_t =
        let final = List.fold_left
            (Fun.flip (run_stage instance))
            (new_state instance)
            widths
        in
        let upto, _ = final.upto in
        (List.hd upto).(Array.length instance - 1)

    let part_1 p = List.map run_one p |> L.sum |> Option.some

    let intercalate_n n sep list =
        List.concat_map
            (Fun.const (list @ sep))
            (L.range 0 (n - 1))
        @ list

    let part_2 p =
        let duplicate (instance, widths) =
            let list = Array.to_list instance in
            let instance = Array.of_list (intercalate_n 5 [U] list) in
            let widths = intercalate_n 5 [] widths in
            (instance, widths)
        in
        let p = List.map duplicate p in
        part_1 p
end

module Solve = Solution (Day_12)

let () = Solve.run ()
