open Advent
open Advent.P
open Advent.Parsing

module SMap = Map.Make (String)
module ISet = Set.Make (Int)
module SSet = Set.Make (String)

module Day_08 = struct
    let day = 8

    type direction = L | R
    type network = (string * string) SMap.t
    type problem_t = direction list * network
    type solution_t = int

    let parse_directions =
        let read_l = (read_text "L" &> yield L) in
        let read_r = (read_text "R" &> yield R) in
        non_zero (read_l <|> read_r) <& finish

    let parse_vertex =
        let alpha x = 'A' <= x && x <= 'Z' || A.is_digit x in
        let read_v = map (count 3 (sat alpha)) S.from_list in
        let> v = read_v in
        let> _ = read_text " = (" in
        let> l = read_v in
        let> _ = read_text ", " in
        let> r = read_v in
        let> _ = read_text ")" in
        let> _ = finish in
        yield (v, (l, r))

    let parse lines = match L.split (String.equal "") lines with
        | [[x]; xs] ->
            let* directions = unique parse_directions x in
            let* vertices = List.map (unique parse_vertex) xs |> O.sequence in
            Some (directions, SMap.of_list vertices)
        | _ -> None
    let display = string_of_int

    let next_vertex c d n =
        let side = match d with
            | L -> fst
            | R -> snd
        in Option.map side (SMap.find_opt c n)

    let rec solve_1 (c : string) (d : direction Seq.t) (n : network) =
        if String.equal c "ZZZ"
            then Some 0
            else match Seq.uncons d with
                | None -> None
                | Some (d, ds) ->
                    let* next = next_vertex c d n in
                    let* count = solve_1 next ds n in
                    Some (count + 1)

    let part_1 (d, n) = solve_1 "AAA" (Seq.cycle (List.to_seq d)) n

    (* idea: consider each path as sets of instruction cycles *)
    (* each starting point has an initial number of cycles *)
    (* then at some point it will repeat *)
    (* so, each cycle has a set of indices where it hits Xs *)
    type path_terminals = ISet.t list * ISet.t list

    (* not used *)
    let run_cycle ds n c : (ISet.t * string) option =
        let update acc (i, d) =
            let* set, c = acc in
            let* next = next_vertex c d n in
            if String.ends_with ~suffix:"X" c
                then Some (ISet.add i set, next)
                else Some (set, next)
        in
        List.fold_left update (Some (ISet.empty, c)) (List.mapi T.pair ds)

    (* not used *)
    let _characteristic ds n c : path_terminals option =
        let rec inner visited c : ((string * ISet.t) list * string) option =
            if SSet.mem c visited
                then Some ([], c)
                else
                    let* data, top = run_cycle ds n c in
                    let* next, repeat = inner (SSet.add c visited) top in
                    Some ((c, data) :: next, repeat)
        in
        let* data, repeat = inner SSet.empty c in
        let left, right = L.span (fst >> (<>) repeat) data in
        Some (List.map snd left, List.map snd right)

    (*
        after looking at the above results it turns out that everyone's inputs
        have a bunch of hidden structure that trivialize them, so we're
        starting over. specifically, we notice that

        1. the prefix of each cycle is exactly ONE edge long, and
        2. every cycle periodically hits an XXZ exactly every n, and
        3. that n is exactly a multiple of the instruction length.

        for (2), we have the additional fact that the the cycle may hit an
        extra XXZ exactly one time. and that extra hit will also be in exactly
        the same number of steps, so it does not affect our math.

        so, our solution will be to
        a. check that these conditions indeed hold
        b. find lcm (n1, n2, ..., nk)
    *)

    let cycle_length (d : direction Seq.t) (n : network) (c : string) =
        let* d, ds = Seq.uncons d in
        let* start = next_vertex c d n in
        let rec inner ds n c =
            let* d, ds = Seq.uncons ds in
            let* next = next_vertex c d n in
            if String.ends_with ~suffix:"Z" c
                then Some (1, ds, next)
                else
                    let* count, remaining, last = inner ds n next in
                    Some (count + 1, remaining, last)
        in
        let* count_1, remaining, last_1 = inner ds n start in
        let* count_2, _, last_2 = inner remaining n last_1 in
        if count_1 == count_2 && last_1 == last_2
            then Some count_1
            else None

    let rec gcd a b = match (a, b) with
        | (_, 0) -> a
        | (a, b) -> gcd b (a mod b)

    let part_2 (d, n) =
        let spin = Seq.cycle (List.to_seq d) in
        let* data = SMap.to_list n
            |> List.map fst
            |> List.filter (String.ends_with ~suffix:"A")
            |> List.map (cycle_length spin n)
            |> O.sequence
        in
        let check = List.map (Fun.flip (mod) (List.length d) >> (==) 0) data in
        let* _ = if List.fold_left (&&) true check then Some () else None in
        let lcm a b = a * b / gcd a b in
        Some (List.fold_left lcm 1 data)
end

module Solve = Solution (Day_08)

let () = Solve.run ()
