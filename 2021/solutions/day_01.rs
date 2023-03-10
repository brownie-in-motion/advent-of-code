type Problem = Vec<u64>;

#[aoc::day(1)]
fn parse_input(input: &str) -> Problem {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn slide(input: Problem, skip: usize) -> usize {
    input
        .iter()
        .zip(input.iter().skip(skip))
        .filter(|(a, b)| b > a)
        .count()
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> usize { slide(input, 1) }

#[aoc::solve(2)]
fn solve_two(input: Problem) -> usize { slide(input, 3) }
