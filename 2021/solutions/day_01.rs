type Input = Vec<u64>;

#[aoc::day(1)]
fn parse_input(input: &str) -> Input {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn slide(input: Input, skip: usize) -> usize {
    input
        .iter()
        .zip(input.iter().skip(skip))
        .filter(|(a, b)| b > a)
        .count()
}

#[aoc::solve(1)]
fn solve_one(input: Input) -> usize { slide(input, 1) }

#[aoc::solve(2)]
fn solve_two(input: Input) -> usize { slide(input, 3) }
