type Problem = Vec<Vec<bool>>;

#[aoc::day(3)]
fn parse_input(input: &str) -> Problem {
    input
        .lines()
        .map(|s| {
            s.chars()
                .map(|c| match c {
                    '0' => false,
                    '1' => true,
                    _ => panic!("invalid input!"),
                })
                .collect()
        })
        .collect()
}

fn decode(binary: &Vec<bool>) -> u64 {
    binary.iter().fold(0, |acc, n| (acc << 1) + *n as u64)
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> u64 {
    let total = input.len();
    let width = input[0].len();
    let result = input.iter().fold(vec![0; width], |acc, line| {
        acc.iter().zip(line).map(|(n, c)| n + *c as usize).collect()
    });
    let best = result.iter().map(|n| (n * 2 >= total)).collect();
    let value = decode(&best);
    value * (value ^ (1 << width) - 1)
}

fn max_column(input: &Problem, i: usize) -> bool {
    input.iter().filter(|row| row[i]).count() * 2 >= input.len()
}

fn prune(i: usize, target: bool, values: Problem) -> Problem {
    match values.len() {
        1 => values,
        _ => values
            .iter()
            .filter(|num| num[i] == target)
            .map(Clone::clone)
            .collect(),
    }
}

#[aoc::solve(2)]
fn solve_two(input: Problem) -> u64 {
    let width = input[0].len();
    let full = (input.clone(), input);
    let (most, least) = (0..width).fold(full, |(most, least), i| {
        (
            prune(i, max_column(&most, i), most),
            prune(i, !max_column(&least, i), least),
        )
    });
    decode(&most[0]) * decode(&least[0])
}
