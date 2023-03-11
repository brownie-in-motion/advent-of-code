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

fn compare(input: &Problem) -> Vec<bool> {
    let total = input.len();
    let width = input[0].len();
    let result = input.iter().fold(vec![0; width], |acc, line| {
        acc.iter()
            .zip(line)
            .map(|(n, c)| n + *c as usize)
            .collect::<Vec<usize>>()
    });
    result.iter().map(|n| (n * 2 >= total)).collect()
}

fn compare_column(input: &Problem, i: usize) -> bool {
    let ones = input.iter().filter(|row| row[i]).count();
    ones * 2 >= input.len()
}

fn decode(binary: &Vec<bool>) -> u64 {
    binary.iter().fold(0, |acc, n| (acc << 1) + *n as u64)
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> u64 {
    let best = compare(&input);
    let value = decode(&best);
    value * (value ^ (1 << best.len()) - 1)
}

fn prune(i: usize, target: bool, values: Vec<Vec<bool>>) -> Vec<Vec<bool>> {
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
            prune(i, compare_column(&most, i), most),
            prune(i, !compare_column(&least, i), least),
        )
    });
    decode(&most[0]) * decode(&least[0])
}
