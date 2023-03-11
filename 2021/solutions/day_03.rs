type Problem = Vec<Vec<u64>>;

#[aoc::day(3)]
fn parse_input(input: &str) -> Problem {
    input
        .lines()
        .map(|s| {
            s.chars()
                .map(|c| match c {
                    '0' => 0,
                    '1' => 1,
                    _ => panic!("invalid input!"),
                })
                .collect()
        })
        .collect()
}

fn compare(input: &Problem) -> Vec<u64> {
    let total = input.len();
    let width = input[0].len();
    let result = input.iter().fold(vec![0; width], |acc, line| {
        acc.iter()
            .zip(line)
            .map(|(n, c)| n + (*c == 1) as usize)
            .collect::<Vec<usize>>()
    });
    result.iter().map(|n| (n * 2 >= total) as u64).collect()
}

fn compare_col(input: &Problem, i: usize) -> u64 {
    (input.iter().filter(|row| row[i] == 1).count() * 2 >= input.len()) as u64
}

fn decode(binary: &Vec<u64>) -> u64 {
    binary.iter().fold(0, |acc, n| (acc << 1) + n)
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> u64 {
    let best = compare(&input);
    let value = decode(&best);
    value * (value ^ (1 << best.len()) - 1)
}

fn prune(i: usize, target: u64, values: Vec<Vec<u64>>) -> Vec<Vec<u64>> {
    match values.len() {
        1 => values,
        _ => values
            .iter()
            .filter(|num| num[i] == target)
            .map(|x| x.clone())
            .collect(),
    }
}

#[aoc::solve(2)]
fn solve_two(input: Problem) -> u64 {
    let width = input[0].len();
    let full = (input.clone(), input);
    let (most, least) = (0..width).fold(full, |(most, least), i| {
        (
            prune(i, compare_col(&most, i), most),
            prune(i, 1 - compare_col(&least, i), least),
        )
    });
    decode(&most[0]) * decode(&least[0])
}
