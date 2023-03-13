type Problem = Vec<u64>;

#[aoc::day(6)]
fn parse_input(input: &str) -> Problem {
    input
        .split([',', '\n'])
        .filter_map(|s| s.parse().ok())
        .collect()
}

fn generate_cache(size: usize) -> [u64; 9] {
    (0..size).fold([1; 9], |acc, _| {
        (0..9)
            .map(|j| match j {
                0 => acc[6] + acc[8],
                _ => acc[j - 1],
            })
            .collect::<Vec<u64>>()
            .try_into()
            .unwrap()
    })
}

fn solve(input: Problem, k: usize) -> u64 {
    let cache = generate_cache(k);
    input.iter().map(|n| cache[*n as usize]).sum()
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> u64 { solve(input, 80) }

#[aoc::solve(2)]
fn solve_two(input: Problem) -> u64 { solve(input, 256) }
