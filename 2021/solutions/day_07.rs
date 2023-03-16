type Problem = Vec<i64>;

fn median(numbers: &Vec<i64>) -> i64 {
    let mut numbers = numbers.clone();
    numbers.sort();
    numbers[numbers.len() / 2]
}

#[aoc::day(7)]
fn parse_input(input: &str) -> Problem {
    input
        .split([',', '\n'])
        .filter_map(|s| s.parse().ok())
        .collect()
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> i64 {
    let location = median(&input);
    input.iter().map(|x| (x - location).abs()).sum()
}

fn score(input: &Problem, location: i64) -> i64 {
    input
        .iter()
        .map(|x| {
            let diff = (x - location).abs();
            diff * (diff + 1) / 2
        })
        .sum()
}

#[aoc::solve(2)]
fn solve_two(mut input: Problem) -> i64 {
    input.sort();

    let min = input.first().unwrap();
    let max = input.last().unwrap();
    let sum = input.iter().sum::<i64>();
    let num = input.len() as i64;

    let (_, (_, location)) = (*min..=*max)
        .fold((0, (None, None)), |(index, (score, value)), k| {
            let (lower, same) = input[index..]
                .iter()
                .take_while(|x| **x <= k)
                .fold((0, 0), |(lower, same), x| {
                    if *x == k {
                        (lower, same + 1)
                    } else {
                        (lower + 1, same)
                    }
                });

            let position = num - 2 * (index + lower) as i64 - same as i64;

            // computed by differentiating the score function
            let new = (2 * (sum - k * num as i64) - position).abs();
            (
                index + lower + same,
                if score.map(|s| new < s).unwrap_or(true) {
                    (Some(new), Some(k))
                } else {
                    (score, value)
                },
            )
        });

    let location = location.unwrap();
    score(&input, location).min(score(&input, location - 1))
}
