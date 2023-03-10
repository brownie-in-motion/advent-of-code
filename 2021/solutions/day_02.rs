enum Direction {
    U,
    D,
    F,
}

struct Instruction {
    direction: Direction,
    steps: i64,
}

type Problem = Vec<Instruction>;

#[aoc::day(2)]
fn parse_input(input: &str) -> Problem {
    input
        .lines()
        .map(|line| match line.split(' ').collect::<Vec<_>>()[..] {
            ["forward", n] => (Direction::F, n),
            ["down", n] => (Direction::D, n),
            ["up", n] => (Direction::U, n),
            _ => panic!("invalid input!"),
        })
        .map(|(d, n)| Instruction {
            direction: d,
            steps: n.parse().unwrap(),
        })
        .collect()
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> i64 {
    let (vertical, horizontal): (Vec<i64>, Vec<i64>) = input
        .iter()
        .map(|Instruction { direction, steps }| match direction {
            Direction::U => (-steps, 0),
            Direction::D => (*steps, 0),
            Direction::F => (0, *steps),
        })
        .unzip();

    vertical.iter().sum::<i64>() * horizontal.iter().sum::<i64>()
}

#[aoc::solve(2)]
fn solve_two(input: Problem) -> i64 {
    let (pos, depth, _) = input.iter().fold(
        (0, 0, 0),
        |(pos, depth, aim), Instruction { direction, steps }| match direction {
            Direction::U => (pos, depth, aim - steps),
            Direction::D => (pos, depth, aim + steps),
            Direction::F => (pos + steps, depth + aim * steps, aim),
        },
    );
    pos * depth
}
