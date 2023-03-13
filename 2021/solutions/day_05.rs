use std::collections::HashMap;

#[derive(Debug)]
struct Line {
    from: (i64, i64),
    to: (i64, i64),
}

impl Line {
    fn coords(&self) -> (i64, i64, i64, i64) {
        let ((x_1, y_1), (x_2, y_2)) = (self.from, self.to);
        (x_1, y_1, x_2, y_2)
    }

    fn is_orthogonal(&self) -> bool {
        let (x_1, y_1, x_2, y_2) = self.coords();
        x_1 == x_2 || y_1 == y_2
    }

    fn delta(&self) -> (i64, i64) {
        let (x_1, y_1, x_2, y_2) = self.coords();
        (
            (x_2 - x_1) / (x_2 - x_1).abs().max(1),
            (y_2 - y_1) / (y_2 - y_1).abs().max(1),
        )
    }

    fn length(&self) -> i64 {
        let (x_1, y_1, x_2, y_2) = self.coords();
        (x_2 - x_1).abs().max((y_2 - y_1).abs())
    }
}

type Problem = Vec<Line>;

#[aoc::day(5)]
fn parse_input(input: &str) -> Problem {
    input
        .lines()
        .map(|line| {
            match line.split([',', ' ']).collect::<Vec<&str>>()[..] {
                [a, b, "->", c, d] => [a, b, c, d],
                _ => panic!("invalid input!"),
            }
            .map(|n| n.parse::<i64>().unwrap())
        })
        .map(|[a, b, c, d]| Line {
            from: (a, b),
            to: (c, d),
        })
        .collect()
}

// spent way too long implementing some sweep line approach
// since the inputs are small, let's just simulate
fn crossings(input: Problem) -> usize {
    input
        .iter()
        .fold(
            (HashMap::<(i64, i64), usize>::new(), 0),
            |(mut map, c), line| {
                let (x_1, y_1) = line.from;
                let (dx, dy) = line.delta();
                let new = (0..line.length() + 1)
                    .filter(|i| {
                        let (x, y) = (x_1 + i * dx, y_1 + i * dy);
                        let count = map.entry((x, y)).or_insert(0);
                        *count += 1;
                        *count == 2
                    })
                    .count();
                (map, c + new)
            },
        )
        .1
}

#[aoc::solve(1)]
fn solve_one(input: Problem) -> usize {
    crossings(input.into_iter().filter(Line::is_orthogonal).collect())
}

#[aoc::solve(2)]
fn solve_two(input: Problem) -> usize { crossings(input) }
