use aoc::itertools::Itertools;
use std::collections::HashMap;
use std::slice;

const SIZE: usize = 5;

type Board = [[u64; SIZE]; SIZE];

#[derive(Debug)]
struct Problem {
    numbers: Vec<u64>,
    boards: Vec<Board>,
}

#[derive(Debug, Clone)]
struct State {
    coordinates: HashMap<u64, (usize, usize)>,
    call: Option<u64>,
    rows: [u64; SIZE],
    cols: [u64; SIZE],
}

impl State {
    fn new(board: &Board) -> State {
        State {
            coordinates: board
                .iter()
                .enumerate()
                .flat_map(|(i, row)| {
                    row.iter().enumerate().map(move |(j, e)| (*e, (i, j)))
                })
                .collect(),
            call: None,
            rows: [0; SIZE],
            cols: [0; SIZE],
        }
    }

    fn add(&mut self, value: u64) -> bool {
        let (row, col) = match self.coordinates.remove(&value) {
            Some(n) => n,
            None => return false,
        };

        self.call = Some(value);

        self.rows[row] += 1;
        self.cols[col] += 1;

        self.rows[row] == SIZE as u64 || self.cols[col] == SIZE as u64
    }

    fn score(&self) -> Option<u64> {
        self.call.map(|n| self.coordinates.keys().sum::<u64>() * n)
    }
}

#[aoc::day(4)]
fn parse_input(input: &str) -> Problem {
    let mut lines = input.lines();

    Problem {
        numbers: lines
            .next()
            .unwrap()
            .split(',')
            .map(|n| n.parse().unwrap())
            .collect(),

        boards: lines
            .chunked(6)
            .map(|chunks| {
                chunks
                    .iter()
                    .skip(1)
                    .map(|line| {
                        line.split_whitespace()
                            .filter_map(|n| n.parse().ok())
                            .collect::<Vec<u64>>()
                            .try_into()
                            .unwrap()
                    })
                    .collect::<Vec<[u64; SIZE]>>()
                    .try_into()
                    .unwrap()
            })
            .collect(),
    }
}

fn run_solver<F: Fn(slice::Iter<u64>, &mut Vec<State>) -> Option<usize>>(
    Problem { numbers, boards }: Problem,
    runner: F,
) -> u64 {
    let mut states: Vec<State> = boards.iter().map(State::new).collect();

    runner(numbers.iter(), &mut states)
        .and_then(|i| states.get(i))
        .and_then(|s| s.score())
        .unwrap()
}

#[aoc::solve(1)]
fn solve_one(problem: Problem) -> u64 {
    run_solver(problem, |mut numbers, states| {
        numbers.find_map(|number| {
            states.iter_mut().position(|board| (*board).add(*number))
        })
    })
}

#[aoc::solve(2)]
fn solve_two(problem: Problem) -> u64 {
    run_solver(problem, |numbers, states| {
        numbers
            .scan(
                (0..states.len()).collect(),
                |acc: &mut Vec<usize>, n: &u64| {
                    *acc = acc
                        .iter()
                        .filter(|i| !(states[**i].add(*n)))
                        .map(|i| i.clone())
                        .collect::<Vec<usize>>();

                    acc.first().cloned()
                },
            )
            .last()
    })
}
