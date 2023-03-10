use std::error::Error;

aoc::load_solutions!("solutions", solution_loader);

#[derive(PartialEq)]
enum Part {
    One,
    Two,
}

struct Args {
    file: String,
    day: u8,
    part: Option<Part>,
}

fn main() {
    let Args { file, day, part } = match get_args() {
        Ok(args) => args,
        Err(e) => return eprintln!("{}", e),
    };

    let input = match std::fs::read_to_string(&file) {
        Ok(input) => input,
        Err(e) => return eprintln!("failed to read {}: {}", file, e),
    };

    run_solution(&input, day, part);
}

fn get_args() -> Result<Args, Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    let file = args.get(1).ok_or("missing file")?.to_string();
    let solve: Vec<&str> = args
        .get(2)
        .ok_or("missing day and part")?
        .split('.')
        .collect();

    let day: u8 = solve.get(0).ok_or("missing day")?.parse()?;
    let part = match solve.get(1) {
        Some(&"1") => Some(Part::One),
        Some(&"2") => Some(Part::Two),
        Some(_) => return Err("invalid part".into()),
        None => None,
    };

    Ok(Args { file, day, part })
}

fn run_solution(input: &str, day: u8, part: Option<Part>) {
    let map = solution_loader();

    let solver = match map.get(&day) {
        Some(solver) => solver,
        None => return eprintln!("no solution for day {}", day),
    };

    if part != Some(Part::Two) {
        println!("{}", solver.solve_one(input))
    };
    if part != Some(Part::One) {
        println!("{}", solver.solve_two(input))
    };
}
