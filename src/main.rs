use advent_of_code::Config;
use std::env;

fn main() -> Result<(), advent_of_code::AdventOfCodeError> {
    let args: Vec<String> = env::args().collect();
    let process_name = args[0].clone();
    let config = Config::build(args).unwrap_or_else(|err| {
        eprintln!("Problem parsing args: {:?}",err);
        eprintln!("Usage: {} DAY_NUM FNAME", process_name);
        std::process::exit(1);
    });
    // advent_of_code::day1::run_day1("day1_input.txt");
    // advent_of_code::day1::run_day1("day1_test_file.txt");
    return advent_of_code::run(config);
}
