use std::fs;
#[derive(Debug)]
pub enum AdventOfCodeError {
    BadArgument(String),
    ProblemWithInputFile(String),
}
pub struct Config {
    day_num: i32,
    fname: String,
}
impl Config {
    pub fn build(args: Vec<String>) -> Result<Config, AdventOfCodeError> {
        if args.len() < 3 {
            return Err(AdventOfCodeError::BadArgument(String::from(
                "Not enough arguments!",
            )));
        }
        if let Ok(day_num) = args[1].parse::<i32>() {
            Ok(Config {
                day_num: day_num,
                fname: args[2].clone(),
            })
        } else {
            Err(AdventOfCodeError::BadArgument(String::from(
                "Parsing error",
            )))
        }
    }
}
pub fn run(config: Config) -> Result<(), AdventOfCodeError> {
    match config.day_num {
        1 => day1::run_day1(&config.fname),
        2 => day2::run_day2(&config.fname),
        3 => day3::run_day3(&config.fname),
        _ => Err(AdventOfCodeError::BadArgument(
            format! {"Day {} not yet implemented", config.day_num},
        )),
    }
}
fn get_lines(fname: &str) -> Vec<String> {
    let contents = fs::read_to_string(fname).unwrap();
    let lines = contents.lines().map(|s| String::from(s));
    lines.collect()
}
mod day1 {
    use crate::get_lines;
    use crate::AdventOfCodeError;
    use crate::AdventOfCodeError::*;
    fn distance(vec1: &Vec<i32>, vec2: &Vec<i32>) -> i32 {
        let itr = vec1.iter().zip(vec2.iter());
        let mut total: i32 = 0;
        for pair in itr {
            let dist = pair.0 - pair.1;
            total += dist.abs();
        }
        total
    }

    fn similarity(vec1: &Vec<i32>, vec2: &Vec<i32>) -> i32 {
        let mut total: i32 = 0;
        for val in vec1 {
            let sim_val = i32::try_from(vec2.iter().filter(|n| *n == val).count()).unwrap() * val;
            total += sim_val;
        }
        total
    }
    pub fn run_day1(fname: &str) -> Result<(), AdventOfCodeError> {
        let mut first: Vec<i32> = vec![];
        let mut second: Vec<i32> = vec![];
        let lines = get_lines(fname);
        for line in lines {
            let vals = line.split(" ").collect::<Vec<&str>>();
            assert_eq!(vals.len(), 2);
            if let Ok(v1) = vals[0].parse::<i32>() {
                first.push(v1);
            } else {
                return Err(ProblemWithInputFile(String::from(
                    "Couldn't parse column 1 data into i32",
                )));
            }
            if let Ok(v2) = vals[1].parse::<i32>() {
                second.push(v2);
            } else {
                return Err(ProblemWithInputFile(String::from(
                    "Couldn't parse column 2 data into i32",
                )));
            }
        }
        first.sort();
        second.sort();
        let total_dist = distance(&first, &second);
        let sim = similarity(&first, &second);
        println!("distance={total_dist}");
        println!("similarity={sim}");
        Ok(())
    }
} // mod day1
mod day2 {
    use crate::AdventOfCodeError;
    // use crate::AdventOfCodeError::*;
    use crate::get_lines;
    pub fn run_day2(fname: &str) -> Result<(), AdventOfCodeError> {
        let _lines: Vec<String> = get_lines(fname);
        Ok(())
    }
} // mod day2
mod day3 {
    use crate::AdventOfCodeError;
    // use crate::AdventOfCodeError::*;
    use crate::get_lines;
    pub fn run_day3(fname: &str) -> Result<(), AdventOfCodeError> {
        let _lines: Vec<String> = get_lines(fname);
        Ok(())
    }
} // mod day3
