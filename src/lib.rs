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
    use crate::get_lines;
    use crate::AdventOfCodeError;
    pub fn run_day2(fname: &str) -> Result<(), AdventOfCodeError> {
        let lines: Vec<String> = get_lines(fname);
        let reports: Vec<Vec<i32>> = lines
            .iter()
            .map(|r| r.split(" ").map(|s| s.parse::<i32>().unwrap()).collect())
            .collect();
        let count_all_safe = reports.iter().filter(|r| is_safe(*r)).count();
        let count_all_dampened_safe = reports.iter().filter(|r| is_dampened_safe(*r)).count();
        println!("num safe={count_all_safe}");
        println!("num safe={count_all_dampened_safe}");
        Ok(())
    }
    fn get_diffs(report: &Vec<i32>) -> Vec<i32> {
        let mut sub = report.clone();
        sub.rotate_left(1);
        let mut diffs: Vec<i32> = report.into_iter().zip(sub).map(|(a, b)| a - b).collect();
        diffs.pop(); // pop to remove the last element, which is 0
        diffs
    }
    fn is_safe(report: &Vec<i32>) -> bool {
        let diffs = get_diffs(report);
        diffs.iter().all(|&x| x < 0 && x.abs() < 4) || diffs.iter().all(|&x| x > 0 && x < 4)
    }
    fn is_dampened_safe(report: &Vec<i32>) -> bool {
        if is_safe(&report) {
            return true;
        }
        let diffs = get_diffs(report);
        // remove a single 0, or changed sign, or abs value > 3
        let num_zeros = diffs.iter().filter(|&x| *x == 0).count();
        let num_abs_gt3 = diffs.iter().filter(|x| x.abs() > 3).count();
        if num_zeros > 1 || num_abs_gt3 > 1 {
            // multiple baddies; can't fix
            return false;
        }
        let mut report_copy = report.clone();
        if num_zeros > 0 {
            let zero_idx = diffs.iter().position(|x| *x == 0).unwrap();
            report_copy.remove(zero_idx + 1);
            // if removing the zero doesn't fix it, no point trying to remove another element
            return is_safe(&report_copy);
        } else if num_abs_gt3 > 0 {
            let gt3_idx = diffs.iter().position(|x| x.abs() > 3).unwrap();
            report_copy.remove(gt3_idx + 1);
            if is_safe(&report_copy) {
                return true;
            }
            // idx comes from diffs, so we may need to test both elements of this diff
            else {
                let mut report_copy = report.clone();
                report_copy.remove(gt3_idx);
                return is_safe(&report_copy);
            }
        }
        let num_pos_diffs = diffs.iter().filter(|&x| *x > 0).count();
        let num_neg_diffs = diffs.iter().filter(|&x| *x < 0).count();

        if num_pos_diffs > num_neg_diffs {
            // remove the first value that causes a negative difference, and retest
            let neg_idx = diffs
                .iter()
                .position(|x| *x < 0)
                .expect("why no negatives?");
            report_copy.remove(neg_idx + 1);
            if is_safe(&report_copy) {
                return true;
            } else {
                let mut report_copy = report.clone();
                report_copy.remove(neg_idx);
                return is_safe(&report_copy);
            }
        } else if num_neg_diffs > num_pos_diffs {
            // remove the first value that causes a positive difference, and retest
            let pos_idx = diffs
                .iter()
                .position(|x| *x > 0)
                .expect("why no positives?");
            report_copy.remove(pos_idx + 1);
            if is_safe(&report_copy) {
                return true;
            } else {
                let mut report_copy = report.clone();
                report_copy.remove(pos_idx);
                return is_safe(&report_copy);
            }
        }
        // more <0 than >0
        else if num_neg_diffs == num_pos_diffs {
            return false;
        }
        // feels like we should never get here
        panic!("what did i miss?");
    } // fn is_dampened_safe
} // mod day2
mod day3 {
    use crate::AdventOfCodeError;
    // use crate::AdventOfCodeError::*;
    use crate::get_lines;
    use regex::Regex;
    pub fn run_day3(fname: &str) -> Result<(), AdventOfCodeError> {
        let lines = get_lines(fname);
        let total_p1 = search_lines(&lines);
        let total_p2 = part2(&lines);
        println!("part1: {total_p1}");
        println!("part2: {total_p2}");
        Ok(())
    }
    fn search_lines(lines: &Vec<String>) -> i32 {
        let re = Regex::new(r"mul\((?<first>[0-9]+),(?<second>[0-9]+)\)").unwrap();
        let mut total = 0;
        for text in lines {
            for (_, [first, second]) in re.captures_iter(&text).map(|c| c.extract()) {
                let prod = first.parse::<i32>().unwrap() * second.parse::<i32>().unwrap();
                total += prod;
            }
        }
        total
    }
    fn part2(lines: &Vec<String>) -> i32 {
        let mut new_lines: Vec<String> = vec![];
        for line in lines {
            let dos: Vec<&str> = line.split("do()").collect();
            for each_do in dos {
                if each_do.matches("don't()").count() > 0 {
                    new_lines.push(each_do.split("don't()").next().unwrap().to_string());
                    // any further don'ts don't matter
                }
                else {
                    new_lines.push(String::from(each_do));
                }
            }
        }
        search_lines(&new_lines)
    }
} // mod day3
