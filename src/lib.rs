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
        4 => day4::run_day4(&config.fname),
        5 => day5::run_day5(&config.fname),
        6 => day6::run_day6(&config.fname),
        7 => day7::run_day7(&config.fname),
        8 => day8::run_day8(&config.fname),
        9 => day9::run_day9(&config.fname),
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
                } else {
                    new_lines.push(String::from(each_do));
                }
            }
        }
        search_lines(&new_lines)
    }
} // mod day3
mod day4 {
    use crate::get_lines;
    use crate::AdventOfCodeError;
    fn count_matches(lines: &Vec<String>) -> i32 {
        lines.iter().fold(0, |acc, ss| {
            acc + ss.matches("XMAS").count() + ss.matches("SAMX").count()
        }) as i32
    }
    pub fn run_day4(fname: &String) -> Result<(), AdventOfCodeError> {
        let lines: Vec<String> = get_lines(fname);
        println!("p1: {}", xmas_puzzle(&lines));
        println!("p2: {}", x_mas_puzzle(&lines));
        Ok(())
    }
    fn xmas_puzzle(horz_lines: &Vec<String>) -> i32 {
        let char_arr: Vec<Vec<char>> = horz_lines.iter().map(|s| s.chars().collect()).collect();
        let num_rows = horz_lines.len();
        let num_cols = horz_lines[0].len();
        let mut vert_lines: Vec<String> = vec![String::new(); num_cols];
        for row in char_arr.iter() {
            for ii in 0..row.len() {
                vert_lines[ii].push(row[ii]);
            }
        }
        let num_diag = num_rows + num_cols - 1;
        let mut frwd_diag_lines: Vec<String> = vec![String::new(); num_diag];
        let mut bkwd_diag_lines: Vec<String> = vec![String::new(); num_diag];
        for ii in 0..num_rows {
            for jj in 0..num_cols {
                frwd_diag_lines[ii + jj].push(char_arr[ii][jj]);
                if jj >= ii {
                    bkwd_diag_lines[jj - ii].push(char_arr[ii][jj]);
                } else {
                    bkwd_diag_lines[num_cols + ii - jj - 1].push(char_arr[ii][jj]);
                }
            }
        }
        count_matches(&horz_lines)
            + count_matches(&vert_lines)
            + count_matches(&bkwd_diag_lines)
            + count_matches(&frwd_diag_lines)
    }
    fn x_mas_puzzle(lines: &Vec<String>) -> i32 {
        let char_arr: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();
        let mut count_xs = 0;
        for (ii, row) in char_arr.iter().enumerate() {
            for (jj, el) in row.iter().enumerate() {
                if ii > 0 && jj > 0 && ii < char_arr.len() - 1 && jj < row.len() - 1 && *el == 'A' {
                    if (char_arr[ii - 1][jj - 1] == 'M' && char_arr[ii + 1][jj + 1] == 'S'
                        || char_arr[ii - 1][jj - 1] == 'S' && char_arr[ii + 1][jj + 1] == 'M')
                        && (char_arr[ii - 1][jj + 1] == 'M' && char_arr[ii + 1][jj - 1] == 'S'
                            || char_arr[ii - 1][jj + 1] == 'S' && char_arr[ii + 1][jj - 1] == 'M')
                    {
                        count_xs += 1;
                    }
                }
            } // iter over elements in row
        } // iter over rows in array
        count_xs
    } // fn x_mas_puzzle
} // mod day4
mod day5 {
    use crate::get_lines;
    use crate::AdventOfCodeError;
    // use std::collections::HashMap;
    pub fn run_day5(fname: &String) -> Result<(), AdventOfCodeError> {
        let lines: Vec<String> = get_lines(fname);
        let mut rules: Vec<(i32, i32)> = Vec::new();
        let mut seqs: Vec<Vec<i32>> = Vec::new();
        for line in lines {
            if line.contains("|") {
                let vals = line
                    .split("|")
                    .map(|v| v.parse::<i32>().unwrap())
                    .collect::<Vec<_>>();
                rules.push((vals[0], vals[1]));
            } else if line.contains(",") {
                // from line, create sequence containing the value and its index
                seqs.push(
                    line.split(",")
                        .map(|v| v.parse::<i32>().unwrap())
                        .collect::<Vec<_>>(),
                );
            }
            // otherwise, it's probably a blank line
        }
        let mut total_passed = 0; // sum of middle values of correct sequences
        let mut total_fixed = 0; // sum of middle values of correctED sequences
                                 /*
                                 let priorities = build_priorities(&rules);
                                 let mut prior_vec = priorities
                                     .iter()
                                             .map(|(key, val)| (*key, *val))
                                             .collect::<Vec<_>>();
                                         prior_vec.sort_by(|(_, pri_a), (_, pri_b)| pri_b.cmp(pri_a));
                                         for (first, sec) in rules.iter() {
                                             let first_pri = prior_vec.iter().find(|(a, _)| *a == *first).unwrap();
                                             let sec_pri = prior_vec.iter().find(|(a, _)| *a == *sec).unwrap();
                                         }
                                 */
        for seq in seqs.iter() {
            if seq.len() % 2 != 1 {
                panic! {"guess my assumption didn't hold"};
            }
            if correctly_ordered(&seq, &rules) {
                total_passed += seq[seq.len() / 2 as usize];
            } else {
                for el in seq.iter() {
                    let mut num_lt = 0;
                    let mut num_gt = 0;
                    for other in seq.iter() {
                        if *el == *other {
                            continue;
                        }
                        if let Some(_) = rules
                            .iter()
                            .find(|(first, sec)| (*first, *sec) == (*el, *other))
                        {
                            num_lt += 1;
                        } else if let Some(_) = rules
                            .iter()
                            .find(|(first, sec)| (*first, *sec) == (*other, *el))
                        {
                            num_gt += 1;
                        } else {
                            panic! {"thought all the rules would be there!"};
                        }
                    } // other in seq.iter
                    if num_lt == num_gt {
                        total_fixed += *el;
                        break;
                    }
                } // for el in seq.iter()
                  // new_seq is reordered version of seq
                  // let prior_vec = build_mini_priorities(&rules, &seq);
                  // let new_seq = build_seq(&seq, &prior_vec);
                  // total_fixed += new_seq[new_seq.len() / 2 as usize];
                  // fix the sequence to pass the rules...
            }
        }
        println!("total of (passed) middle vals: {total_passed}");
        println!("total of (fixed) middle vals: {total_fixed}");
        Ok(())
    }
    fn correctly_ordered(seq: &Vec<i32>, rules: &Vec<(i32, i32)>) -> bool {
        for (first, second) in rules.iter() {
            if let Some((first_loc, _)) = seq.iter().enumerate().find(|(_, el)| *el == first) {
                if let Some((sec_loc, _)) = seq.iter().enumerate().find(|(_, el)| *el == second) {
                    if first_loc > sec_loc {
                        return false;
                    }
                }
            }
        }
        true
    }
    /*
    fn build_priorities(rules: &Vec<(i32, i32)>) -> HashMap<i32, i32> {
        // BAD: takes an absurdly long time
        let mut priorities: HashMap<i32, i32> = HashMap::new();
        for (first, second) in rules.iter() {
            priorities
                .entry(*first)
                .and_modify(|p| *p += 1)
                .or_insert(1);
            priorities.entry(*second).or_insert(0);
            update_priorities(&mut priorities, &rules, *first, *second);
            // want to iterate over all the rules we've already seen and update the priorities if needed
        }
        priorities
    }
    fn update_priorities(
        priorities: &mut HashMap<i32, i32>,
        rules: &Vec<(i32, i32)>,
        current_a: i32,
        current_b: i32,
    ) {
        let mut iter = rules.iter();
        while let Some((a, b)) = iter.next() {
            if (*a, *b) == (current_a, current_b) {
                return;
            }
            if *b == current_a {
                priorities.entry(*a).and_modify(|p| *p += 1);
                update_priorities(priorities, rules, *a, *b);
            }
        }
    }
    fn build_mini_priorities(rules: &Vec<(i32, i32)>, seq: &Vec<i32>) -> Vec<(i32, i32)> {
        // STILL BAD: takes less time, but still too long to be useful
        let mut new_rules: Vec<(i32, i32)> = Vec::new();
        for (first, sec) in rules.iter() {
            if let Some(_) = seq.iter().find(|c| *c == first) {
                if let Some(_) = seq.iter().find(|c| *c == first) {
                    new_rules.push((*first, *sec));
                }
            }
        }
        let priorities = build_priorities(&rules);
        let mut prior_vec = priorities
            .iter()
            .map(|(key, val)| (*key, *val))
            .collect::<Vec<_>>();
        prior_vec.sort_by(|(_, pri_a), (_, pri_b)| pri_b.cmp(pri_a));
        prior_vec
    }
    fn build_seq(seq: &Vec<i32>, priorities: &Vec<(i32, i32)>) -> Vec<i32> {
        let mut new_seq: Vec<i32> = Vec::new();
        for (val, _) in priorities.iter() {
            if let Some(_) = seq.iter().find(|c| *c == val) {
                new_seq.push(*val);
            }
        }
        new_seq
    }
    */
} // mod day5
mod day6 {
    use crate::get_lines;
    use crate::AdventOfCodeError;
    use std::collections::HashMap;
    pub fn run_day6(fname: &str) -> Result<(), AdventOfCodeError> {
        let lines = get_lines(fname);
        let char_arr_init: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();
        let mut char_arr = char_arr_init.clone();
        let mut init_guard_loc: (usize, usize) = (0, 0);
        for (idx, row) in char_arr_init.iter().enumerate() {
            if let Some(loc) = row.iter().position(|&c| is_guard(c)) {
                init_guard_loc = (idx, loc);
                break;
            }
        }
        let init_guard_dir = char_arr_init[init_guard_loc.0][init_guard_loc.1];
        println!("part1: {}", find_num_pos(&mut char_arr, init_guard_loc));
        println!(
            "part2: {}",
            find_num_loop_locs(&mut char_arr, init_guard_loc, init_guard_dir)
        );
        Ok(())
    } // run_day6
    fn find_num_pos(char_arr: &mut Vec<Vec<char>>, init_guard_loc: (usize, usize)) -> i32 {
        traverse(char_arr, init_guard_loc);
        let final_str: String = char_arr
            .iter()
            .map(|s| s.iter().collect::<String>())
            .collect::<String>();
        final_str.matches("X").count().try_into().unwrap()
    }
    fn find_num_loop_locs(
        char_arr: &mut Vec<Vec<char>>,
        init_guard_loc: (usize, usize),
        init_guard_dir: char,
    ) -> i32 {
        let mut visited_locs = Vec::new();
        for (row_idx, row) in char_arr.iter().enumerate() {
            for (col_idx, cel) in row.iter().enumerate() {
                if *cel == 'X' {
                    visited_locs.push((row_idx, col_idx));
                }
            }
        }
        let mut num_possible_loops: i32 = 0;
        for loc in visited_locs.iter() {
            // reset board to original state
            for loc_1 in visited_locs.iter() {
                char_arr[loc_1.0][loc_1.1] = '.'
            }
            char_arr[init_guard_loc.0][init_guard_loc.1] = init_guard_dir;
            if *loc != init_guard_loc {
                char_arr[loc.0][loc.1] = 'O';
                if !traverse(char_arr, init_guard_loc) {
                    num_possible_loops += 1;
                }
            }
        }
        num_possible_loops
    }
    fn traverse(char_arr: &mut Vec<Vec<char>>, init_guard_loc: (usize, usize)) -> bool {
        let mut dir_arr: HashMap<(i32, i32), Vec<char>> = HashMap::new();
        let num_rows = char_arr.len();
        let num_cols = char_arr[0].len();
        let mut guard_loc: (i32, i32) = (
            init_guard_loc.0.try_into().unwrap(),
            init_guard_loc.1.try_into().unwrap(),
        );
        while !off_grid(guard_loc, num_rows, num_cols) {
            // find the guard
            let dir = char_arr[guard_loc.0 as usize][guard_loc.1 as usize];
            char_arr[guard_loc.0 as usize][guard_loc.1 as usize] = 'X';
            dir_arr
                .entry(guard_loc)
                .and_modify(|ch| ch.push(dir))
                .or_insert(vec![dir]);
            let (new_loc, new_dir) = new_guard_loc(guard_loc, dir, char_arr);
            if off_grid(new_loc, num_rows, num_cols) {
                // walked off the edge
                return true;
            }
            // if we didn't walk off the edge, check if we've hit a loop
            if char_arr[new_loc.0 as usize][new_loc.1 as usize] == 'X' {
                if let Some(char_list) = dir_arr.get(&new_loc) {
                    if let Some(_) = char_list.iter().find(|&c| *c == new_dir) {
                        // been here before
                        return false;
                    }
                }
            }
            char_arr[new_loc.0 as usize][new_loc.1 as usize] = new_dir;
            guard_loc = new_loc;
        }
        true
    }
    fn is_guard(sym: char) -> bool {
        sym == 'v' || sym == '^' || sym == '<' || sym == '>'
    }
    fn new_guard_loc(loc: (i32, i32), dir: char, char_arr: &Vec<Vec<char>>) -> ((i32, i32), char) {
        let mut new_loc: (i32, i32);
        let mut new_dir: char = dir;
        match dir {
            'v' => {
                new_loc = (loc.0 + 1, loc.1);
                if !off_grid(new_loc, char_arr.len(), char_arr[0].len()) {
                    if !(char_arr[new_loc.0 as usize][new_loc.1 as usize] == 'X'
                        || char_arr[new_loc.0 as usize][new_loc.1 as usize] == '.')
                    {
                        new_loc = loc;
                        new_dir = '<';
                    }
                }
            }
            '<' => {
                new_loc = (loc.0, loc.1 - 1);
                if !off_grid(new_loc, char_arr.len(), char_arr[0].len()) {
                    if !(char_arr[new_loc.0 as usize][new_loc.1 as usize] == 'X'
                        || char_arr[new_loc.0 as usize][new_loc.1 as usize] == '.')
                    {
                        new_loc = loc;
                        new_dir = '^';
                    }
                }
            }
            '^' => {
                new_loc = (loc.0 - 1, loc.1);
                if !off_grid(new_loc, char_arr.len(), char_arr[0].len()) {
                    if !(char_arr[new_loc.0 as usize][new_loc.1 as usize] == 'X'
                        || char_arr[new_loc.0 as usize][new_loc.1 as usize] == '.')
                    {
                        new_loc = loc;
                        new_dir = '>';
                    }
                }
            }
            '>' => {
                new_loc = (loc.0, loc.1 + 1);
                if !off_grid(new_loc, char_arr.len(), char_arr[0].len()) {
                    if !(char_arr[new_loc.0 as usize][new_loc.1 as usize] == 'X'
                        || char_arr[new_loc.0 as usize][new_loc.1 as usize] == '.')
                    {
                        new_loc = loc;
                        new_dir = 'v';
                    }
                }
            }
            _ => {
                println!("{dir}");
                panic! {"bad direction!"};
            }
        };
        (new_loc, new_dir)
    }
    fn off_grid(loc: (i32, i32), row_len: usize, col_len: usize) -> bool {
        loc.0 < 0 || loc.1 < 0 || loc.0 >= row_len as i32 || loc.1 >= col_len as i32
    }
    #[test]
    fn run_day6_test_numlocs() {
        let test_input = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";
        let lines = test_input
            .lines()
            .map(|s| String::from(s))
            .collect::<Vec<_>>();
        let mut char_arr: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();
        let mut init_guard_loc: (usize, usize) = (0, 0);
        for (idx, row) in char_arr.iter().enumerate() {
            if let Some(loc) = row.iter().position(|&c| is_guard(c)) {
                init_guard_loc = (idx, loc);
                break;
            }
        }
        assert_eq!(find_num_pos(&mut char_arr, init_guard_loc), 41);
    }

    #[test]
    fn run_day6_test_numloops() {
        let test_input = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";
        let lines = test_input
            .lines()
            .map(|s| String::from(s))
            .collect::<Vec<_>>();
        let char_arr_init: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();
        let mut char_arr = char_arr_init.clone();
        let mut init_guard_loc: (usize, usize) = (0, 0);
        for (idx, row) in char_arr_init.iter().enumerate() {
            if let Some(loc) = row.iter().position(|&c| is_guard(c)) {
                init_guard_loc = (idx, loc);
                break;
            }
        }
        let init_guard_dir = char_arr_init[init_guard_loc.0][init_guard_loc.1];
        traverse(&mut char_arr, init_guard_loc);
        assert_eq!(
            find_num_loop_locs(&mut char_arr, init_guard_loc, init_guard_dir),
            6
        );
    }
} // mod day6
mod day7 {
    use crate::get_lines;
    use crate::AdventOfCodeError;
    use radix_fmt::radix;
    pub fn run_day7(fname: &str) -> Result<(), AdventOfCodeError> {
        let lines = get_lines(fname);
        let mut equations: Vec<(u64, Vec<u64>)> = Vec::new();
        for line in lines.iter() {
            let sides: Vec<&str> = line.split(":").collect();
            let (key, els) = (
                sides[0].parse::<u64>().unwrap(),
                sides[1]
                    .trim()
                    .split(" ")
                    .map(|s| s.parse::<u64>().unwrap())
                    .collect::<Vec<_>>(),
            );
            equations.push((key, els));
        }
        let part1 = total_calibration_result(&equations, 2);
        let part2 = total_calibration_result(&equations, 3);
        println!("p1: {:?}", part1);
        println!("p2: {:?}", part2);
        Ok(())
    }
    fn total_calibration_result(equations: &Vec<(u64, Vec<u64>)>, base: u8) -> (u128, u32) {
        let mut total: u128 = 0;
        let mut num_valid = 0;
        for (result, equation) in equations.iter() {
            if can_be_true(*result, equation, base) {
                total += *result as u128;
                num_valid += 1;
            }
        }
        (total, num_valid)
    }
    fn can_be_true(test_val: u64, operands: &Vec<u64>, base: u8) -> bool {
        let num_operators = operands.len();
        let num_combos = (base as usize).pow(num_operators as u32) as usize;
        for ii in 0..num_combos {
            let mut itr = operands.iter();
            let mut ans = *itr.next().unwrap();
            let nary = format! {"{:0>num_operators$}", format!{"{}", radix(ii,base)}};
            for (val, op) in itr.zip(nary.chars()) {
                if op == '0' {
                    ans += val;
                } else if op == '1' {
                    ans *= val;
                } else if op == '2' {
                    ans = format! {"{ans}{val}"}.parse::<u64>().unwrap();
                } else {
                    panic! {"whaaa?"};
                }
                if ans > test_val {
                    break;
                }
            }
            if ans == test_val {
                return true;
            }
        } // cycle through possible combos of operators
        false
    }
    #[test]
    fn test_part1() {
        let test_input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20";
        let lines: Vec<String> = test_input.lines().map(|s| String::from(s)).collect();
        let mut equations: Vec<(u64, Vec<u64>)> = vec![];
        for line in lines {
            let sides: Vec<&str> = line.split(":").collect();
            let (key, els) = (
                sides[0].parse::<u64>().unwrap(),
                sides[1]
                    .trim()
                    .split(" ")
                    .map(|s| s.parse::<u64>().unwrap())
                    .collect::<Vec<_>>(),
            );
            equations.push((key, els));
        }
        let (total, num_valid) = total_calibration_result(&equations, 2);
        assert_eq!(total, 3749);
        assert_eq!(num_valid, 3);
    }
    #[test]
    fn test_part2() {
        let test_input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20";
        let lines: Vec<String> = test_input.lines().map(|s| String::from(s)).collect();
        let mut equations: Vec<(u64, Vec<u64>)> = vec![];
        for line in lines {
            let sides: Vec<&str> = line.split(":").collect();
            let (key, els) = (
                sides[0].parse::<u64>().unwrap(),
                sides[1]
                    .trim()
                    .split(" ")
                    .map(|s| s.parse::<u64>().unwrap())
                    .collect::<Vec<_>>(),
            );
            equations.push((key, els));
        }
        let (total, num_valid) = total_calibration_result(&equations, 3);
        assert_eq!(total, 11387);
        assert_eq!(num_valid, 6);
    }
} // mod day7
mod day8 {
    // type Num = i32;  // type alias - could be handy
    use crate::get_lines;
    use crate::AdventOfCodeError;
    use std::collections::HashMap;
    pub fn run_day8(fname: &str) -> Result<(), AdventOfCodeError> {
        let lines = get_lines(fname);
        let char_arr: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();
        let mut antenna_map: HashMap<char, Vec<(usize, usize)>> = HashMap::new();
        for (ii, row) in char_arr.iter().enumerate() {
            for (jj, &cell_val) in row.iter().enumerate() {
                if cell_val != '.' {
                    antenna_map
                        .entry(cell_val)
                        .and_modify(|v| {
                            if let Some(_) = v.iter().find(|&&c| c == (ii, jj)) {
                            } else {
                                v.push((ii, jj));
                            }
                        })
                        .or_insert(vec![(ii, jj)]);
                }
            }
        }
        let part1 = num_antinode_locs(&char_arr, &antenna_map, true);
        let part2 = num_antinode_locs(&char_arr, &antenna_map, false);
        println!("{part1}");
        println!("{part2}");
        Ok(())
    }
    // number of unique antinode locations
    fn num_antinode_locs(
        char_arr: &Vec<Vec<char>>,
        antenna_map: &HashMap<char, Vec<(usize, usize)>>,
        double_dist: bool,
    ) -> usize {
        // store locations of anodes
        let (num_rows, num_cols) = (char_arr.len(), char_arr[0].len());
        let mut anode_locs: Vec<(usize, usize)> = Vec::new();
        for (_, locs) in antenna_map.iter() {
            let mut loc_iter = locs.iter();
            while let Some(first_loc) = loc_iter.next() {
                for sec_loc in locs.iter() {
                    if sec_loc == first_loc {
                        continue;
                    }
                    let mut roc = (
                        // rate of change
                        first_loc.0 as i64 - sec_loc.0 as i64,
                        first_loc.1 as i64 - sec_loc.1 as i64,
                    );
                    if roc.0 > 0 && roc.1 > 0 {
                        let gcf = gcd(roc.0 as u64, roc.1 as u64) as i64;
                        roc = (roc.0 / gcf, roc.1 / gcf);
                    }
                    let mut edge = (first_loc.0 as i64, first_loc.1 as i64);
                    loop {
                        let diff = (edge.0 - roc.0, edge.1 - roc.1);
                        if diff.0 >= 0
                            && diff.1 >= 0
                            && diff.0 < num_rows.try_into().unwrap()
                            && diff.1 < num_cols.try_into().unwrap()
                        {
                            edge = (edge.0 - roc.0, edge.1 - roc.1);
                        } else {
                            break;
                        }
                    }
                    let mut test_locs: Vec<(usize, usize)> = Vec::new();
                    let mut marker = edge;
                    while marker.0 >= 0
                        && marker.1 >= 0
                        && (marker.0 as usize) < num_rows
                        && (marker.1 as usize) < num_cols
                    {
                        // get distance from the two antennas
                        if double_dist {
                            let dist1 = dist(*first_loc, (marker.0 as usize, marker.1 as usize));
                            let dist2 = dist(*sec_loc, (marker.0 as usize, marker.1 as usize));
                            if dist1 == dist2 * 2.0 || dist1 * 2.0 == dist2 {
                                test_locs.push((marker.0 as usize, marker.1 as usize));
                            }
                        } else {
                            test_locs.push((marker.0 as usize, marker.1 as usize));
                        }
                        marker = (marker.0 + roc.0, marker.1 + roc.1);
                    }
                    for test_loc in test_locs.iter() {
                        if anode_locs.iter().find(|&c| *c == *test_loc).is_none()
                            && is_valid_loc(*test_loc, num_rows, num_cols)
                        {
                            // now check if one antenna is twice as far from potential antinode as the
                            // other
                            anode_locs.push(*test_loc);
                        }
                    }
                }
            }
        } // iter over freq in antenna map
        anode_locs.len()
    }
    fn dist(loc: (usize, usize), marker: (usize, usize)) -> f64 {
        let p1 = (loc.0 as f64, loc.1 as f64);
        let p2 = (marker.0 as f64, marker.1 as f64);
        ((p1.0 - p2.0) * (p1.0 - p2.0) + (p1.1 - p2.1) * (p1.1 - p2.1)).sqrt()
    }
    // check if location exists on board
    fn is_valid_loc(loc: (usize, usize), num_rows: usize, num_cols: usize) -> bool {
        loc.0 <= num_rows && loc.1 <= num_cols
    }
    fn gcd(mut n: u64, mut m: u64) -> u64 {
        // from victor-iyi on github
        assert!(n != 0 && m != 0);
        while m != 0 {
            if m < n {
                std::mem::swap(&mut m, &mut n);
            }
            m %= n;
        }
        n
    }
    #[test]
    fn test_day8() {
        let test_input = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";
        let lines = test_input
            .lines()
            .map(|s| String::from(s))
            .collect::<Vec<_>>();
        let char_arr: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();
        let mut antenna_map: HashMap<char, Vec<(usize, usize)>> = HashMap::new();
        for (ii, row) in char_arr.iter().enumerate() {
            for (jj, &cell_val) in row.iter().enumerate() {
                if cell_val != '.' {
                    antenna_map
                        .entry(cell_val)
                        .and_modify(|v| {
                            if let Some(_) = v.iter().find(|&&c| c == (ii, jj)) {
                            } else {
                                v.push((ii, jj));
                            }
                        })
                        .or_insert(vec![(ii, jj)]);
                }
            }
        }
        assert_eq!(num_antinode_locs(&char_arr, &antenna_map, true), 14);
        assert_eq!(num_antinode_locs(&char_arr, &antenna_map, false), 34);
    }
} // mod day8
mod day9 {
    use crate::fs;
    use crate::AdventOfCodeError;
    // use crate::get_lines;
    pub fn run_day9(fname: &str) -> Result<(), AdventOfCodeError> {
        let contents = &fs::read_to_string(fname).unwrap()[..];
        let new_file_map_p1 = compact_file_blocks(contents.trim());
        let new_file_map_p2 = compact_whole_files(contents.trim());
        println!("p1: {}", checksum(new_file_map_p1));
        println!("p2: {}", checksum(new_file_map_p2));
        Ok(())
    }
    fn compact_file_blocks(file_map: &str) -> Vec<i64> {
        let mut new_file_map: Vec<i64> = Vec::new();
        let mut file_id: i64 = 0;
        for (ii, val) in file_map.chars().enumerate() {
            let val_num: usize = format! {"{val}"}
                .parse::<usize>()
                .expect("that should've worked");
            if ii % 2 == 0 {
                // it's a file
                for _ in 0..val_num {
                    new_file_map.push(file_id);
                }
                file_id += 1;
            } else {
                // it's a space
                for _ in 0..val_num {
                    new_file_map.push(-1);
                }
            }
        }
        let mut swapped_map: Vec<i64> = new_file_map
            .clone()
            .iter()
            .map(|c| format! {"{c}"}.parse::<i64>().expect("this should be fine"))
            .collect();
        let max_idx = swapped_map.len() - 1;
        for (ii, &el) in new_file_map.iter().collect::<Vec<_>>().iter().enumerate() {
            if swapped_map[ii..].iter().find(|&a| *a > 0).is_none() {
                // no more numbers after this point; we done
                break;
            }
            if *el == -1 {
                if let Some((idx, _)) = swapped_map.iter().rev().enumerate().find(|(_, &a)| a > 0) {
                    if ii >= max_idx - idx {
                        panic!("a bug!");
                    }
                    swapped_map.swap(ii, max_idx - idx);
                }
            }
        }
        swapped_map
    }
    fn compact_whole_files(file_map: &str) -> Vec<i64> {
        let mut new_file_map: Vec<(i64, usize)> = Vec::new();
        let mut file_id: i64 = 0;
        for (ii, val) in file_map.chars().enumerate() {
            let val_num: usize = format! {"{val}"}
                .parse::<usize>()
                .expect("that should've worked");
            if ii % 2 == 0 {
                // it's a file
                new_file_map.push((file_id, val_num));
                file_id += 1;
            } else {
                // it's a space
                new_file_map.push((-1, val_num));
            }
        }
        let mut swapped_map: Vec<(i64, usize)> = new_file_map.clone();
        let file_blocks = new_file_map
            .iter()
            .rev()
            .filter(|(f, _)| *f > 0)
            .collect::<Vec<_>>();
        for (blk_fid, blk_size) in file_blocks.iter() {
            let swapped_copy = swapped_map.clone();
            if let Some((idx, (_, _))) = swapped_copy
                .iter()
                .enumerate()
                .find(|(_, (f, _))| f == blk_fid)
            {
                for (ii, (fid, space_size)) in swapped_copy.iter().enumerate() {
                    if *fid != -1 {
                        continue;
                    }
                    if ii >= idx {
                        break;
                    }
                    if *space_size == *blk_size {
                        swapped_map.swap(ii, idx);
                        break;
                    } else if *space_size > *blk_size {
                        swapped_map[ii] = (*blk_fid, *blk_size);
                        swapped_map[idx] = (-1, *blk_size);
                        swapped_map.insert(ii + 1, (-1, space_size - blk_size));
                        break;
                    }
                }
            }
        } // iterate over file blocks
        let mut final_map: Vec<i64> = Vec::new();
        for &(file_id, size) in swapped_map.iter() {
            for _ in 0..size {
                final_map.push(file_id);
            }
        }
        final_map
    }
    fn checksum(file_map: Vec<i64>) -> i64 {
        let mut checksum: i64 = 0;
        for (pos, file_id) in file_map.iter().enumerate() {
            if *file_id < 0 {
                continue;
            }
            let file_id_val: i64 = format! {"{file_id}"}
                .parse::<i64>()
                .expect("parsing error in checksum (file id)");
            let pos_val: i64 = format! {"{pos}"}
                .parse::<i64>()
                .expect("parsing error in checksum (position)");
            checksum += file_id_val * pos_val;
        }
        checksum
    }
    #[test]
    fn test_day9_p1() {
        let test_input = "2333133121414131402";
        let new_file_map = compact_file_blocks(test_input);
        assert_eq!(checksum(new_file_map), 1928);
    }
    #[test]
    fn test_day9_p2() {
        let test_input = "2333133121414131402";
        let new_file_map = compact_whole_files(test_input);
        assert_eq!(checksum(new_file_map), 2858);
    }
} // mod day9
