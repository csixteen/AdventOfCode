// https://adventofcode.com/2020/day/1

use std::cmp::Ordering;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::str::FromStr;


const TARGET_SUM: i32 = 2020;


//--------------------------------------------------
// Part 1

fn two_sum_part1(nums: &Vec<i32>) -> (i32, i32) {
    let mut numbers = HashSet::new();

    for i in nums.iter() {
        match numbers.get(&(TARGET_SUM - i)) {
            None => { numbers.insert(i); },
            Some(&n) => { return (*i, *n); },
        }
    }

    unreachable!();
}

//--------------------------------------------------
// Part 2

fn two_sum_part2(nums: &Vec<i32>, i: usize) -> Option<(i32, i32, i32)> {
    let (mut lo, mut hi) = (i+1, nums.len() - 1);

    while lo < hi {
        match (nums[i] + nums[lo] + nums[hi]).cmp(&TARGET_SUM) {
            Ordering::Less => { lo += 1; },
            Ordering::Greater => { hi -= 1; },
            Ordering::Equal => { return Some((nums[i], nums[lo], nums[hi])); }
        }
    }

    None
}

fn three_sum_part2(nums: Vec<i32>) -> (i32, i32, i32) {
    let mut nums = nums;

    &nums.sort_unstable();

    for i in 0..nums.len() {
        match two_sum_part2(&nums, i) {
            None => { continue; },
            Some(x) => { return x; },
        }
    }

    unreachable!();
}

//--------------------------------------------------
// Main

fn main() {
    let mut buffer = String::new();
    let numbers: Vec<i32>;

    match File::open("data/input.txt") {
        Err(_) => panic!("Error reading <input.txt>"),
        Ok(mut file) => {
            file.read_to_string(&mut buffer).unwrap();
            numbers = buffer
                .trim()
                .split("\n")
                .map(|n| { i32::from_str(n).unwrap() })
                .collect();
        }
    }

    let (a, b) = two_sum_part1(&numbers);
    let (c, d, e) = three_sum_part2(numbers.clone());

    println!("Day 1 / Part 1: {}", a*b);
    println!("Day 1 / Part 2: {}", c*d*e);
}
