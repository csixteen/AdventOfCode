// https://adventofcode.com/2020/day/1

#![allow(non_snake_case)]

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

fn three_sum_part2(numbers: &Vec<i32>) -> (i32, i32, i32) {
    let mut nums = numbers.clone();

    &nums.sort_unstable();

    (0..nums.len()).find_map(|i| two_sum_part2(&nums, i)).unwrap()
}

//--------------------------------------------------
// Main

fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?;

    file.read_to_string(&mut buffer).unwrap();
    let numbers: Vec<i32> = buffer
        .trim()
        .split("\n")
        .map(|n| { i32::from_str(n).unwrap() })
        .collect();


    let (a, b) = two_sum_part1(&numbers);
    let (c, d, e) = three_sum_part2(&numbers);

    println!("Day 1 / Part 1: {}", a*b);
    println!("Day 1 / Part 2: {}", c*d*e);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_two_sum_part1() {
        assert_eq!(
            (299, 1721),
            two_sum_part1(&vec![1721, 979, 366, 299, 675, 1456]),
        );
    }

    #[test]
    fn test_three_sum_part2() {
        assert_eq!(
            (366, 675, 979),
            three_sum_part2(&vec![1721, 979, 366, 299, 675, 1456]),
        );
    }
}
