use std::cmp::Ordering;
use std::str::FromStr;

use aoc::math::two_sum;

use aoc::Solver;

fn find_incorrect(numbers: &Vec<i64>, preamble: usize) -> Option<usize> {
    (preamble..numbers.len())
        .find(|&i| two_sum(&numbers[i - preamble..i].to_vec(), numbers[i]).is_none())
}

fn crack_xmas(numbers: &Vec<i64>, preamble: usize) -> i64 {
    match find_incorrect(numbers, preamble) {
        None => panic!("Uncrackable!"),
        Some(i) => numbers[i],
    }
}

fn find_min_max(numbers: &Vec<i64>, start: usize, end: usize, t: i64) -> Option<(i64, i64)> {
    let mut _min = i64::MAX;
    let mut _max = i64::MIN;
    let mut sum = 0_i64;

    for j in start..end {
        sum += numbers[j];

        match sum.cmp(&t) {
            Ordering::Equal => {
                return Some((_min, _max));
            }
            Ordering::Greater => break,
            Ordering::Less => {
                _min = _min.min(numbers[j]);
                _max = _max.max(numbers[j]);
            }
        }
    }

    None
}

fn crack_me(numbers: &Vec<i64>, index: usize) -> i64 {
    let target_sum = numbers[index];

    for i in 0..index {
        if numbers[i] >= target_sum {
            continue;
        }

        if let Some((_min, _max)) = find_min_max(numbers, i, index, target_sum) {
            return _min + _max;
        }
    }

    unreachable!();
}

fn crack_xmas2(numbers: &Vec<i64>, preamble: usize) -> i64 {
    let i = match find_incorrect(numbers, preamble) {
        None => panic!("Uncrackable!"),
        Some(n) => n,
    };

    crack_me(numbers, i)
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let numbers: Vec<i64> = input
            .iter()
            .map(|line| i64::from_str(line).unwrap())
            .collect();
        crack_xmas(&numbers, 25).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let numbers: Vec<i64> = input
            .iter()
            .map(|line| i64::from_str(line).unwrap())
            .collect();
        crack_xmas2(&numbers, 25).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const NUMBERS: [i64; 20] = [
        35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576,
    ];

    #[test]
    fn test_crack_xmas() {
        assert_eq!(127, crack_xmas(&NUMBERS.to_vec(), 5),);
    }

    #[test]
    fn test_crack_xmas2() {
        assert_eq!(62, crack_xmas2(&NUMBERS.to_vec(), 5),);
    }
}
