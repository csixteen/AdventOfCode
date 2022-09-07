#![allow(non_snake_case)]

use std::cmp::Ordering;
use std::str::FromStr;

use aoc::Solver;
use aoc::math::two_sum;


const TARGET_SUM: i32 = 2020;

struct Solution;

impl Solution {
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

        let _ = &nums.sort_unstable();

        (0..nums.len()).find_map(|i| Self::two_sum_part2(&nums, i)).unwrap()
    }

    fn numbers(input: &Vec<&str>) -> Vec<i32> {
        input
            .iter()
            .map(|line| i32::from_str(line).unwrap())
            .collect()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        let numbers: Vec<i32> = Self::numbers(input);
        let (a, b) = two_sum(&numbers, TARGET_SUM).unwrap();
        (a*b).to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        let numbers: Vec<i32> = Self::numbers(input);
        let (a, b, c) = Solution::three_sum_part2(&numbers);
        (a*b*c).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_three_sum_part2() {
        assert_eq!(
            (366, 675, 979),
            Solution::three_sum_part2(&vec![1721, 979, 366, 299, 675, 1456]),
        );
    }
}
