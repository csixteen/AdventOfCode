// MIT License
//
// Copyright (c) 2020 Pedro Rodrigues
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// https://adventofcode.com/2020/day/1

#![allow(non_snake_case)]

use std::cmp::Ordering;
use std::str::FromStr;

use aoc::fs::get_file_contents;
use aoc::math::two_sum;


const TARGET_SUM: i32 = 2020;


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

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;
    let numbers: Vec<i32> = lines
        .iter()
        .map(|line| i32::from_str(line).unwrap())
        .collect();


    let (a, b) = two_sum(&numbers, TARGET_SUM).unwrap();
    let (c, d, e) = three_sum_part2(&numbers);

    println!("Day 1 / Part 1: {}", a*b);
    println!("Day 1 / Part 2: {}", c*d*e);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_three_sum_part2() {
        assert_eq!(
            (366, 675, 979),
            three_sum_part2(&vec![1721, 979, 366, 299, 675, 1456]),
        );
    }
}
