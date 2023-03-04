use std::collections::HashSet;

use aoc::Solver;

pub struct Solution;

impl Solution {
    fn all_unique(seq: &[char]) -> bool {
        let mut seen: HashSet<char> = HashSet::new();
        for &c in seq.iter() {
            if !seen.insert(c) {
                return false;
            }
        }
        true
    }

    fn find_window(seq: &str, n: usize) -> i32 {
        let v: Vec<_> = seq.chars().collect();
        for (i, window) in v.windows(n).enumerate() {
            if Self::all_unique(window) {
                return (i + n) as i32;
            }
        }
        unreachable!()
    }

    fn find_marker(seq: &str) -> i32 {
        Self::find_window(seq, 4)
    }

    fn find_message(seq: &str) -> i32 {
        Self::find_window(seq, 14)
    }
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        Solution::find_marker(input[0]).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        Solution::find_message(input[0]).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_marker() {
        assert_eq!(7, Solution::find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb"));
        assert_eq!(5, Solution::find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz"));
        assert_eq!(6, Solution::find_marker("nppdvjthqldpwncqszvftbrmjlhg"));
        assert_eq!(
            10,
            Solution::find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
        );
        assert_eq!(
            11,
            Solution::find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
        );
    }

    #[test]
    fn test_find_message() {
        assert_eq!(19, Solution::find_message("mjqjpqmgbljsphdztnvjfqwrcgsmlb"));
        assert_eq!(23, Solution::find_message("bvwbjplbgvbhsrlpgdmjqwftvncz"));
        assert_eq!(23, Solution::find_message("nppdvjthqldpwncqszvftbrmjlhg"));
        assert_eq!(
            29,
            Solution::find_message("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
        );
        assert_eq!(
            26,
            Solution::find_message("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
        );
    }
}
