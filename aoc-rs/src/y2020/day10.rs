use std::str::FromStr;

use aoc::Solver;

const DIFF: usize = 3;

fn arrangements(jolts: &[usize]) -> usize {
    let len = jolts.len();
    let mut dp = vec![0; len];
    dp[len - 1] = 1;

    for i in (0..len).rev() {
        for j in 1..=DIFF.min((len - 1) - i) {
            if jolts[i + j] <= jolts[i] + DIFF {
                dp[i] += dp[i + j];
            }
        }
    }

    dp[0]
}

fn jolt_distribution(jolts: &[usize]) -> (usize, usize, usize) {
    (1..jolts.len()).fold((0, 0, 0), |(a, b, c), i| match jolts[i] - jolts[i - 1] {
        1 => (a + 1, b, c),
        2 => (a, b + 1, c),
        3 => (a, b, c + 1),
        _ => panic!("You got jolted!"),
    })
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let mut jolts: Vec<usize> = input
            .iter()
            .map(|line| usize::from_str(line).unwrap())
            .collect();

        jolts.push(0); // charging outlet
        jolts.push(jolts.iter().max().unwrap() + 3); // the device
        let _ = &jolts.sort_unstable();

        let (d1, _, d3) = jolt_distribution(&jolts);
        (d1 * d3).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let mut jolts: Vec<usize> = input
            .iter()
            .map(|line| usize::from_str(line).unwrap())
            .collect();

        jolts.push(0); // charging outlet
        jolts.push(jolts.iter().max().unwrap() + 3); // the device
        let _ = &jolts.sort_unstable();
        arrangements(&jolts).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ADAPTERS1: [usize; 13] = [0, 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 22];

    const ADAPTERS2: [usize; 33] = [
        28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8,
        17, 7, 9, 4, 2, 34, 10, 3, 0, 52,
    ];

    #[test]
    fn test_jolt_distribution1() {
        let mut jolts = ADAPTERS1.to_vec().clone();
        let _ = &jolts.sort_unstable();

        assert_eq!((7, 0, 5), jolt_distribution(&jolts));
    }

    #[test]
    fn test_jolt_distribution2() {
        let mut jolts = ADAPTERS2.to_vec().clone();
        let _ = &jolts.sort_unstable();

        assert_eq!((22, 0, 10), jolt_distribution(&jolts));
    }

    #[test]
    fn test_arrangements1() {
        let mut jolts = ADAPTERS1.to_vec().clone();
        let _ = &jolts.sort_unstable();

        assert_eq!(8, arrangements(&jolts));
    }

    #[test]
    fn test_arrangements2() {
        let mut jolts = ADAPTERS2.to_vec().clone();
        let _ = &jolts.sort_unstable();

        assert_eq!(19208, arrangements(&jolts));
    }
}
