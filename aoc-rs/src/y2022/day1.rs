use aoc::Solver;
use itertools::Itertools;
use std::str::FromStr;

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        input
            .split(|s| s.is_empty())
            .filter(|&p| !p.is_empty())
            .map(|c| c.iter().map(|&i| i32::from_str(i).unwrap()).sum::<i32>())
            .max()
            .unwrap()
            .to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        input
            .split(|s| s.is_empty())
            .filter(|&p| !p.is_empty())
            .map(|c| c.iter().map(|&i| i32::from_str(i).unwrap()).sum::<i32>())
            .sorted_by(|a, b| b.partial_cmp(a).unwrap())
            .take(3)
            .sum::<i32>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1() {
        let solver = Solution;
        assert_eq!(
            "24000".to_string(),
            solver.part1(&vec![
                "1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000",
                "", "10000"
            ])
        )
    }

    #[test]
    fn part2() {
        let solver = Solution;
        assert_eq!(
            "45000".to_string(),
            solver.part2(&vec![
                "1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000",
                "", "10000"
            ])
        )
    }
}
