use std::str::FromStr;

use aoc::Solver;

struct Section(i32, i32);

impl Section {
    fn new(s: &str) -> Self {
        let mut parts = s.split("-").map(|p| i32::from_str(p).unwrap());
        Section(parts.next().unwrap(), parts.next().unwrap())
    }

    fn contained(&self, other: &Section) -> bool {
        (self.0 <= other.0 && self.1 >= other.1) || (other.0 <= self.0 && other.1 >= self.1)
    }

    fn overlaps(&self, other: &Section) -> bool {
        self.contained(other)
            || (self.1 >= other.0 && self.0 <= other.0)
            || (other.1 >= self.0 && other.0 <= self.0)
    }
}

pub struct Solution;

impl Solution {
    fn solve(input: &Vec<&str>, f: fn(&Section, &Section) -> bool) -> String {
        input
            .iter()
            .map(|&line| {
                let mut elves = line.split(",");
                let elf1 = Section::new(elves.next().unwrap());
                let elf2 = Section::new(elves.next().unwrap());
                f(&elf1, &elf2)
            })
            .filter(|&p| p)
            .count()
            .to_string()
    }

    fn contained(elf1: &Section, elf2: &Section) -> bool {
        elf1.contained(elf2)
    }

    fn overlap(elf1: &Section, elf2: &Section) -> bool {
        elf1.overlaps(elf2)
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        Solution::solve(input, Solution::contained)
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        Solution::solve(input, Solution::overlap)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1() {
        let s = Solution;
        assert_eq!(
            "2",
            s.part1(&vec![
                "2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"
            ])
        )
    }

    #[test]
    fn part2() {
        let s = Solution;
        assert_eq!(
            "4",
            s.part2(&vec![
                "2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"
            ])
        )
    }
}
