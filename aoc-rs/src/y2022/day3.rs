use aoc::Solver;
use std::collections::HashSet;

type Compartment = HashSet<char>;

pub struct Solution;

impl Solution {
    fn str_to_set(s: &str) -> Compartment {
        s.chars().fold(HashSet::new(), |mut acc, c| {
            acc.insert(c);
            acc
        })
    }

    fn split_rucksack(s: &str) -> (Compartment, Compartment) {
        let l = s.len() / 2;
        (Self::str_to_set(&s[..l]), Self::str_to_set(&s[l..]))
    }

    fn priority(c: char) -> i32 {
        match c {
            'a'..='z' => c as i32 - 96,
            'A'..='Z' => c as i32 - 38,
            _ => unreachable!(),
        }
    }

    fn group_badge(group: &[&str]) -> char {
        let elfs: Vec<_> = group.iter().map(|&s| Self::str_to_set(s)).collect();
        let i1: HashSet<char> = elfs[0].intersection(&elfs[1]).map(|c| *c).collect();
        *i1.intersection(&elfs[2]).next().unwrap()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        input
            .iter()
            .map(|&rucksack| Solution::split_rucksack(rucksack))
            .map(|(c1, c2)| {
                c1.intersection(&c2)
                    .map(|i| Solution::priority(*i))
                    .sum::<i32>()
            })
            .sum::<i32>()
            .to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        input
            .chunks(3)
            .map(|chunk| Solution::priority(Solution::group_badge(chunk)))
            .sum::<i32>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_priority() {
        assert_eq!(1, Solution::priority('a'));
        assert_eq!(27, Solution::priority('A'));
    }

    #[test]
    fn test_str_to_compartment() {
        let s = String::from("ttgJtRGJQctTZtZT");
        let comp = Solution::str_to_set(&s);
        assert_eq!(9, comp.len());
        s.chars().for_each(|c| assert!(comp.contains(&c)));
    }

    #[test]
    fn test_split_rucksack() {
        let (c1, c2) = Solution::split_rucksack("vJrwpWtwJgWrhcsFMMfFFhFp");
        let common: Vec<&char> = c1.intersection(&c2).collect();
        assert_eq!(vec![&'p'], common);
    }

    #[test]
    fn part1() {
        let solver = Solution;
        assert_eq!(
            "157",
            solver.part1(&vec![
                "vJrwpWtwJgWrhcsFMMfFFhFp",
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                "PmmdzqPrVvPwwTWBwg",
                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                "ttgJtRGJQctTZtZT",
                "CrZsJsPPZsGzwwsLwLmpwMDw"
            ])
        );
    }

    #[test]
    fn part2() {
        let solver = Solution;
        assert_eq!(
            "70",
            solver.part2(&vec![
                "vJrwpWtwJgWrhcsFMMfFFhFp",
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                "PmmdzqPrVvPwwTWBwg",
                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                "ttgJtRGJQctTZtZT",
                "CrZsJsPPZsGzwwsLwLmpwMDw"
            ])
        );
    }
}
