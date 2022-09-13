#![allow(non_snake_case)]

use aoc::Solver;

pub struct Solution;

impl Solution {
    fn individual_answers(answers: &str) -> u32 {
        answers.bytes().fold(0, |acc, c| {
            acc | 0x1 << (c - b'a')
        })
    }

    fn count_group_answers(group: &Vec<&str>) -> usize {
        group.iter().fold(0_u32, |acc, answers| {
            acc | Self::individual_answers(answers)
        }).count_ones() as usize
    }

    fn count_total_answers(groups: &Vec<Vec<&str>>) -> usize {
        groups.iter().fold(0, |acc, group| {
            acc + Self::count_group_answers(group)
        })
    }

    fn count_group_all_yes(group: &Vec<&str>) -> usize {
        group
            .iter()
            .fold([0; 26], |mut acc, person| {
                for c in person.bytes() {
                    acc[(c - b'a') as usize] += 1;
                }
                acc
            })
            .iter()
            .filter(|a| **a == group.len())
            .count()
    }

    fn count_total_all_yes(groups: &Vec<Vec<&str>>) -> usize {
        groups.iter().fold(0, |acc, group| {
            acc + Self::count_group_all_yes(group)
        })
    }

    fn build_groups(mut input: Vec<&str>) -> Vec<Vec<&str>> {
        input
            .as_mut_slice()
            .split(|line| line.is_empty())
            .map(|x| x.to_vec())
            .collect()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        let groups = Self::build_groups(input.clone());
        Self::count_total_answers(&groups).to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        let groups = Self::build_groups(input.clone());
        Self::count_total_all_yes(&groups).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn proper_vec(v: Vec<&str>) -> Vec<String> {
        v.iter().map(|&x| String::from(x)).collect()
    }

    #[test]
    fn test_individual_answers() {
        assert_eq!(0x7, Solution::individual_answers("abc"));
        assert_eq!(0x1, Solution::individual_answers("aaaa"));
        assert_eq!(0x1081006, Solution::individual_answers("cbymt"));
    }

    #[test]
    fn test_count_group_answers() {
        assert_eq!(3, Solution::count_group_answers(&proper_vec(vec!["abc"])));
        assert_eq!(3, Solution::count_group_answers(&proper_vec(vec!["a", "b", "c"])));
        assert_eq!(3, Solution::count_group_answers(&proper_vec(vec!["ab", "ac"])));
        assert_eq!(1, Solution::count_group_answers(&proper_vec(vec!["a", "a", "a", "a"])));
        assert_eq!(1, Solution::count_group_answers(&proper_vec(vec!["b"])));
    }

    #[test]
    fn test_count_total_answers() {
        assert_eq!(
            11,
            Solution::count_total_answers(
                &vec![
                    proper_vec(vec!["abc"]),
                    proper_vec(vec!["a", "b", "c"]),
                    proper_vec(vec!["ab", "ac"]),
                    proper_vec(vec!["a", "a", "a", "a"]),
                    proper_vec(vec!["b"]),
                ],
            ),
        );
    }

    #[test]
    fn test_count_group_all_yes() {
        assert_eq!(3, Solution::count_group_all_yes(&proper_vec(vec!["abc"])));
        assert_eq!(0, Solution::count_group_all_yes(&proper_vec(vec!["a", "b", "c"])));
        assert_eq!(1, Solution::count_group_all_yes(&proper_vec(vec!["ab", "ac"])));
        assert_eq!(1, Solution::count_group_all_yes(&proper_vec(vec!["a", "a", "a", "a"])));
        assert_eq!(1, Solution::count_group_all_yes(&proper_vec(vec!["b"])));
    }

    #[test]
    fn test_count_total_all_yes() {
        assert_eq!(
            6,
            Solution::count_total_all_yes(
                &vec![
                    proper_vec(vec!["abc"]),
                    proper_vec(vec!["a", "b", "c"]),
                    proper_vec(vec!["ab", "ac"]),
                    proper_vec(vec!["a", "a", "a", "a"]),
                    proper_vec(vec!["b"]),
                ],
            ),
        );
    }
}
