#![allow(non_snake_case)]

use std::collections::{HashMap, HashSet};

use aoc::Solver;

pub struct Solution;

impl Solution {
    fn next_pos(d: char, pos: (i32, i32)) -> (i32, i32) {
        match (d, pos) {
            ('>', (x, y)) => (x+1, y),
            ('<', (x, y)) => (x-1, y),
            ('^', (x, y)) => (x, y+1),
            ('v', (x, y)) => (x, y-1),
            (_, _)        => panic!("Invalid character"),
        }
    }

    fn deliver_presents(dir: &str) -> HashMap<(i32,i32), i32> {
        let mut grid = HashMap::new();
        grid.insert((0, 0), 1);

        dir.chars().fold((grid, (0, 0)), |(mut acc, p), c| {
            let next = Self::next_pos(c, p);
            acc.entry(next).and_modify(|counter| *counter += 1).or_insert(1);
            (acc, next)
        }).0
    }

    fn count_houses(dir: &str) -> usize {
        let houses = Self::deliver_presents(dir);
        houses.keys().count()
    }

    fn santa_and_robot(dir: &str) -> usize {
        let santa_steps: String = dir.chars().step_by(2).collect();
        let robot_steps: String = dir.chars().skip(1).step_by(2).collect();
        let santa_houses: HashSet<_> = Self::deliver_presents(&santa_steps).keys().cloned().collect();
        let robot_houses: HashSet<_> = Self::deliver_presents(&robot_steps).keys().cloned().collect();
        let houses = santa_houses.union(&robot_houses);
        houses.count()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        Solution::count_houses(&input[0]).to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        Solution::santa_and_robot(&input[0]).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_houses_with_presents() {
        assert_eq!(2, Solution::count_houses(">"));
        assert_eq!(4, Solution::count_houses("^>v<"));
        assert_eq!(2, Solution::count_houses("^v^v^v^v^v"));
    }

    #[test]
    fn test_santa_and_robot() {
        assert_eq!(3, Solution::santa_and_robot("^v"));
        assert_eq!(3, Solution::santa_and_robot("^>v<"));
        assert_eq!(11, Solution::santa_and_robot("^v^v^v^v^v"));
    }
}
