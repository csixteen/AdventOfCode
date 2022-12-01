#![allow(non_snake_case)]

use aoc::Solver;

pub struct Solution;

impl Solution {
    fn count_trees(lines: &Vec<&str>, dx: usize, dy: usize) -> usize {
        let width = lines[0].len();
        let mut x = 0;
        let mut num_trees = 0_usize;

        for i in (0..lines.len()).step_by(dy) {
            if lines[i].chars().nth(x).unwrap() == '#' {
                num_trees += 1;
            }

            x = (x + dx) % width;
        }

        num_trees
    }

    fn count_trees_all_slopes(lines: &Vec<&str>) -> usize {
        [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
            .iter()
            .fold(1, |acc, (x, y)| {
                acc * Self::count_trees(lines, *x, *y)
            })
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        Self::count_trees(input, 3, 1).to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        Self::count_trees_all_slopes(input).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MAP: [&str; 11] = [
        "..##.......",
        "#...#...#..",
        ".#....#..#.",
        "..#.#...#.#",
        ".#...##..#.",
        "..#.##.....",
        ".#.#.#....#",
        ".#........#",
        "#.##...#...",
        "#...##....#",
        ".#..#...#.#",
    ];

    #[test]
    fn test_count_trees() {
        assert_eq!(
            7,
            Solution::count_trees(&MAP.to_vec(), 3, 1),
        );
    }

    #[test]
    fn test_count_trees_all_slopes() {
        assert_eq!(
            336,
            Solution::count_trees_all_slopes(&MAP.to_vec()),
        );
    }
}
