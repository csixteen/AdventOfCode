use itertools::Itertools;
use std::str::FromStr;

use aoc::Solver;

type Command = (usize, usize, usize);

pub struct Solution;

impl Solution {
    fn parse_cmd(cmd: &str) -> Command {
        let parts: Vec<_> = cmd
            .split(" ")
            .map(|p| usize::from_str(p))
            .filter(|n| n.is_ok())
            .map(|n| n.unwrap())
            .collect();
        (parts[0], parts[1], parts[2])
    }

    fn move_from_to(n: usize, from: usize, to: usize, state: &mut Vec<Vec<char>>) {
        for _ in 0..n {
            let c = &state[from].pop().unwrap();
            state[to].push(*c);
        }
    }

    fn move_from_to_9001(n: usize, from: usize, to: usize, state: &mut Vec<Vec<char>>) {
        let to_move: Vec<char> = (0..n).fold(Vec::new(), |mut acc, _| {
            acc.push(state[from].pop().unwrap());
            acc
        });

        for c in to_move.iter().rev() {
            state[to].push(*c);
        }
    }

    fn initial_state() -> Vec<Vec<char>> {
        vec![
            vec!['W', 'B', 'D', 'N', 'C', 'F', 'J'],
            vec!['P', 'Z', 'V', 'Q', 'L', 'S', 'T'],
            vec!['P', 'Z', 'B', 'G', 'J', 'T'],
            vec!['D', 'T', 'L', 'J', 'Z', 'B', 'H', 'C'],
            vec!['G', 'V', 'B', 'J', 'S'],
            vec!['P', 'S', 'Q'],
            vec!['B', 'V', 'D', 'F', 'L', 'M', 'P', 'N'],
            vec!['P', 'S', 'M', 'F', 'B', 'D', 'L', 'R'],
            vec!['V', 'D', 'T', 'R'],
        ]
    }

    fn solve(
        input: &[&str],
        state: Vec<Vec<char>>,
        f: fn(usize, usize, usize, &mut Vec<Vec<char>>),
    ) -> String {
        let mut state = state;
        input
            .iter()
            .skip(10)
            .map(|&line| Solution::parse_cmd(line))
            .for_each(|cmd| {
                f(cmd.0, cmd.1 - 1, cmd.2 - 1, &mut state);
            });

        state
            .iter()
            .map(|stack| stack.iter().last())
            .filter(|&top| top.is_some())
            .map(|c| c.unwrap())
            .join("")
    }
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        Solution::solve(input, Solution::initial_state(), Solution::move_from_to)
    }

    fn part2(&self, input: &[&str]) -> String {
        Solution::solve(
            input,
            Solution::initial_state(),
            Solution::move_from_to_9001,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_cmd() {
        assert_eq!((1, 2, 3), Solution::parse_cmd("move 1 from 2 to 3"));
        assert_eq!((3, 3, 3), Solution::parse_cmd("move 3 from 3 to 3"));
    }

    #[test]
    fn test_move_from_to() {
        let mut state = vec![vec!['a', 'b', 'c'], vec!['d', 'e']];
        Solution::move_from_to(2, 0, 1, &mut state);
        assert_eq!(vec!['a'], state[0]);
        assert_eq!(vec!['d', 'e', 'c', 'b'], state[1]);
    }
}
