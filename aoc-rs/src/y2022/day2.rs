use aoc::Solver;
use std::str::FromStr;

#[derive(Clone, Eq, PartialEq)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl Shape {
    fn score(&self) -> usize {
        match &self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }

    fn wins_against(&self) -> Shape {
        match &self {
            Shape::Rock => Shape::Scissors,
            Shape::Paper => Shape::Rock,
            Shape::Scissors => Shape::Paper,
        }
    }

    fn loses_against(&self) -> Shape {
        match &self {
            Shape::Rock => Shape::Paper,
            Shape::Paper => Shape::Scissors,
            Shape::Scissors => Shape::Rock,
        }
    }
}

enum Outcome {
    Won,
    Draw,
    Lost,
}

impl Outcome {
    fn score(&self) -> usize {
        match &self {
            Outcome::Won => 6,
            Outcome::Draw => 3,
            Outcome::Lost => 0,
        }
    }
}

pub struct Solution;

impl Solution {
    fn play(me: &Shape, other: &Shape) -> Outcome {
        if *other == me.loses_against() {
            Outcome::Lost
        } else if *other == me.wins_against() {
            Outcome::Won
        } else {
            Outcome::Draw
        }
    }

    fn me_to_shape(c: char) -> Shape {
        match c {
            'X' => Shape::Rock,
            'Y' => Shape::Paper,
            'Z' => Shape::Scissors,
            _ => panic!("Invalid shape char"),
        }
    }

    fn outcome_to_shape(other: &Shape, outcome: char) -> Shape {
        match outcome {
            'Y' => other.clone(),
            'Z' => other.loses_against(),
            'X' => other.wins_against(),
            _ => panic!("Invalid outcome char"),
        }
    }

    fn other_to_shape(c: char) -> Shape {
        match c {
            'A' => Shape::Rock,
            'B' => Shape::Paper,
            'C' => Shape::Scissors,
            _ => panic!("Invalid shape char"),
        }
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        input
            .iter()
            .map(|&s| {
                let parts: Vec<char> = s.split(" ").map(|s| char::from_str(s).unwrap()).collect();
                let me = Solution::me_to_shape(parts[1]);
                let other = Solution::other_to_shape(parts[0]);
                let outcome = Solution::play(&me, &other);
                outcome.score() + me.score()
            })
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        input
            .iter()
            .map(|&s| {
                let parts: Vec<char> = s.split(" ").map(|s| char::from_str(s).unwrap()).collect();
                let other = Solution::other_to_shape(parts[0]);
                let me = Solution::outcome_to_shape(&other, parts[1]);
                let outcome = Solution::play(&me, &other);
                outcome.score() + me.score()
            })
            .sum::<usize>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1() {
        let solver = Solution;
        assert_eq!("15", solver.part1(&vec!["A Y", "B X", "C Z"]));
    }

    #[test]
    fn part2() {
        let solver = Solution;
        assert_eq!("12", solver.part2(&vec!["A Y", "B X", "C Z"]));
    }
}
