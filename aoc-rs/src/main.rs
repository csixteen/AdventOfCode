use std::io::{self, Read};

use aoc::Solver;

mod y2015;
mod y2020;
mod y2022;
mod y2023;
mod y2024;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let lines: Vec<&str> = input.lines().collect();
    let solver = y2024::day3::Solution;

    println!("Part1: {}", solver.part1(&lines));
    println!("Part2: {}", solver.part2(&lines));

    Ok(())
}
