mod y2015;
mod y2020;

use std::io;
use std::io::Read;

use aoc::Solver;

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let lines: Vec<&str> = input.lines().collect();
    let solver = y2015::day1::Solution;

    println!("Part1: {}", solver.part1(&lines));
    println!("Part2: {}", solver.part2(&lines));

    Ok(())
}
