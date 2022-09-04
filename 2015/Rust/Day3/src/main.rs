#![allow(non_snake_case)]

use std::collections::{HashMap, HashSet};

use aoc::fs::get_file_contents;

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
        let next = next_pos(c, p);
        acc.entry(next).and_modify(|counter| *counter += 1).or_insert(1);
        (acc, next)
    }).0
}

fn count_houses(dir: &str) -> usize {
    let houses = deliver_presents(dir);
    houses.keys().count()
}

fn santa_and_robot(dir: &str) -> usize {
    let santa_steps: String = dir.chars().step_by(2).collect();
    let robot_steps: String = dir.chars().skip(1).step_by(2).collect();
    let santa_houses: HashSet<_> = deliver_presents(&santa_steps).keys().cloned().collect();
    let robot_houses: HashSet<_> = deliver_presents(&robot_steps).keys().cloned().collect();
    let houses = santa_houses.union(&robot_houses);
    houses.count()
}

fn main() -> std::io::Result<()> {
    let lines: Vec<String> = get_file_contents("data/input.txt")?;

    println!("Part1: {}", count_houses(&lines[0]));
    println!("Part2: {}", santa_and_robot(&lines[0]));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_houses_with_presents() {
        assert_eq!(2, count_houses(">"));
        assert_eq!(4, count_houses("^>v<"));
        assert_eq!(2, count_houses("^v^v^v^v^v"));
    }

    #[test]
    fn test_santa_and_robot() {
        assert_eq!(3, santa_and_robot("^v"));
        assert_eq!(3, santa_and_robot("^>v<"));
        assert_eq!(11, santa_and_robot("^v^v^v^v^v"));
    }
}
