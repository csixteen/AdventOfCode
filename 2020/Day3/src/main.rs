// https://adventofcode.com/2020/day/3

use std::fs::File;
use std::io::Read;


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
    count_trees(lines, 1, 1) *
    count_trees(lines, 3, 1) *
    count_trees(lines, 5, 1) *
    count_trees(lines, 7, 1) *
    count_trees(lines, 1, 2)
}


fn main() {
    let mut buffer = String::new();
    let lines: Vec<&str>;

    match File::open("data/input.txt") {
        Err(_) => panic!("Error reading <input.txt>"),
        Ok(mut file) => {
            file.read_to_string(&mut buffer).unwrap();
            lines = buffer.trim().split("\n").collect();
        }
    }

    println!("Day 3 / Part 1: {}", count_trees(&lines, 3, 1));
    println!("Day 3 / Part 2: {}", count_trees_all_slopes(&lines));
}
