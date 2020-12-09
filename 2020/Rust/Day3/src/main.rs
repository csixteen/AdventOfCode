// MIT License
//
// Copyright (c) 2020 Pedro Rodrigues
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// https://adventofcode.com/2020/day/3

#![allow(non_snake_case)]

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


fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?; 

    file.read_to_string(&mut buffer).unwrap();
    let lines: Vec<&str> = buffer.trim().split("\n").collect();

    println!("Day 3 / Part 1: {}", count_trees(&lines, 3, 1));
    println!("Day 3 / Part 2: {}", count_trees_all_slopes(&lines));

    Ok(())
}
