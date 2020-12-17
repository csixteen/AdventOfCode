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

// https://adventofcode.com/2020/day/17

#![allow(non_snake_case)]

use std::collections::HashSet;
use itertools::iproduct;


const INPUT: [[char; 8]; 8] = [
    ['#','.','#','#','#','#','#','.'],
    ['#','.','.','#','#','.','.','.'],
    ['.','#','#','.','.','#','.','.'],
    ['#','.','#','#','.','#','#','#'],
    ['.','#','.','#','.','#','.','.'],
    ['#','.','#','#','.','.','#','.'],
    ['#','#','#','#','#','.','.','#'],
    ['.','.','#','.','#','.','#','#'],
];

#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
struct Coord(i32,i32,i32,i32);

fn neighbours(coord: &Coord) -> Vec<Coord> {
    let Coord(x,y,z,w) = coord;
    let mut n = Vec::new();

    for (dx, dy, dz, dw) in iproduct!(-1..=1, -1..=1, -1..=1, -1..=1) {
        if !(dx == 0 && dy == 0 && dz == 0 && dw == 0) {
            n.push(Coord(x+dx, y+dy, z+dz, w+dw));
        }
    }

    n
}

fn run_cycle(active: &HashSet<Coord>) -> HashSet<Coord> {
    let mut new_active = HashSet::new();
    let to_visit: HashSet<Coord> = active
        .iter()
        .flat_map(|coord| neighbours(coord))
        .collect();

    for coord in to_visit.iter() {
        let n: Vec<Coord> = neighbours(coord)
            .iter()
            .cloned()
            .filter(|c| active.contains(c))
            .collect();

        if n.len() == 3 || (active.contains(coord) && n.len() == 2) {
            new_active.insert(*coord);
        }
    }

    new_active
}

fn active_cubes(matrix: &Vec<Vec<char>>, cycles: usize) -> usize {
    let mut active: HashSet<Coord> = HashSet::new();

    for x in 0..matrix[0].len() {
        for y in 0..matrix.len() {
            if matrix[y][x] == '#' {
                active.insert(Coord(x as i32, y as i32, 0, 0));
            }
        }
    }

    (0..cycles).fold(active, |acc, _| run_cycle(&acc)).len()
}

fn main() {
    let initial_state = INPUT.to_vec().iter().map(|row| row.to_vec()).collect();

    println!("Day 17 / Part 2: {}", active_cubes(&initial_state, 6));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_active_cubes_no_cycles() {
        assert_eq!(
            5,
            active_cubes(
                &vec![
                    vec!['.', '#', '.'],
                    vec!['.', '.', '#'],
                    vec!['#', '#', '#'],
                ],
                0,
            ),
        );
    }

    #[test]
    fn test_active_cubes_6_cycles() {
        assert_eq!(
            848,
            active_cubes(
                &vec![
                    vec!['.', '#', '.'],
                    vec!['.', '.', '#'],
                    vec!['#', '#', '#'],
                ],
                6,
            ),
        );
    }
}
