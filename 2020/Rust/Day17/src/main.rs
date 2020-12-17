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
use std::hash::Hash;

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

trait Coordinate: Clone {
    type Item;

    fn neighbours(&self) -> Vec<Self::Item>;
    fn new(x: i32, y: i32) -> Self::Item;
}

#[derive(Copy,Eq,Hash,PartialEq)]
struct Coord3D(i32,i32,i32);

impl Clone for Coord3D {
    fn clone(&self) -> Self {
        let Coord3D(x,y,z) = &self;

        Coord3D(*x, *y, *z)
    }
}

impl Coordinate for Coord3D {
    type Item = Coord3D;

    fn neighbours(&self) -> Vec<Self::Item> {
        let Coord3D(x,y,z) = &self;

        iproduct!(-1..=1,-1..=1,-1..=1)
            .filter(|&(dx,dy,dz)| !(dx == 0 && dy == 0 && dz == 0))
            .fold(Vec::new(), |mut acc, (dx, dy, dz)| {
                acc.push(Coord3D(x+dx, y+dy, z+dz));
                acc
            })
    }

    fn new(x: i32, y: i32) -> Self::Item {
        Coord3D(x, y, 0)
    }
}

#[derive(Copy,Eq,Hash,PartialEq)]
struct Coord4D(i32,i32,i32,i32);

impl Clone for Coord4D {
    fn clone(&self) -> Self {
        let Coord4D(x,y,z,w) = &self;

        Coord4D(*x, *y, *z, *w)
    }
}

impl Coordinate for Coord4D {
    type Item = Coord4D;

    fn neighbours(&self) -> Vec<Self::Item> {
        let Coord4D(x,y,z,w) = &self;

        iproduct!(-1..=1,-1..=1,-1..=1,-1..=1)
            .filter(|&(dx,dy,dz,dw)| !(dx == 0 && dy == 0 && dz == 0 && dw == 0))
            .fold(Vec::new(), |mut acc, (dx, dy, dz, dw)| {
                acc.push(Coord4D(x+dx, y+dy, z+dz, w+dw));
                acc
            })
    }

    fn new(x: i32, y: i32) -> Self::Item {
        Coord4D(x, y, 0, 0)
    }
}

fn run_cycle<T>(active: &HashSet<T>) -> HashSet<T>
where
    T: Coordinate<Item = T> + Copy + Clone + Eq + Hash
{
    let mut new_active = HashSet::new();
    let to_visit: HashSet<T> = active
        .iter()
        .flat_map(|coord| coord.neighbours())
        .collect();

    for coord in to_visit.iter() {
        let n: Vec<T> = coord.neighbours()
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

fn init_active_cubes<T>(matrix: &Vec<Vec<char>>) -> HashSet<T>
where
    T: Coordinate<Item = T> + Eq + Hash 
{
    (0..matrix[0].len())
        .flat_map(|x| (0..matrix.len()).map(move |y| (x, y)))
        .filter(|(x, y)| matrix[*y][*x] == '#')
        .fold(HashSet::new(), |mut acc, (x, y)| {
            acc.insert(T::new(x as i32, y as i32));
            acc
        })
}

fn active_cubes<T>(matrix: &Vec<Vec<char>>, cycles: usize) -> usize
where
    T: Coordinate<Item = T> + Copy + Eq + Hash
{
    let active = init_active_cubes::<T>(matrix);

    (0..cycles).fold(active, |acc, _| run_cycle(&acc)).len()
}

fn main() {
    let initial_state: Vec<Vec<char>> = INPUT
        .to_vec()
        .iter()
        .map(|row| row.to_vec())
        .collect();

    println!("Day 17 / Part 1: {}", active_cubes::<Coord3D>(&initial_state, 6));
    println!("Day 17 / Part 2: {}", active_cubes::<Coord4D>(&initial_state, 6));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_active_cubes_no_cycles() {
        assert_eq!(
            5,
            active_cubes::<Coord3D>(
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
            active_cubes::<Coord4D>(
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
