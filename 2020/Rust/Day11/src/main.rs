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

// https://adventofcode.com/2020/day/11

#![allow(non_snake_case)]

use std::convert::TryFrom;

use aoc::fs::get_file_contents;


trait SeatSimulator {
    fn floor_count(&mut self) -> usize;
    fn change_pos(&mut self, i: usize, j: usize, c: char);
    fn matrix(&mut self) -> &Vec<Vec<char>>;
    fn tolerance(&mut self) -> usize;
    fn ahead(&mut self, row: usize, col: usize, drow: i32, dcol: i32) -> (usize, usize);

    fn occupied_neighbours(&mut self, row: i32, col: i32) -> usize {
        let (rows, cols) = (self.matrix().len(), self.matrix()[0].len());
        let mut neighbours = Vec::with_capacity(8);

        for dcol in -1..=1 {
            for drow in -1..=1 {
                if drow == 0 && dcol == 0 { continue; }

                match (usize::try_from(row + drow), usize::try_from(col + dcol)) {
                    (Ok(i), Ok(j)) if i < rows && j < cols => {
                        neighbours.push(self.ahead(i, j, drow, dcol));
                    },
                    _ => (),
                }
            }
        }

        neighbours.iter().filter(|(x, y)| self.matrix()[*x][*y] == '#').count()
    }

    fn must_occupy(&mut self, i: i32, j: i32) -> bool {
        self.occupied_neighbours(i, j) == 0
    }

    fn must_vacate(&mut self, i: i32, j: i32) -> bool {
        self.occupied_neighbours(i, j) >= self.tolerance()
    }

    fn single_round(&mut self, t: usize) -> usize {
        let (rows, cols) = (self.matrix().len(), self.matrix()[0].len());
        let mut changes: Vec<(usize,usize,char)> =
            Vec::with_capacity(rows*cols - self.floor_count());
        let mut total = t;

        for i in 0..rows {
            for j in 0..cols {
                match self.matrix()[i][j] {
                    '.' => (),
                    'L' => if self.must_occupy(i as i32, j as i32) {
                        changes.push((i,j,'#'));
                        total += 1;
                    },
                    '#' => if self.must_vacate(i as i32, j as i32) {
                        changes.push((i,j,'L'));
                        total -= 1;
                    },
                    _ => panic!("Unknown char"),
                }
            }
        }

        for (i, j, c) in changes.iter() {
            self.change_pos(*i, *j, *c);
        }

        total
    }

    fn occupied_seats(&mut self) -> usize {
        let mut previous = 0;

        loop {
            let curr = self.single_round(previous);
            if curr == previous {
                return curr;
            }
            previous = curr;
        }
    }
}

struct GridPart1 {
    m: Vec<Vec<char>>,
    fc: usize,
}

impl GridPart1 {
    fn new(m: Vec<Vec<char>>) -> Self {
        let fc = m.iter().fold(0, |acc, line| {
            acc + line.iter().filter(|&c| *c == '.').count()
        });

        GridPart1 {
            m: m,
            fc: fc,
        }
    }
}

impl SeatSimulator for GridPart1 {
    fn floor_count(&mut self) -> usize {
        self.fc
    }

    fn tolerance(&mut self) -> usize {
        4
    }

    fn change_pos(&mut self, i: usize, j: usize, c: char) {
        self.m[i][j] = c;
    }

    fn matrix(&mut self) -> &Vec<Vec<char>> {
        &self.m
    }

    fn ahead(&mut self, x: usize, y: usize, _dx: i32, _dy: i32) -> (usize, usize) {
        (x, y)
    }
}

struct GridPart2 {
    m: Vec<Vec<char>>,
    fc: usize,
}

impl GridPart2 {
    fn new(m: Vec<Vec<char>>) -> Self {
        let fc = m.iter().fold(0, |acc, line| {
            acc + line.iter().filter(|&c| *c == '.').count()
        });

        GridPart2 {
            m: m,
            fc: fc,
        }
    }
}

impl SeatSimulator for GridPart2 {
    fn floor_count(&mut self) -> usize {
        self.fc
    }

    fn tolerance(&mut self) -> usize {
        5
    }

    fn change_pos(&mut self, i: usize, j: usize, c: char) {
        self.m[i][j] = c;
    }

    fn matrix(&mut self) -> &Vec<Vec<char>> {
        &self.m
    }

    fn ahead(&mut self, row: usize, col: usize, drow: i32, dcol: i32) -> (usize, usize) {
        let (rows, cols) = (self.matrix().len(), self.matrix()[0].len());
        let mut i = row as i32;
        let mut j = col as i32;

        while i >= 0 &&
            (i as usize) < rows &&
            j >= 0 &&
            (j as usize) < cols &&
            self.matrix()[i as usize][j as usize] == '.' {
            match (usize::try_from(i+drow), usize::try_from(j+dcol)) {
                (Ok(i2), Ok(j2)) if i2 < rows && j2 < cols => {
                    i = i2 as i32;
                    j = j2 as i32;
                },
                _ => break,
            }
        }

        (i as usize, j as usize)
    }
}

fn build_matrix(lines: Vec<String>) -> Vec<Vec<char>> {
    lines.iter().map(|line| line.chars().collect()).collect()
}

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;
    let matrix = build_matrix(lines);

    let mut grid1 = GridPart1::new(matrix.clone());
    let mut grid2 = GridPart2::new(matrix.clone());

    println!("Day 11 / Part 1: {}", grid1.occupied_seats());
    println!("Day 11 / Part 2: {}", grid2.occupied_seats());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let matrix = build_matrix(
            vec![
                "L.LL.LL.LL".to_string(),
                "LLLLLLL.LL".to_string(),
                "L.L.L..L..".to_string(),
                "LLLL.LL.LL".to_string(),
                "L.LL.LL.LL".to_string(),
                "L.LLLLL.LL".to_string(),
                "..L.L.....".to_string(),
                "LLLLLLLLLL".to_string(),
                "L.LLLLLL.L".to_string(),
                "L.LLLLL.LL".to_string(),
            ],
        );

        let mut grid1 = GridPart1::new(matrix);

        assert_eq!(37, grid1.occupied_seats());
    }

    #[test]
    fn test_neighbours_part2() {
        let matrix = build_matrix(
            vec![
                ".......#.".to_string(),
                "...#.....".to_string(),
                ".#.......".to_string(),
                ".........".to_string(),
                "..#L....#".to_string(),
                "....#....".to_string(),
                ".........".to_string(),
                "#........".to_string(),
                "...#.....".to_string(),
            ],
        );

        let mut grid2 = GridPart2::new(matrix);
        assert_eq!(8, grid2.occupied_neighbours(4, 3));
    }

    #[test]
    fn test_neighbours_part2_empty_list() {
        let matrix = build_matrix(
            vec![
                ".##.##.".to_string(),
                "#.#.#.#".to_string(),
                "##...##".to_string(),
                "...L...".to_string(),
                "##...##".to_string(),
                "#.#.#.#".to_string(),
                ".##.##.".to_string(),
            ]
        );

        let mut grid2 = GridPart2::new(matrix);
        assert_eq!(0, grid2.occupied_neighbours(3, 3));
    }

    #[test]
    fn test_part2() {
        let matrix = build_matrix(
            vec![
                "L.LL.LL.LL".to_string(),
                "LLLLLLL.LL".to_string(),
                "L.L.L..L..".to_string(),
                "LLLL.LL.LL".to_string(),
                "L.LL.LL.LL".to_string(),
                "L.LLLLL.LL".to_string(),
                "..L.L.....".to_string(),
                "LLLLLLLLLL".to_string(),
                "L.LLLLLL.L".to_string(),
                "L.LLLLL.LL".to_string(),
            ],
        );

        let mut grid2 = GridPart2::new(matrix);

        assert_eq!(26, grid2.occupied_seats());
    }
}
