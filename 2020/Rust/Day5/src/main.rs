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

// https://adventofcode.com/2020/day/5

#![allow(non_snake_case)]

use std::fs::File;
use std::io::Read;

fn seat_row(seat: &str) -> usize {
    let (mut lo, mut hi) = (0, 127);

    for c in seat.chars() {
        let mid = lo + (hi - lo) / 2;
        if c == 'F' {
            hi = mid;
        } else if c == 'B' {
            lo = mid;
        }
    }

    hi
}

fn seat_col(seat: &str) -> usize {
    let (mut lo, mut hi) = (0, 7);

    for c in seat.chars() {
        let mid = lo + (hi - lo) / 2;
        if c == 'L' {
            hi = mid;
        } else if c == 'R' {
            lo = mid;
        }
    }

    hi
}

fn seat_id(seat: &str) -> usize {
    seat_row(seat) * 8 + seat_col(seat)
}

fn get_max_id(seats: &Vec<&str>) -> usize {
    seats.iter().fold(0, |acc, seat| {
        let id = seat_id(seat);
        acc.max(id)
    })
}

fn find_missing_seat(seats: &Vec<&str>) -> usize {
    let min_id = seats.iter().map(|seat| seat_id(seat)).min().unwrap();
    let max_id = get_max_id(seats);

    let mut all_seats = vec![false; 1024];
    for seat in seats.iter() {
        all_seats[seat_id(seat)] = true;
    }

    (min_id..max_id).find(|id| !all_seats[*id]).unwrap()
}

fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?;

    file.read_to_string(&mut buffer).unwrap();
    let lines: Vec<&str> = buffer.trim().split("\n").collect();

    println!("Day 5 / Part 1: {}", get_max_id(&lines));
    println!("Day 5 / Part 2: {}", find_missing_seat(&lines));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_seat_row() {
        assert_eq!(70, seat_row("BFFFBBFRRR"));
        assert_eq!(14, seat_row("FFFBBBFRRR"));
        assert_eq!(102, seat_row("BBFFBBFRLL"));
    }

    #[test]
    fn test_seat_col() {
        assert_eq!(7, seat_col("BFFFBBFRRR"));
        assert_eq!(7, seat_col("FFFBBBFRRR"));
        assert_eq!(4, seat_col("BBFFBBFRLL"));
    }

    #[test]
    fn test_seat_id() {
        assert_eq!(567, seat_id("BFFFBBFRRR"));
        assert_eq!(119, seat_id("FFFBBBFRRR"));
        assert_eq!(820, seat_id("BBFFBBFRLL"));
    }
}
