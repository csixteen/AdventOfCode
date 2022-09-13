#![allow(non_snake_case)]

use aoc::Solver;

pub struct Solution;

impl Solution {
    fn binary_search(seat: &str, lo: usize, hi: usize, lo_char: char, hi_char: char) -> usize {
        let (mut lo, mut hi) = (lo, hi);

        for c in seat.chars() {
            let mid = lo + (hi - lo) / 2;
            if c == hi_char {
                hi = mid;
            } else if c == lo_char {
                lo = mid;
            }
        }

        hi
    }

    fn seat_row(seat: &str) -> usize {
        Self::binary_search(seat, 0, 127, 'B', 'F')
    }

    fn seat_col(seat: &str) -> usize {
        Self::binary_search(seat, 0, 7, 'R', 'L')
    }

    fn seat_id(seat: &str) -> usize {
        Self::seat_row(seat) * 8 + Self::seat_col(seat)
    }

    fn get_max_id(seats: &Vec<&str>) -> usize {
        seats.iter().fold(0, |acc, seat| {
            let id = Self::seat_id(seat);
            acc.max(id)
        })
    }

    fn find_missing_seat(seats: &Vec<&str>) -> usize {
        let min_id = seats.iter().map(|seat| Self::seat_id(seat)).min().unwrap();
        let max_id = Self::get_max_id(seats);

        let mut all_seats = vec![false; 1024];
        for seat in seats.iter() {
            all_seats[Self::seat_id(seat)] = true;
        }

        (min_id..max_id).find(|id| !all_seats[*id]).unwrap()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        Self::get_max_id(input).to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        Self::find_missing_seat(input).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_seat_row() {
        assert_eq!(70, Solution::seat_row("BFFFBBFRRR"));
        assert_eq!(14, Solution::seat_row("FFFBBBFRRR"));
        assert_eq!(102, Solution::seat_row("BBFFBBFRLL"));
    }

    #[test]
    fn test_seat_col() {
        assert_eq!(7, Solution::seat_col("BFFFBBFRRR"));
        assert_eq!(7, Solution::seat_col("FFFBBBFRRR"));
        assert_eq!(4, Solution::seat_col("BBFFBBFRLL"));
    }

    #[test]
    fn test_seat_id() {
        assert_eq!(567, Solution::seat_id("BFFFBBFRRR"));
        assert_eq!(119, Solution::seat_id("FFFBBBFRRR"));
        assert_eq!(820, Solution::seat_id("BBFFBBFRLL"));
    }
}
