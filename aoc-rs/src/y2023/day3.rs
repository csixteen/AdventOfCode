use std::{collections::HashMap, hash::Hash};

use aoc::Solver;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct Symbol {
    pos: (usize, usize),
    s: char,
}

impl Symbol {
    fn new(pos: (usize, usize), s: char) -> Self {
        Self { pos, s }
    }
}

fn is_symbol(c: char) -> bool {
    c != '.' && !c.is_ascii_digit()
}

type Symbols = HashMap<Symbol, Vec<usize>>;

fn init_symbols(input: &[&str]) -> Symbols {
    let mut res = HashMap::new();

    for (r, row) in input.iter().enumerate() {
        for (c, s) in row.char_indices() {
            if is_symbol(s) {
                res.insert(Symbol::new((r, c), s), Vec::new());
            }
        }
    }

    res
}

fn extract_numbers_row(i: usize, row: &str, symbols: &mut Symbols) {}

fn extract_numbers(input: &[&str], symbols: &mut Symbols) {
    for (i, row) in input.iter().enumerate() {
        extract_numbers_row(i, row, symbols);
    }
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        "0".to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{collections::HashSet, hash::Hash};

    fn compare_unsorted<T>(a: &[T], b: &[T]) -> bool
    where
        T: Eq + Hash,
    {
        let a: HashSet<_> = a.iter().collect();
        let b: HashSet<_> = b.iter().collect();
        a == b
    }

    #[ignore = "Incomplete"]
    #[test]
    fn test_part1() {
        let s = Solution;
        assert_eq!(
            "4361".to_string(),
            s.part1(&[
                "467..114..",
                "...@......",
                "..35..633.",
                "....../...",
                "617*......",
                ".....=.58.",
                "..592.....",
                "......755.",
                "..._.*....",
                ".664.598.."
            ])
        );
    }
}
