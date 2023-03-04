#![allow(bare_trait_objects)]
#![allow(non_snake_case)]

use std::collections::HashMap;
use std::str::FromStr;

use aoc::Solver;
use lazy_static::lazy_static;
use regex::Regex;

type CheckerFn = dyn Fn(usize, usize, char, String) -> bool;

pub struct Solution;

impl Solution {
    fn is_valid_password(password: &str, checker: &CheckerFn) -> bool {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^(\d+)-(\d+) (\w{1}): (\w+)").unwrap();
        }

        match RE.captures(password) {
            None => panic!("Malformed entry: {}", password),
            Some(c) => {
                let _min = usize::from_str(c.get(1).unwrap().as_str()).unwrap();
                let _max = usize::from_str(c.get(2).unwrap().as_str()).unwrap();
                let letter = char::from_str(c.get(3).unwrap().as_str()).unwrap();
                let passwd = c.get(4).unwrap().as_str();

                checker(_min, _max, letter, passwd.to_string())
            }
        }
    }

    fn count_valid_passwords_part1(passwords: &[&str]) -> usize {
        passwords
            .iter()
            .filter(|p| {
                Self::is_valid_password(p, &|_min, _max, letter, passwd: String| {
                    let count = passwd.chars().filter(|c| *c == letter).count();
                    count >= _min && count <= _max
                })
            })
            .count()
    }

    fn count_valid_passwords_part2(passwords: &[&str]) -> usize {
        passwords
            .iter()
            .filter(|p| {
                Self::is_valid_password(p, &|first, second, letter, passwd: String| {
                    let indices =
                        passwd
                            .as_str()
                            .char_indices()
                            .fold(HashMap::new(), |mut acc, opt| {
                                let (i, c) = opt;
                                acc.insert(i + 1, c);
                                acc
                            });

                    let a = *indices.get(&first).unwrap();
                    let b = *indices.get(&second).unwrap();

                    (a == letter || b == letter) && !(a == letter && b == letter)
                })
            })
            .count()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        Self::count_valid_passwords_part1(input).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        Self::count_valid_passwords_part2(input).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const RULES: [&str; 3] = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"];

    #[test]
    fn test_count_valid_passwords_part1() {
        assert_eq!(2, Solution::count_valid_passwords_part1(&RULES.to_vec()));
    }

    #[test]
    fn test_count_valid_passwords_part2() {
        assert_eq!(1, Solution::count_valid_passwords_part2(&RULES.to_vec()));
    }
}
