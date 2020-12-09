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

// https://adventofcode.com/2020/day/2

#![allow(bare_trait_objects)]
#![allow(non_snake_case)]

use std::collections::HashMap;
use std::str::FromStr;

use aoc::fs::get_file_contents;
use lazy_static::lazy_static;
use regex::Regex;


type CheckerFn = Fn(usize,usize,char,String) -> bool;


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

fn count_valid_passwords_part1(passwords: &Vec<String>) -> usize {
    passwords
        .iter()
        .filter(|p| is_valid_password(p, &|_min, _max, letter, passwd: String| {
            let count = passwd.chars().filter(|c| *c == letter).count();
            count >= _min && count <= _max
        }))
        .count()
}

fn count_valid_passwords_part2(passwords: &Vec<String>) -> usize {
    passwords
        .iter()
        .filter(|p| is_valid_password(p, &|first, second, letter, passwd: String| {
            let indices = passwd
                .as_str()
                .char_indices()
                .fold(HashMap::new(), |mut acc, opt| {
                    let (i, c) = opt;
                    acc.insert(i+1, c);
                    acc
                });

            let a = *indices.get(&first).unwrap();
            let b = *indices.get(&second).unwrap();

            (a == letter || b == letter) && !(a == letter && b == letter)
        }))
        .count()
}

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;

    println!("Day 2 / Part 1: {}", count_valid_passwords_part1(&lines));
    println!("Day 2 / Part 2: {}", count_valid_passwords_part2(&lines));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const RULES: [&str; 3] = [
        "1-3 a: abcde",
        "1-3 b: cdefg",
        "2-9 c: ccccccccc",
    ];

    #[test]
    fn test_count_valid_passwords_part1() {
        assert_eq!(
            2,
            count_valid_passwords_part1(
                &RULES.to_vec().iter().map(|&s| String::from(s)).collect(),
            )
        );
    }

    #[test]
    fn test_count_valid_passwords_part2() {
        assert_eq!(
            1,
            count_valid_passwords_part2(
                &RULES.to_vec().iter().map(|&s| String::from(s)).collect(),
            )
        );
    }
}
