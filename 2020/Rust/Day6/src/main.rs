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

// https://adventofcode.com/2020/day/6

#![allow(non_snake_case)]

use std::fs::File;
use std::io::Read;

fn individual_answers(answers: &str) -> u32 {
    answers.bytes().fold(0, |acc, c| {
        acc | 0x1 << (c - b'a')
    })
}

fn count_group_answers(group: &Vec<&str>) -> usize {
    group.iter().fold(0_u32, |acc, answers| {
        acc | individual_answers(answers)
    }).count_ones() as usize
}

fn count_total_answers(groups: &Vec<Vec<&str>>) -> usize {
    groups.iter().fold(0, |acc, group| {
        acc + count_group_answers(group)
    })
}

fn count_group_all_yes(group: &Vec<&str>) -> usize {
    group
        .iter()
        .fold([0; 26], |mut acc, person| {
            for c in person.bytes() {
                acc[(c - b'a') as usize] += 1;
            }
            acc
        })
        .iter()
        .filter(|a| **a == group.len())
        .count()
}

fn count_total_all_yes(groups: &Vec<Vec<&str>>) -> usize {
    groups.iter().fold(0, |acc, group| {
        acc + count_group_all_yes(group)
    })
}

fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?;

    file.read_to_string(&mut buffer).unwrap();
    let mut lines: Vec<&str> = buffer.trim().split("\n").collect();
    let groups: Vec<Vec<&str>> =
        lines
        .as_mut_slice()
        .split(|line| line.is_empty())
        .map(|x| x.to_vec())
        .collect();

    println!("Day 6 / Part 1: {}", count_total_answers(&groups));
    println!("Day 6 / Part 2: {}", count_total_all_yes(&groups));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_individual_answers() {
        assert_eq!(0x7, individual_answers("abc"));
        assert_eq!(0x1, individual_answers("aaaa"));
        assert_eq!(0x1081006, individual_answers("cbymt"));
    }

    #[test]
    fn test_count_group_answers() {
        assert_eq!(3, count_group_answers(&vec!["abc"]));
        assert_eq!(3, count_group_answers(&vec!["a", "b", "c"]));
        assert_eq!(3, count_group_answers(&vec!["ab", "ac"]));
        assert_eq!(1, count_group_answers(&vec!["a", "a", "a", "a"]));
        assert_eq!(1, count_group_answers(&vec!["b"]));
    }

    #[test]
    fn test_count_total_answers() {
        assert_eq!(
            11,
            count_total_answers(
                &vec![
                    vec!["abc"],
                    vec!["a", "b", "c"],
                    vec!["ab", "ac"],
                    vec!["a", "a", "a", "a"],
                    vec!["b"],
                ],
            ),
        );
    }

    #[test]
    fn test_count_group_all_yes() {
        assert_eq!(3, count_group_all_yes(&vec!["abc"]));
        assert_eq!(0, count_group_all_yes(&vec!["a", "b", "c"]));
        assert_eq!(1, count_group_all_yes(&vec!["ab", "ac"]));
        assert_eq!(1, count_group_all_yes(&vec!["a", "a", "a", "a"]));
        assert_eq!(1, count_group_all_yes(&vec!["b"]));
    }

    #[test]
    fn test_count_total_all_yes() {
        assert_eq!(
            6,
            count_total_all_yes(
                &vec![
                    vec!["abc"],
                    vec!["a", "b", "c"],
                    vec!["ab", "ac"],
                    vec!["a", "a", "a", "a"],
                    vec!["b"],
                ],
            ),
        );
    }
}
