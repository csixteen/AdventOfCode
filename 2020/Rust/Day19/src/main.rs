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

// https://adventofcode.com/2020/day/19

#![allow(non_snake_case)]

use std::collections::HashMap;
use std::str::FromStr;

use aoc::fs::get_file_contents;
use itertools::Itertools;


#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
enum Rule {
    Branch(usize),
    Leaf(char),
}

type Grammar = HashMap<Rule, Vec<Vec<Rule>>>;

fn match_choices(s: &str, i: usize, choices: &Vec<Rule>, g: &Grammar) -> Option<usize> {
    let mut j = i;

    for rule in choices.iter() {
        match match_rule(s, j, *rule, g) {
            Some(x) => j = x,
            None => return None,
        }
    }

    Some(j)
}

fn match_branches(s: &str, i: usize, rules: &Vec<Vec<Rule>>, g: &Grammar) -> Option<usize> {
    rules
        .iter()
        .map(|choices| match_choices(s, i, choices, g))
        .filter(|res| res.is_some())
        .map(|res| res.unwrap())
        .min()
}

fn match_rule(s: &str, i: usize, rule: Rule, g: &Grammar) -> Option<usize> {
    if i >= s.len() { None }
    else {
        match rule {
            Rule::Leaf(c) => if c == s.chars().nth(i).unwrap() {
                Some(i+1)
            } else {
                None
            },
            _ => match_branches(s, i, g.get(&rule).unwrap(), g),
        }
    }
}

fn valid_string(s: &str, g: &Grammar) -> bool {
    match match_rule(s, 0, Rule::Branch(0), g) {
        Some(i) => i == s.len(),
        None => false,
    }
}

fn build_grammar(lines: &Vec<String>) -> Grammar {
    lines
        .iter()
        .fold(HashMap::new(), |mut acc, line| {
            let parts: Vec<&str> = line.split(':').collect();
            let branch = Rule::Branch(usize::from_str(parts[0]).unwrap());
            let choices = parts[1]
                .split('|')
                .map(|p| p
                     .trim()
                     .split(' ')
                     .map(|y| {
                         let x = y.trim_matches('"');
                         match usize::from_str(x) {
                            Ok(n) => Rule::Branch(n),
                            Err(_) => Rule::Leaf(x.chars().nth(0).unwrap()),
                         }
                     })
                     .collect::<Vec<Rule>>()
                )
                .collect::<Vec<Vec<Rule>>>();

            acc.insert(branch, choices);
            acc
        })
}

fn total_valid(lines: &Vec<String>, g: &Grammar) -> usize {
    lines
        .iter()
        .filter(|line| valid_string(line, g))
        .count()
}

fn main() -> std::io::Result<()> {
    //========================================================
    //                     Part 1
    //
    let lines = get_file_contents("data/input.txt")?;
    let groups = &lines.into_iter().group_by(|line| !line.is_empty());
    let grouped: Vec<Vec<String>> = groups
        .into_iter()
        .map(|(_, group)| group.collect::<Vec<String>>())
        .filter(|g| !g[0].is_empty())
        .collect();

    let grammar = build_grammar(&grouped[0]);

    println!("Day 19 / Part 1: {}", total_valid(&grouped[1], &grammar));

    //==========================================================
    //                     Part 2
    //
    let lines = get_file_contents("data/input2.txt")?;
    let groups = &lines.into_iter().group_by(|line| !line.is_empty());
    let grouped: Vec<Vec<String>> = groups
        .into_iter()
        .map(|(_, group)| group.collect::<Vec<String>>())
        .filter(|g| !g[0].is_empty())
        .collect();

    let grammar = build_grammar(&grouped[0]);

    println!("Day 19 / Part 2: {}", total_valid(&grouped[1], &grammar));
    

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_rule() {
        let grammar = build_grammar(
            &vec![
                "0: 1 2".to_string(),
                "1: \"a\"".to_string(),
                "2: \"b\"".to_string(),
            ],
        );

        assert_eq!(Some(1), match_rule("ab", 0, Rule::Leaf('a'), &grammar));
        assert_eq!(None, match_rule("ab", 0, Rule::Leaf('b'), &grammar));
        assert_eq!(Some(2), match_rule("ab", 1, Rule::Leaf('b'), &grammar));
    }

    #[test]
    fn test_valid_string_basic() {
        let grammar = build_grammar(
            &vec![
                "0: 1 2".to_string(),
                "1: \"a\"".to_string(),
                "2: 1 3 | 3 1".to_string(),
                "3: \"b\"".to_string(),
            ],
        );

        assert!(valid_string("aab", &grammar));
        assert!(valid_string("aba", &grammar));
        assert!(!valid_string("bbb", &grammar));
    }

    #[test]
    fn test_valid_string_basic2() {
        let grammar = build_grammar(
            &vec![
                "0: 4 1 5".to_string(),
                "1: 2 3 | 3 2".to_string(),
                "2: 4 4 | 5 5".to_string(),
                "3: 4 5 | 5 4".to_string(),
                "4: \"a\"".to_string(),
                "5: \"b\"".to_string(),
            ],
        );

        assert!(valid_string("ababbb", &grammar));
        assert!(!valid_string("bababa", &grammar));
        assert!(valid_string("abbbab", &grammar));
        assert!(!valid_string("aaabbb", &grammar));
        assert!(!valid_string("aaaabbb", &grammar));
    }

    #[test]
    fn test_valid_string_advanced() {
        let grammar = build_grammar(
            &vec![
                "0: 1".to_string(),
                "1: 2 | 2 1".to_string(),
                "2: 3 | 4".to_string(),
                "3: \"a\"".to_string(),
                "4: \"b\"".to_string(),
            ],
        );

        assert!(valid_string("a", &grammar));
        assert!(valid_string("aa", &grammar));
        assert!(valid_string("ab", &grammar));
        assert!(valid_string("abab", &grammar));
    }

    #[test]
    fn test_valid_string_advanced2() {
        let grammar = build_grammar(
            &vec![
                "42: 9 14 | 10 1".to_string(),
                "9: 14 27 | 1 26".to_string(),
                "10: 23 14 | 28 1".to_string(),
                "1: \"a\"".to_string(),
                "11: 42 31 | 42 11 31".to_string(),
                "5: 1 14 | 15 1".to_string(),
                "19: 14 1 | 14 14".to_string(),
                "12: 24 14 | 19 1".to_string(),
                "16: 15 1 | 14 14".to_string(),
                "31: 14 17 | 1 13".to_string(),
                "6: 14 14 | 1 14".to_string(),
                "2: 1 24 | 14 4".to_string(),
                "0: 8 11".to_string(),
                "13: 14 3 | 1 12".to_string(),
                "15: 1 | 14".to_string(),
                "17: 14 2 | 1 7".to_string(),
                "23: 25 1 | 22 14".to_string(),
                "28: 16 1".to_string(),
                "4: 1 1".to_string(),
                "20: 14 14 | 1 15".to_string(),
                "3: 5 14 | 16 1".to_string(),
                "27: 1 6 | 14 18".to_string(),
                "14: \"b\"".to_string(),
                "21: 14 1 | 1 14".to_string(),
                "25: 1 1 | 1 14".to_string(),
                "22: 14 14".to_string(),
                "8: 42 | 42 8".to_string(),
                "26: 14 22 | 1 20".to_string(),
                "18: 15 15".to_string(),
                "7: 14 5 | 1 21".to_string(),
                "24: 14 1".to_string(),
            ],
        );

        assert!(valid_string("bbabbbbaabaabba", &grammar));
        //assert!(valid_string("babbbbaabbbbbabbbbbbaabaaabaaa", &grammar));
        //assert!(valid_string("aaabbbbbbaaaabaababaabababbabaaabbababababaaa", &grammar));
        //assert!(valid_string("bbbbbbbaaaabbbbaaabbabaaa", &grammar));
        //assert!(valid_string("bbbababbbbaaaaaaaabbababaaababaabab", &grammar));
        //assert!(valid_string("ababaaaaaabaaab", &grammar));
        //assert!(valid_string("ababaaaaabbbaba", &grammar));
        //assert!(valid_string("baabbaaaabbaaaababbaababb", &grammar));
        //assert!(valid_string("abbbbabbbbaaaababbbbbbaaaababb", &grammar));
        //assert!(valid_string("aaaaabbaabaaaaababaa", &grammar));
        //assert!(valid_string("aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", &grammar));
        //assert!(valid_string("aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba", &grammar));
    }
}
