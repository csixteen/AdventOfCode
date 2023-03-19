use std::collections::HashMap;
use std::str::FromStr;

use itertools::Itertools;

use aoc::Solver;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Rule {
    Branch(usize),
    Leaf(char),
}

type Grammar = HashMap<Rule, Vec<Vec<Rule>>>;

fn match_choices(s: &str, i: usize, choices: &[Rule], g: &Grammar) -> Option<usize> {
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
    if i >= s.len() {
        None
    } else {
        match rule {
            Rule::Leaf(c) => {
                if c == s.chars().nth(i).unwrap() {
                    Some(i + 1)
                } else {
                    None
                }
            }
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
    lines.iter().fold(HashMap::new(), |mut acc, line| {
        let parts: Vec<&str> = line.split(':').collect();
        let branch = Rule::Branch(usize::from_str(parts[0]).unwrap());
        let choices = parts[1]
            .split('|')
            .map(|p| {
                p.trim()
                    .split(' ')
                    .map(|y| {
                        let x = y.trim_matches('"');
                        match usize::from_str(x) {
                            Ok(n) => Rule::Branch(n),
                            Err(_) => Rule::Leaf(x.chars().nth(0).unwrap()),
                        }
                    })
                    .collect::<Vec<Rule>>()
            })
            .collect::<Vec<Vec<Rule>>>();

        acc.insert(branch, choices);
        acc
    })
}

fn total_valid(lines: &Vec<String>, g: &Grammar) -> usize {
    lines.iter().filter(|&line| valid_string(line, g)).count()
}

pub struct Solution;

fn grammar_from_input(input: &[&str]) -> (Grammar, Vec<Vec<String>>) {
    let groups = input
        .iter()
        .map(|&s| s.to_owned())
        .group_by(|line| !line.is_empty());
    let grouped: Vec<Vec<String>> = groups
        .into_iter()
        .map(|(_, group)| group.collect::<Vec<String>>())
        .filter(|g| !g[0].is_empty())
        .collect();

    let grammar = build_grammar(&grouped[0]);

    (grammar, grouped)
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let (grammar, grouped) = grammar_from_input(input);
        total_valid(&grouped[1], &grammar).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let (grammar, grouped) = grammar_from_input(input);
        total_valid(&grouped[1], &grammar).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_rule() {
        let grammar = build_grammar(&vec![
            "0: 1 2".to_string(),
            "1: \"a\"".to_string(),
            "2: \"b\"".to_string(),
        ]);

        assert_eq!(Some(1), match_rule("ab", 0, Rule::Leaf('a'), &grammar));
        assert_eq!(None, match_rule("ab", 0, Rule::Leaf('b'), &grammar));
        assert_eq!(Some(2), match_rule("ab", 1, Rule::Leaf('b'), &grammar));
    }

    #[test]
    fn test_valid_string_basic() {
        let grammar = build_grammar(&vec![
            "0: 1 2".to_string(),
            "1: \"a\"".to_string(),
            "2: 1 3 | 3 1".to_string(),
            "3: \"b\"".to_string(),
        ]);

        assert!(valid_string("aab", &grammar));
        assert!(valid_string("aba", &grammar));
        assert!(!valid_string("bbb", &grammar));
    }

    #[test]
    fn test_valid_string_basic2() {
        let grammar = build_grammar(&vec![
            "0: 4 1 5".to_string(),
            "1: 2 3 | 3 2".to_string(),
            "2: 4 4 | 5 5".to_string(),
            "3: 4 5 | 5 4".to_string(),
            "4: \"a\"".to_string(),
            "5: \"b\"".to_string(),
        ]);

        assert!(valid_string("ababbb", &grammar));
        assert!(!valid_string("bababa", &grammar));
        assert!(valid_string("abbbab", &grammar));
        assert!(!valid_string("aaabbb", &grammar));
        assert!(!valid_string("aaaabbb", &grammar));
    }
}
