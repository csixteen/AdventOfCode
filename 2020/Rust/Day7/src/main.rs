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

// https://adventofcode.com/2020/day/7

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;
use std::str::FromStr;

use lazy_static::lazy_static;
use regex::Regex;

fn build_graph(rules: &Vec<&str>) -> HashMap<String, HashSet<String>> {
    lazy_static! {
        static ref CONTAINS: Regex = Regex::new(r"(.*) bags? contain (.*)").unwrap();
        static ref CONTAINED: Regex = Regex::new(r"(\d+) (.*) bags?").unwrap();
    }

    let mut graph = HashMap::new();

    for r in rules.iter() {
        if let Some(g1) = CONTAINS.captures(r) {
            let _contains = g1.get(1).unwrap().as_str();
            let _contained: Vec<&str> = g1.get(2).unwrap().as_str().split(",").collect();

            for c in _contained.iter() {
                if let Some(g2) = CONTAINED.captures(c) {
                    graph.entry(String::from(g2.get(2).unwrap().as_str()))
                        .or_insert(HashSet::new())
                        .insert(String::from(_contains));
                }
            }
        }
    }

    graph
}

fn contain_color(graph: &HashMap<String, HashSet<String>>, color: &str, acc: &mut HashSet<String>) {
    match graph.get(color) {
        None => (),
        Some(contain) => {
            contain.iter().for_each(|c| {
                acc.insert(String::from(c));
                contain_color(graph, c, acc);
            });
        },
    };
}

fn total_containing_bags(rules: &Vec<&str>, color: &str) -> usize {
    let graph = build_graph(&rules);
    let mut acc = HashSet::new();
    contain_color(&graph, color, &mut acc);

    acc.len()
}

fn build_inverted_graph(rules: &Vec<&str>) -> HashMap<String, HashSet<(usize, String)>> {
    lazy_static! {
        static ref CONTAINS: Regex = Regex::new(r"(.*) bags? contain (.*)").unwrap();
        static ref CONTAINED: Regex = Regex::new(r"(\d+) (.*) bags?").unwrap();
    }

    let mut graph = HashMap::new();

    for r in rules.iter() {
        if let Some(g1) = CONTAINS.captures(r) {
            let _contains = g1.get(1).unwrap().as_str();
            let _contained: Vec<&str> = g1.get(2).unwrap().as_str().split(",").collect();

            for c in _contained.iter() {
                if let Some(g2) = CONTAINED.captures(c) {
                    graph.entry(String::from(_contains))
                        .or_insert(HashSet::new())
                        .insert((
                            usize::from_str(g2.get(1).unwrap().as_str()).unwrap(),
                            String::from(g2.get(2).unwrap().as_str())
                        ));
                }
            }
        }
    }

    graph
}

fn contained_colors(graph: &HashMap<String, HashSet<(usize,String)>>, color: &str) -> usize {
    match graph.get(color) {
        None => 0,
        Some(contained) => contained.iter().fold(0, |acc, (c, i)| {
            acc + c + c * contained_colors(graph, i)
        })
    }
}

fn total_contained_bags(rules: &Vec<&str>, color: &str) -> usize {
    let graph = build_inverted_graph(&rules);
    contained_colors(&graph, color)
}

fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?;

    file.read_to_string(&mut buffer).unwrap();
    let lines: Vec<&str> = buffer.trim().split("\n").collect();

    println!("Day 7 / part 1: {}", total_containing_bags(&lines, "shiny gold"));
    println!("Day 7 / part 2: {}", total_contained_bags(&lines, "shiny gold"));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const RULES: [&str; 9] = [
        "light red bags contain 1 bright white bag, 2 muted yellow bags.",
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        "bright white bags contain 1 shiny gold bag.",
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
        "faded blue bags contain no other bags.",
        "dotted black bags contain no other bags.",
    ];

    const RULES2: [&str; 7] = [
        "shiny gold bags contain 2 dark red bags.",
        "dark red bags contain 2 dark orange bags.",
        "dark orange bags contain 2 dark yellow bags.",
        "dark yellow bags contain 2 dark green bags.",
        "dark green bags contain 2 dark blue bags.",
        "dark blue bags contain 2 dark violet bags.",
        "dark violet bags contain no other bags.",
    ];

    #[test]
    fn test_build_graph() {
        let graph = build_graph(&RULES.to_vec());

        assert!(graph.contains_key("shiny gold"));
        assert!(graph.get("shiny gold").unwrap().contains("bright white"));
        assert!(graph.get("shiny gold").unwrap().contains("muted yellow"));
    }

    #[test]
    fn test_total_containing_bags() {
        assert_eq!(4, total_containing_bags(&RULES.to_vec(), "shiny gold"));
    }

    #[test]
    fn test_total_contained_bags() {
        assert_eq!(32, total_contained_bags(&RULES.to_vec(), "shiny gold"));
        assert_eq!(126, total_contained_bags(&RULES2.to_vec(), "shiny gold"));
    }
}
