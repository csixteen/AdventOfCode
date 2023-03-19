#![allow(non_snake_case)]

use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

use lazy_static::lazy_static;
use regex::Regex;

use aoc::Solver;

fn build_allergens(foods: &Vec<Vec<Vec<String>>>) -> HashMap<String, HashSet<String>> {
    let mut res: HashMap<String, HashSet<String>> = HashMap::new();

    for food in foods.iter() {
        for allergen in food[0].iter() {
            if !res.contains_key(allergen) {
                res.insert(
                    allergen.to_string(),
                    HashSet::from_iter(food[1].iter().cloned()),
                );
            } else {
                let ii = res.get(allergen).unwrap();
                res.insert(
                    allergen.to_string(),
                    ii.intersection(&HashSet::from_iter(food[1].iter().cloned()))
                        .cloned()
                        .collect(),
                );
            }
        }
    }

    res
}

fn raw_data(lines: &[&str]) -> Vec<Vec<Vec<String>>> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"([\w\s]+) \(contains (.*)\)").unwrap();
    }

    let mut res = Vec::new();

    for &line in lines.iter() {
        match RE.captures(line) {
            None => panic!("Malformed entry: {}", line),
            Some(c) => {
                let ingredients: Vec<String> = c
                    .get(1)
                    .unwrap()
                    .as_str()
                    .split(' ')
                    .map(|w| w.trim().to_string())
                    .clone()
                    .collect();
                let allergens: Vec<String> = c
                    .get(2)
                    .unwrap()
                    .as_str()
                    .split(',')
                    .map(|w| w.trim().to_string())
                    .clone()
                    .collect();

                res.push(vec![allergens, ingredients]);
            }
        }
    }

    res
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let foods = raw_data(input);
        let mut ai = build_allergens(&foods);
        let allergens: Vec<String> = ai.keys().cloned().collect();

        loop {
            for a in allergens.iter() {
                let ii = ai.get(a).unwrap().len();
                if ii == 1 {
                    let to_remove = ai.get(a).unwrap().iter().cloned().last().unwrap();
                    for (allergen, ingredients) in ai.iter_mut() {
                        if allergen != a {
                            ingredients.remove(&to_remove);
                        }
                    }
                }
            }

            if ai.values().all(|ii| ii.len() == 1) {
                break;
            }
        }

        let mut occur: HashMap<&String, usize> = HashMap::new();

        for food in foods.iter() {
            for ingredient in food[1].iter() {
                if let None = ai.values().find(|i| i.iter().last().unwrap() == ingredient) {
                    *occur.entry(ingredient).or_insert(0) += 1;
                }
            }
        }

        println!("Day 21 / Part 1: {}", occur.values().sum::<usize>());

        let mut canonical = ai.iter().fold(Vec::new(), |mut acc, (k, v)| {
            acc.push((k, v.iter().last().unwrap()));
            acc
        });
        let _ = &canonical.sort_unstable();
        let canonical_form: Vec<String> = canonical.iter().map(|(_, v)| v.to_string()).collect();

        println!("Day 21 / Part 2: {}", &canonical_form.join(","));

        String::new()
    }

    fn part2(&self, _input: &[&str]) -> String {
        todo!()
    }
}
