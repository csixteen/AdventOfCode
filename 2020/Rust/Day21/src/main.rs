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

// https://adventofcode.com/2020/day/21

#![allow(non_snake_case)]
#![allow(mutable_borrow_reservation_conflict)]

use std::collections::{HashMap,HashSet};
use std::iter::FromIterator;

use aoc::fs::get_file_contents;
use lazy_static::lazy_static;
use regex::Regex;


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
                    ii.intersection(
                        &HashSet::from_iter(food[1].iter().cloned())
                    ).cloned().collect());
            }
        }
    }

    res
}

fn raw_data(lines: &Vec<String>) -> Vec<Vec<Vec<String>>> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"([\w\s]+) \(contains (.*)\)").unwrap();
    }

    let mut res = Vec::new();

    for line in lines.iter() {
        match RE.captures(line) {
            None => panic!("Malformed entry: {}", line),
            Some(c) => {
                let ingredients: Vec<String> =
                    c.get(1).unwrap().as_str()
                        .split(' ')
                        .map(|w| w.trim().to_string())
                        .clone()
                        .collect();
                let allergens: Vec<String> =
                    c.get(2).unwrap().as_str()
                        .split(',')
                        .map(|w| w.trim().to_string())
                        .clone()
                        .collect();

                res.push(vec![allergens, ingredients]);
            },
        }
    }

    res
}

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;

    /*
     * This creates a Vec with the following structure:
     * vec![
     *     vec![vec![allergen1, ...], vec![ingredient1, ...]],
     *     ...
     * ]
     * This will be used for the count later on.
     */
    let foods = raw_data(&lines);

    // Builds a HashMap that maps each allergen to a HashSet of ingredients
    // that might have it.
    let mut ai = build_allergens(&foods);
    let allergens: Vec<String> = ai.keys().cloned().collect();

    /*
     * As a result of building `allergens`, there is at least one allergen
     * whose HashSet of potential matched ingredients only has one element.
     * Such allergen will be our starting point.
     */
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

        if ai.values().all(|ii| ii.len() == 1) { break ; }
    }

    // Builds a HashMap that maps each ingredient that doesn't contain an 
    // allergen to the number of times it occurs.
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
    &canonical.sort_unstable();
    let canonical_form: Vec<String> = canonical.iter().map(|(_, v)| v.to_string()).collect();

    println!("Day 21 / Part 2: {}", &canonical_form.join(","));

    Ok(())
}
