// https://adventofcode.com/2020/day/2

#![allow(bare_trait_objects)]
#![allow(non_snake_case)]

use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::str::FromStr;

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

fn count_valid_passwords_part1(passwords: &Vec<&str>) -> usize {
    passwords
        .iter()
        .filter(|p| is_valid_password(p, &|_min, _max, letter, passwd: String| {
            let count = passwd.chars().filter(|c| *c == letter).count();
            count >= _min && count <= _max
        }))
        .count()
}

fn count_valid_passwords_part2(passwords: &Vec<&str>) -> usize {
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
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?;

    file.read_to_string(&mut buffer).unwrap();
    let lines: Vec<&str> = buffer.trim().split("\n").collect();

    println!("Day 2 / Part 1: {}", count_valid_passwords_part1(&lines));
    println!("Day 2 / Part 2: {}", count_valid_passwords_part2(&lines));

    Ok(())
}
