#![allow(non_snake_case)]

use std::str::FromStr;

use aoc::Solver;
use lazy_static::lazy_static;
use regex::Regex;

pub struct Solution;

impl Solution {
    fn is_valid_field(field: &str, value: &str) -> bool {
        lazy_static! {
            static ref HGT: Regex = Regex::new(r"^(\d+)(in|cm)$").unwrap();
            static ref HCL: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
            static ref PID: Regex = Regex::new(r"^\d{9}$").unwrap();
        }

        match field {
            "byr" => {
                let v = i32::from_str(value).unwrap_or(0);
                v >= 1920 && v <= 2002
            },
            "iyr" => {
                let v = i32::from_str(value).unwrap_or(0);
                v >= 2010 && v <= 2020
            },
            "eyr" => {
                let v = i32::from_str(value).unwrap_or(0);
                v >= 2020 && v <= 2030
            },
            "hgt" => {
                match HGT.captures(value) {
                    None => false,
                    Some(c) => {
                        let height = i32::from_str(c.get(1).unwrap().as_str()).unwrap();
                        let metric = c.get(2).unwrap().as_str();
                        (metric == "cm" && (height >= 150 && height <= 193)) ||
                            (metric == "in" && (height >= 59 && height <= 76))
                    }
                }
            },
            "hcl" => HCL.is_match(value),
            "ecl" => ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&value),
            "pid" => PID.is_match(value),
            "cid" => true,
            _ => panic!("Invalid field"),
        }
    }

    fn field_to_bit(field: &str) -> u8 {
        match field {
            "byr" => 0x80,
            "iyr" => 0x40,
            "eyr" => 0x20,
            "hgt" => 0x10,
            "hcl" => 0x8,
            "ecl" => 0x4,
            "pid" => 0x2,
            "cid" => 0x1,
            _ => panic!("Invalid field"),
        }
    }

    fn get_fields(line: &str) -> u8 {
        line.split(" ").fold(0, |acc, part| {
            let mut it = part.split(":");
            let (field, value) = (it.next().unwrap(), it.next().unwrap());
            acc | (if Self::is_valid_field(field, value) { Self::field_to_bit(field) } else { 0 })
        })
    }

    fn count_valid_passports(content: &Vec<&str>) -> usize {
        let mut total = 0;
        let mut fields: u8 = 0;

        for line in content.iter() {
            if !line.is_empty() {
                fields |= Self::get_fields(line);
            } else {
                if fields >= 0xFE {
                    total += 1;
                }

                fields = 0;
            }
        }

        total + if fields >= 0xFE { 1 } else { 0 }
    }
}

impl Solver for Solution {
    fn part1(&self, _input: &Vec<&str>) -> String {
        "".to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        Self::count_valid_passports(input).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_valid_passports() {
        assert_eq!(
            0,
            Solution::count_valid_passports(&vec![
                    "eyr:2027",
                    "",
                    "byr:1981"
                ],),
        );
    }

    #[test]
    fn test_two_valid_passport() {
        assert_eq!(
            2,
            Solution::count_valid_passports(
                &vec![
                    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
                    "byr:1937 iyr:2017 cid:147 hgt:183cm",
                    "",
                    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
                    "hcl:#cfa07d byr:1929",
                    "",
                    "hcl:#ae17e1 iyr:2013",
                    "eyr:2024",
                    "ecl:brn pid:760753108 byr:1931",
                    "hgt:179cm",
                    "",
                    "hcl:#cfa07d eyr:2025 pid:166559648",
                    "iyr:2011 ecl:brn hgt:59in"
                ]
            ),
        );
    }

    #[test]
    fn test_valid_fields() {
        assert!(Solution::is_valid_field("byr", "2002"));
        assert!(!Solution::is_valid_field("byr", "2003"));
        assert!(Solution::is_valid_field("hgt", "60in"));
        assert!(Solution::is_valid_field("hgt", "190cm"));
        assert!(!Solution::is_valid_field("hgt", "190in"));
        assert!(!Solution::is_valid_field("hgt", "190"));
        assert!(Solution::is_valid_field("hcl", "#123abc"));
        assert!(!Solution::is_valid_field("hcl", "#123abz"));
        assert!(!Solution::is_valid_field("hcl", "123abc"));
        assert!(Solution::is_valid_field("ecl", "brn"));
        assert!(!Solution::is_valid_field("ecl", "wat"));
        assert!(Solution::is_valid_field("pid", "000000001"));
        assert!(!Solution::is_valid_field("pid", "0123456789"));
    }
}
