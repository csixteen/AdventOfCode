use aoc::Solver;

pub struct Solution;

const SPELLED: [(&str, u32); 9] = [
    ("one", 1),
    ("two", 2),
    ("six", 6),
    ("four", 4),
    ("five", 5),
    ("nine", 9),
    ("three", 3),
    ("seven", 7),
    ("eight", 8),
];

fn find_elem(s: impl Iterator<Item = char>, rev: bool) -> u32 {
    let mut peek = s.peekable();

    if let Some(c) = peek.peek() {
        if c.is_ascii_digit() {
            return c.to_digit(10).unwrap();
        }
    }

    let ss: String = peek.collect();
    for (spelled, value) in SPELLED.iter() {
        let sss: String = if rev {
            spelled.chars().rev().collect()
        } else {
            spelled.chars().collect()
        };
        if ss.starts_with(&sss) {
            return *value;
        }
    }

    find_elem(ss.chars().skip(1), rev)
}

fn find_first(s: impl Iterator<Item = char>) -> u32 {
    find_elem(s, false)
}

fn find_last(s: impl Iterator<Item = char>) -> u32 {
    find_elem(s, true)
}

fn number_from_parts_2(s: &str) -> u32 {
    let a = find_first(s.chars());
    let b = find_last(s.chars().rev());
    let s = format!("{}{}", a, b);
    s.parse::<u32>().unwrap()
}

fn number_from_parts(s: &str) -> u32 {
    let a = s
        .chars()
        .find(|c| c.is_ascii_digit())
        .and_then(|c| c.to_digit(10))
        .unwrap();
    let b = s
        .chars()
        .rev()
        .find(|c| c.is_ascii_digit())
        .and_then(|c| c.to_digit(10))
        .unwrap();
    let s = format!("{}{}", a, b);
    s.parse::<u32>().unwrap()
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        input
            .iter()
            .map(|&s| number_from_parts(s))
            .sum::<u32>()
            .to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        input
            .iter()
            .map(|&s| number_from_parts_2(s))
            .sum::<u32>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let s = Solution;
        assert_eq!(
            "142".to_string(),
            s.part1(&["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"])
        );
    }

    #[test]
    fn test_find_first() {
        assert_eq!(2, find_first("two1nine".chars()));
        assert_eq!(8, find_first("eightwothree".chars()));
        assert_eq!(1, find_first("abcone2threexyz".chars()));
        assert_eq!(2, find_first("xtwone3four".chars()));
        assert_eq!(4, find_first("4nineeightseven2".chars()));
        assert_eq!(1, find_first("zoneight234".chars()));
        assert_eq!(7, find_first("7pqrstsixteen".chars()));
    }

    #[test]
    fn test_find_last() {
        assert_eq!(9, find_last("two1nine".chars().rev()));
        assert_eq!(3, find_last("eightwothree".chars().rev()));
        assert_eq!(3, find_last("abcone2threexyz".chars().rev()));
        assert_eq!(4, find_last("xtwone3four".chars().rev()));
        assert_eq!(2, find_last("4nineeightseven2".chars().rev()));
        assert_eq!(4, find_last("zoneight234".chars().rev()));
        assert_eq!(6, find_last("7pqrstsixteen".chars().rev()));
    }

    #[test]
    fn test_part2() {
        let s = Solution;
        assert_eq!(
            "281".to_string(),
            s.part2(&[
                "two1nine",
                "eightwothree",
                "abcone2threexyz",
                "xtwone3four",
                "4nineeightseven2",
                "zoneight234",
                "7pqrstsixteen"
            ])
        );
    }
}
