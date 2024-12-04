use aoc::Solver;
use lazy_static::lazy_static;
use regex::Regex;

pub struct Solution;

lazy_static! {
    static ref RE: Regex =
        Regex::new(r"mul\((?P<left>[0-9]{1,3}),(?P<right>[0-9]{1,3})\)").unwrap();
    static ref EN: Regex = Regex::new(r"don't\(\)(?:.*?do\(\)|.*$)").unwrap();
}

fn remove_disabled(s: &str) -> String {
    EN.replace_all(s, "").to_string()
}

fn line_to_pairs(line: &str) -> Vec<(i32, i32)> {
    RE.captures_iter(line)
        .map(|caps| {
            let left = caps
                .name("left")
                .unwrap()
                .as_str()
                .parse::<i32>()
                .expect("Couldn't parse i32");
            let right = caps
                .name("right")
                .unwrap()
                .as_str()
                .parse::<i32>()
                .expect("Couldn't parse i32");
            (left, right)
        })
        .collect()
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        input
            .iter()
            .map(|&line| {
                let pairs = line_to_pairs(line);
                pairs.into_iter().map(|(l, r)| (l * r) as i64).sum::<i64>()
            })
            .sum::<i64>()
            .to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let all = input.iter().fold(String::new(), |mut acc, &s| {
            acc.push_str(s);
            acc
        });

        line_to_pairs(&remove_disabled(&all))
            .into_iter()
            .map(|(l, r)| (l * r) as i64)
            .sum::<i64>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_to_pairs() {
        assert_eq!(
            vec![(2, 4), (5, 5), (11, 8), (8, 5)],
            line_to_pairs(
                "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))mul(44444,103)"
            )
        );
    }

    #[test]
    fn test_remove_disabled1() {
        assert_eq!("abcdef", &remove_disabled("abcdon't()123do()def"));
    }

    #[test]
    fn test_remove_disabled2() {
        assert_eq!(
            "abcdefghi",
            &remove_disabled("abcdon't()123do()defdon't()456do()ghi")
        );
    }

    #[test]
    fn test_remove_disabled3() {
        assert_eq!(
            "abcdo()defdo()ghi",
            &remove_disabled("abcdo()defdo()don't()123don't()456do()ghi")
        );
    }

    #[test]
    fn test_new_line_to_pairs() {
        assert_eq!(
            vec![(2, 4), (8, 5)],
            line_to_pairs(&remove_disabled(
                "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
            ))
        );
    }
}
