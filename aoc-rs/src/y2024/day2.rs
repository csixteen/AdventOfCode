use aoc::Solver;

pub struct Solution;

fn is_safe(level: &[i32]) -> bool {
    let increasing = level[1] > level[0];
    for window in level.windows(2) {
        if window[0] == window[1]
            || (window[1] > window[0]) != increasing
            || (window[1] - window[0]).abs() > 3
        {
            return false;
        }
    }

    true
}

fn try_into_safe(report: Vec<i32>) -> Vec<i32> {
    if is_safe(&report) {
        return report;
    }

    for skip in 0..report.len() {
        let candidate = report
            .iter()
            .enumerate()
            .filter(|(index, _)| *index != skip)
            .map(|(_, elem)| *elem)
            .collect::<Vec<_>>();

        if is_safe(&candidate) {
            return candidate;
        }
    }

    report
}

fn str_to_level(s: &str) -> Vec<i32> {
    s.split(char::is_whitespace)
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<i32>().expect("Could not parse i32"))
        .collect()
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        input
            .iter()
            .map(|s| is_safe(&str_to_level(s)))
            .filter(|x| *x)
            .count()
            .to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        input
            .iter()
            .map(|s| is_safe(&try_into_safe(str_to_level(s))))
            .filter(|x| *x)
            .count()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str_to_level() {
        assert_eq!(vec![1, 2, 33, 456], str_to_level("1 2 33 456"));
    }

    #[test]
    fn test_is_safe1() {
        assert!(is_safe(&[7, 6, 4, 2, 1]));
    }

    #[test]
    fn test_is_safe2() {
        assert!(!is_safe(&[1, 2, 7, 8, 9]));
    }

    #[test]
    fn test_is_safe3() {
        assert!(!is_safe(&[9, 7, 6, 2, 1]));
    }

    #[test]
    fn test_is_safe4() {
        assert!(!is_safe(&[1, 3, 2, 4, 5]));
    }

    #[test]
    fn test_is_safe5() {
        assert!(!is_safe(&[8, 6, 4, 4, 1]));
    }

    #[test]
    fn test_is_safe6() {
        assert!(is_safe(&[1, 3, 6, 7, 9]));
    }

    #[test]
    fn test_try_into_safe1() {
        assert_eq!(vec![7, 6, 4, 2, 1], try_into_safe(vec![7, 6, 4, 2, 1]));
    }

    #[test]
    fn test_try_into_safe2() {
        assert_eq!(vec![1, 2, 7, 8, 9], try_into_safe(vec![1, 2, 7, 8, 9]));
    }

    #[test]
    fn test_try_into_safe3() {
        assert_eq!(vec![9, 7, 6, 2, 1], try_into_safe(vec![9, 7, 6, 2, 1]));
    }

    #[test]
    fn test_try_into_safe4() {
        assert_eq!(vec![1, 2, 4, 5], try_into_safe(vec![1, 3, 2, 4, 5]));
    }

    #[test]
    fn test_try_into_safe5() {
        assert_eq!(vec![8, 6, 4, 1], try_into_safe(vec![8, 6, 4, 4, 1]));
    }

    #[test]
    fn test_try_into_safe6() {
        assert_eq!(vec![1, 3, 6, 7, 9], try_into_safe(vec![1, 3, 6, 7, 9]));
    }
}
