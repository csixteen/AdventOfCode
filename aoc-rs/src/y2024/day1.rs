use std::{collections::HashMap, hash::Hash};

use aoc::Solver;

pub struct Solution;

impl Solution {
    fn location_ids(input: &[&str]) -> (Vec<i32>, Vec<i32>) {
        input
            .iter()
            .fold((Vec::new(), Vec::new()), |(mut acc1, mut acc2), line| {
                let parts: Vec<&str> = line
                    .split(char::is_whitespace)
                    .filter(|s| !s.is_empty())
                    .collect();
                acc1.push(parts[0].parse::<i32>().expect("Could not parse i32"));
                acc2.push(parts[1].parse::<i32>().expect("Could not parse i32"));
                (acc1, acc2)
            })
    }
}

fn distance(a: &mut [i32], b: &mut [i32]) -> i32 {
    a.sort();
    b.sort();

    a.iter()
        .zip(b)
        .map(|(left, right)| (*left - *right).abs())
        .sum()
}

fn occur<T, U>(xs: T) -> HashMap<U, i32>
where
    U: Hash + Eq + PartialEq + PartialOrd,
    T: Iterator<Item = U>,
{
    xs.fold(HashMap::new(), |mut acc, x| {
        acc.entry(x).and_modify(|e| *e += 1).or_insert(1);
        acc
    })
}

fn similarity(a: &[i32], b: &[i32]) -> i32 {
    let occ = occur(b.iter());
    a.iter()
        .fold(0, |acc, i| acc + (i * (*occ.get(&i).unwrap_or(&0))))
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let (mut left, mut right) = Solution::location_ids(input);
        distance(&mut left, &mut right).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let (left, right) = Solution::location_ids(input);
        similarity(&left, &right).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_case1() {
        assert_eq!(
            11,
            distance(&mut [3, 4, 2, 1, 3, 3], &mut [4, 3, 5, 3, 9, 3])
        );
    }

    #[test]
    fn test_case2() {
        assert_eq!(31, similarity(&[3, 4, 2, 1, 3, 3], &[4, 3, 5, 3, 9, 3]));
    }
}
