use std::collections::HashMap;

use aoc::Solver;

fn init_sequence(seq: &[usize]) -> HashMap<usize, Vec<usize>> {
    (0..seq.len()).fold(HashMap::new(), |mut acc, i| {
        acc.entry(seq[i]).or_insert(Vec::new()).push(i + 1);
        acc
    })
}

fn nth_number_spoken(numbers: Vec<usize>, k: usize) -> usize {
    let mut seen: HashMap<usize, Vec<usize>> = init_sequence(&numbers);

    let len = numbers.len();
    let mut last_number = numbers[len - 1];
    let mut turn = len + 1;

    while turn <= k {
        match seen.get(&last_number) {
            None => last_number = 0,
            Some(o) => {
                let l = o.len();
                last_number = o[l - 1] - o[0.max(l as i32 - 2) as usize];
            }
        }

        seen.entry(last_number).or_insert(Vec::new()).push(turn);
        turn += 1;
    }

    last_number
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, _input: &[&str]) -> String {
        nth_number_spoken(vec![2, 0, 1, 9, 5, 19], 2020).to_string()
    }

    fn part2(&self, _input: &[&str]) -> String {
        nth_number_spoken(vec![2, 0, 1, 9, 5, 19], 30000000).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nth_number_spoken() {
        assert_eq!(436, nth_number_spoken(vec![0, 3, 6], 2020));
        assert_eq!(1, nth_number_spoken(vec![1, 3, 2], 2020));
        assert_eq!(10, nth_number_spoken(vec![2, 1, 3], 2020));
        assert_eq!(27, nth_number_spoken(vec![1, 2, 3], 2020));
        assert_eq!(78, nth_number_spoken(vec![2, 3, 1], 2020));
        assert_eq!(438, nth_number_spoken(vec![3, 2, 1], 2020));
        assert_eq!(1836, nth_number_spoken(vec![3, 1, 2], 2020));
    }
}
