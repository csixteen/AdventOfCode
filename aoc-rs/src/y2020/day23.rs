use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::iter::FromIterator;

use aoc::Solver;

const INPUT: [i32; 9] = [2, 4, 7, 8, 1, 9, 3, 5, 6];

fn pick_three(cups: &mut VecDeque<i32>, curr: i32) -> Vec<i32> {
    let i = cups.iter().position(|&c| c == curr).unwrap() + 1;

    match (cups.len() - i).cmp(&3) {
        Ordering::Less => cups.rotate_left(3 - (cups.len() - i)),
        Ordering::Greater => cups.rotate_right(cups.len() - (3 + i)),
        Ordering::Equal => (),
    }

    vec![
        cups.pop_back().unwrap(),
        cups.pop_back().unwrap(),
        cups.pop_back().unwrap(),
    ]
}

fn place_three(cups: &mut VecDeque<i32>, dest: i32, to_place: Vec<i32>) {
    let mut to_place = to_place.to_owned();
    let r = cups.len() - (cups.iter().position(|&c| c == dest).unwrap() + 1);
    cups.rotate_right(r);
    cups.push_back(to_place.pop().unwrap());
    cups.push_back(to_place.pop().unwrap());
    cups.push_back(to_place.pop().unwrap());
}

fn wrapping_sub(i: i32, min: &i32, max: &i32) -> i32 {
    let ret = i - 1;

    if ret < *min {
        *max
    } else {
        ret
    }
}

fn play_game(cups: VecDeque<i32>, n: usize) -> VecDeque<i32> {
    let mut cups = cups;
    let mut curr = cups[0];
    let min = cups.iter().min().unwrap().clone();
    let max = cups.iter().max().unwrap().clone();

    for _ in 0..n {
        let three = pick_three(&mut cups, curr);

        // Pick destination
        let mut tmp = curr;
        let dest = loop {
            tmp = wrapping_sub(tmp, &min, &max);
            if let Some(_) = cups.iter().find(|&c| *c == tmp) {
                break tmp;
            }
        };

        place_three(&mut cups, dest, three);

        curr = cups[(cups.iter().position(|&c| c == curr).unwrap() + 1) % cups.len()];
    }

    cups
}

fn final_order(cups: VecDeque<i32>) -> Vec<i32> {
    let mut cups = cups;
    let i = cups.iter().position(|&c| c == 1).unwrap();
    cups.rotate_left(i);
    cups.iter().skip(1).cloned().collect()
}

fn crab_cups(cups: &[i32], n: usize) -> Vec<i32> {
    let mut xs = VecDeque::from_iter(cups.to_vec().iter().cloned());
    xs = play_game(xs, n);

    final_order(xs)
}

fn crab_cups_part2() -> (i32, i32) {
    let mut xs = VecDeque::from_iter(
        vec![2, 4, 7, 8, 1, 9, 3, 5, 6]
            .iter()
            .cloned()
            .chain(10..=1000000),
    );

    xs = play_game(xs, 10000000);
    let i = xs.iter().position(|&c| c == 1).unwrap();

    (xs[(i + 1) % 1000000], xs[(i + 2) % 1000000])
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, _input: &[&str]) -> String {
        crab_cups(&INPUT, 100).iter().join("")
    }

    fn part2(&self, _input: &[&str]) -> String {
        let (a, b) = crab_cups_part2();
        format!("({}, {})", a, b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crab_cups() {
        assert_eq!(
            vec![9, 2, 6, 5, 8, 3, 7, 4],
            crab_cups(&[3, 8, 9, 1, 2, 5, 4, 6, 7], 10),
        );
        assert_eq!(
            vec![6, 7, 3, 8, 4, 5, 2, 9],
            crab_cups(&[3, 8, 9, 1, 2, 5, 4, 6, 7], 100),
        );
    }
}
