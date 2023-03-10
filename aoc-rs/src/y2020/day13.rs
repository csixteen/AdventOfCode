use std::str::FromStr;

use aoc::Solver;

fn shuttle_search(lines: &[&str]) -> usize {
    let timestamp = usize::from_str(&lines[0]).unwrap();
    let mut wait_time = usize::MAX;
    let mut bus_id = 0;

    for id in lines[1].split(',') {
        if let Ok(n) = usize::from_str(id) {
            let wt = n * (timestamp / n + 1) - timestamp;
            if wt < wait_time {
                wait_time = wt;
                bus_id = n;
            }
        }
    }

    wait_time * bus_id
}

// https://math.stackexchange.com/questions/147152/how-to-find-the-meeting-number-of-two-sequences
// https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
// https://www.discoverbits.in/post/extended-euclid-algorithm-for-gcd-in-python/
fn extended_gcd(a: i128, b: i128) -> (i128, i128, i128) {
    /*
     * The extended GCD not only returns the gcd(a, b) but also the
     * coefficients of Bezout's identity, which are integers x and y
     * such that a*x + b*y = gcd(a,b). This method can be used to find
     * the solutions to linear Diophantine equations.
     */
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, m, n) = extended_gcd(b % a, a);
        (g, n - (b / a) * m, m)
    }
}

// https://cronokirby.com/posts/2020/12/chinese-remainder-theorem/
// Many thanks!!
fn calculate(a: (i128, i128), b: (i128, i128)) -> (i128, i128) {
    let (_, m, n) = extended_gcd(a.0, b.0);
    let k = a.1 * n * b.0 + b.1 * m * a.0;

    // Because I'm using i128, I need to use rem_euclid, or else the
    // regular % will return negative results at some point.
    (a.0 * b.0, k.rem_euclid(a.0 * b.0))
}

fn minimum_timestamp(ids: Vec<(i128, i128)>) -> i128 {
    let (mut a_i, mut a_j) = (ids[0].0, ids[0].1);

    for (i, offset) in ids.iter().skip(1) {
        let tmp = calculate((a_i, a_j), (*i, *offset));
        a_i = tmp.0;
        a_j = tmp.1;
    }

    a_i % a_j
}

fn first_timestamp(ids: &str) -> i128 {
    minimum_timestamp(
        ids.split(',')
            .enumerate()
            .fold(Vec::new(), |mut acc, (i, id)| {
                if let Ok(n) = i128::from_str(id) {
                    acc.push((n, i as i128));
                }
                acc
            }),
    )
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        shuttle_search(input).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        first_timestamp(input[1]).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shuttle_search() {
        assert_eq!(295, shuttle_search(&["939", "7,13,x,x,59,x,31,19"],),);
    }

    #[test]
    fn test_minimum_timestamp() {
        assert_eq!(
            1068781,
            minimum_timestamp(vec![(7, 0), (13, 1), (59, 4), (31, 6), (19, 7)]),
        );
        assert_eq!(
            754018,
            minimum_timestamp(vec![(67, 0), (7, 1), (59, 2), (61, 3)]),
        );
        assert_eq!(
            779210,
            minimum_timestamp(vec![(67, 0), (7, 2), (59, 3), (61, 4)]),
        );
    }
}
