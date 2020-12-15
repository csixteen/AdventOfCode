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

// https://adventofcode.com/2020/day/14

#![allow(non_snake_case)]

use std::collections::HashMap;


fn init_sequence(seq: &Vec<usize>) -> HashMap<usize, Vec<usize>> {
    (0..seq.len()).fold(HashMap::new(), |mut acc, i| {
        acc.entry(seq[i]).or_insert(Vec::new()).push(i +1);
        acc
    })
}

fn nth_number_spoken(numbers: Vec<usize>, k: usize) -> usize {
    let mut seen: HashMap<usize, Vec<usize>> = init_sequence(&numbers);

    let len = numbers.len();
    let mut last_number = numbers[len-1];
    let mut turn = len+1;

    while turn <= k {
        if let None = seen.get(&last_number) {
            last_number = 0;
        } else if let Some(o) = seen.get(&last_number) {
            let l = o.len();
            if l == 1 {
                last_number = 0;
            } else {
                last_number = o[l-1] - o[l-2];
            }
        }

        seen.entry(last_number).or_insert(Vec::new()).push(turn);
        turn += 1;
    }

    last_number
}

fn main() {
    println!(
        "Day 15 / Part 1: {}",
        nth_number_spoken(vec![2, 0, 1, 9, 5, 19], 2020),
    );
    println!(
        "Day 15 / Part 2: {}",
        nth_number_spoken(vec![2, 0, 1, 9, 5, 19], 30000000),
    );
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
