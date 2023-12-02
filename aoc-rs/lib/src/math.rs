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

use std::collections::HashSet;
use std::hash::Hash;

use num::Integer;

pub fn two_sum<T: Integer + Copy + Hash>(nums: &Vec<T>, target: T) -> Option<(T, T)> {
    let mut numbers = HashSet::new();

    for i in nums.iter() {
        match numbers.get(&(target - *i)) {
            None => {
                numbers.insert(i);
            }
            Some(&n) => {
                return Some((*i, *n));
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_two_sum_is_some() {
        assert_eq!(Some((25, 15)), two_sum(&vec![35, 20, 15, 25, 47], 40),);
    }

    #[test]
    fn test_two_sum_is_none() {
        assert_eq!(None, two_sum(&vec![1, 2, 3, 4, 5], 10),);
    }
}
