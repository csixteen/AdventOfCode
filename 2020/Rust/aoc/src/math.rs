use std::collections::HashSet;
use std::hash::Hash;

use num::Integer;

pub fn two_sum<T: Integer + Copy + Hash>(nums: &Vec<T>, target: T) -> Option<(T, T)> {
    let mut numbers = HashSet::new();

    for i in nums.iter() {
        match numbers.get(&(target - *i)) {
            None => { numbers.insert(i); },
            Some(&n) => { return Some((*i, *n)); },
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_two_sum_is_some() {
        assert_eq!(
            Some((25, 15)),
            two_sum(&vec![35, 20, 15, 25, 47], 40),
        );
    }

    #[test]
    fn test_two_sum_is_none() {
        assert_eq!(
            None,
            two_sum(&vec![1, 2, 3, 4, 5], 10),
        );
    }
}
