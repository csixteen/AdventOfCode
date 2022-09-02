#![allow(non_snake_case)]

use aoc::fs::get_file_contents;

fn calculate_floor(input: &str) -> i32 {
    input.chars().fold(0, |acc, c| match c {
        '(' => acc + 1,
        ')' => acc - 1,
        _   => unreachable!(),
    })
}

fn first_time_position(input: &str) -> usize {
    let mut floor = 0;

    for (c, i) in input.chars().zip(1..) {
        floor += if c == '(' { 1 } else { -1 };
        if floor == -1 {
            return i;
        }
    }

    unreachable!()
}

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;
    
    println!("Part1: {}", calculate_floor(&lines[0]));
    println!("Part2: {}", first_time_position(&lines[0]));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let input_expected = [
            ("(())", 0),
            ("()()", 0),
            ("(((", 3),
            ("(()(()(", 3),
            ("))(((((", 3),
            ("())", -1),
            ("))(", -1),
            (")))", -3),
            (")())())", -3),
        ];

        for (i, e) in input_expected {
            assert_eq!(e, calculate_floor(i));
        }
    }

    #[test]
    fn test2() {
        assert_eq!(1, first_time_position(")"));
        assert_eq!(5, first_time_position("()())"));
    }
}
