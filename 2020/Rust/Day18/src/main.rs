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

// https://adventofcode.com/2020/day/17

#![allow(non_snake_case)]

use std::str::FromStr;

use aoc::fs::get_file_contents;


#[derive(Clone,Copy,Debug,Eq,PartialEq)]
enum Token {
    Number(i64),
    Add,
    Mul,
    LeftParen,
    RightParen,
}

type Expression = Vec<Token>;

trait Calculator {
    fn eval(exp: &Expression) -> i64;

    fn calc(v: &mut Vec<Token>) {
        if v.len() < 3 { return () }

        use Token::*;

        let op2 = v.pop();
        let op = v.pop();
        let op1 = v.pop();

        match (op1, op, op2) {
            (Some(Number(x)), Some(Add), Some(Number(y))) => v.push(Number(x+y)),
            (Some(Number(x)), Some(Mul), Some(Number(y))) => v.push(Number(x*y)),
            _ => panic!("wtf"),
        }
    }
}

struct BasicCalculator;

impl Calculator for BasicCalculator {
    fn eval(exp: &Expression) -> i64 {
        let mut stacks: Vec<Vec<Token>> = vec![vec![]];
        let mut i = 0;

        for token in exp.iter() {
            match token {
                Token::Add | Token::Mul => stacks[i].push(*token),
                Token::LeftParen => { stacks.push(Vec::new()); i += 1; },
                Token::RightParen => {
                    if let Some(n) = stacks[i].pop() {
                        stacks.pop();
                        i -= 1;
                        stacks[i].push(n);
                        Self::calc(&mut stacks[i]);
                    }
                },
                Token::Number(_) => {
                    stacks[i].push(*token);
                    Self::calc(&mut stacks[i]);
                },
            }
        }

        if let Token::Number(n) = stacks[0][0] {
            n
        } else {
            panic!("wtf")
        }
    }
}

struct AdvancedCalculator;

impl Calculator for AdvancedCalculator {
    fn eval(exp: &Expression) -> i64 {
        let mut stacks: Vec<Vec<Token>> = vec![vec![]];
        let mut i = 0;

        for token in exp.iter() {
            match token {
                Token::Add | Token::Mul => stacks[i].push(*token),
                Token::LeftParen => { stacks.push(Vec::new()); i += 1; },
                Token::RightParen => {
                    while stacks[i].len() > 1 { Self::calc(&mut stacks[i]); }
                    if let Some(n) = stacks[i].pop() {
                        stacks.pop();
                        i -= 1;
                        stacks[i].push(n);
                    }
                    while stacks[i].len() > 1 && 
                        stacks[i][stacks[i].len() - 2] == Token::Add {
                        Self::calc(&mut stacks[i]);
                    }
                },
                Token::Number(_) => {
                    stacks[i].push(*token);
                    while stacks[i].len() > 1 && 
                        stacks[i][stacks[i].len() - 2] == Token::Add {
                        Self::calc(&mut stacks[i]);
                    }
                },
            }
        }

        while stacks[0].len() > 1 { Self::calc(&mut stacks[0]); }

        if let Token::Number(n) = stacks[0][0] {
            n
        } else {
            panic!("wtf")
        }
    }
}

fn parse(exp: &String) -> Vec<Token> {
    let len = exp.len();
    let t = exp.chars().collect::<Vec<char>>();
    let mut i = 0;
    let mut res = Vec::new();

    while i < len {
        match t[i] {
            ' ' => (),
            '+' => res.push(Token::Add),
            '*' => res.push(Token::Mul),
            '(' => res.push(Token::LeftParen),
            ')' => res.push(Token::RightParen),
            _ => {
                let mut s = String::new();
                s.push(t[i]);
                while i < len-1 && t[i+1].is_digit(10) {
                    i += 1;
                    s.push(t[i]);
                }

                res.push(Token::Number(i64::from_str(&s).unwrap()));
            },
        }

        i += 1;
    }

    res
}

fn calculate<T>(lines: &Vec<String>) -> i64
where
    T: Calculator
{
    lines
        .iter()
        .fold(0, |acc, line| {
            acc + T::eval(&parse(line))
        })
}

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;

    println!("Day 18 / Part 1: {}", calculate::<BasicCalculator>(&lines));
    println!("Day 18 / Part 2: {}", calculate::<AdvancedCalculator>(&lines));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            vec![
                Token::Number(1),
                Token::Add,
                Token::LeftParen,
                Token::Number(2),
                Token::Mul,
                Token::Number(3),
                Token::RightParen,
            ],
            parse(&"1 + (2 * 3)".to_string()),
        );

        assert_eq!(
            vec![
                Token::LeftParen,
                Token::LeftParen,
                Token::Number(1),
                Token::Add,
                Token::Number(2),
                Token::RightParen,
                Token::RightParen,
            ],
            parse(&"(    ( 1       +  2  )   )".to_string()),
        );
    }

    #[test]
    fn test_eval_basic() {
        let exp = parse(&"1 + 2 * 3".to_string());
        assert_eq!(9, BasicCalculator::eval(&exp));

        let exp = parse(&"2 * 3 + (4 * 5)".to_string());
        assert_eq!(26, BasicCalculator::eval(&exp));

        let exp = parse(&"5 + (8 * 3 + 9 + 3 * 4 * 3)".to_string());
        assert_eq!(437, BasicCalculator::eval(&exp));
    }

    #[test]
    fn test_eval_advanced() {
        let exp = parse(&"1 + (2 * 3) + (4 * (5 + 6))".to_string());
        assert_eq!(51, AdvancedCalculator::eval(&exp));

        let exp = parse(&"2 * 3 + (4 * 5)".to_string());
        assert_eq!(46, AdvancedCalculator::eval(&exp));

        let exp = parse(&"5 + (8 * 3 + 9 + 3 * 4 * 3)".to_string());
        assert_eq!(1445, AdvancedCalculator::eval(&exp));

        let exp = parse(&"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".to_string());
        assert_eq!(669060, AdvancedCalculator::eval(&exp));

        let exp = parse(&"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".to_string());
        assert_eq!(23340, AdvancedCalculator::eval(&exp));

        let exp = parse(&"((((2 * 3) + 4) * 5) + 6)".to_string());
        assert_eq!(56, AdvancedCalculator::eval(&exp));

        let exp = parse(&"(2 * (3 + (4 * (5 + 6))))".to_string());
        assert_eq!(94, AdvancedCalculator::eval(&exp));
    }
}
