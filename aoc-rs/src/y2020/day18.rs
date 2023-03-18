use std::str::FromStr;

use aoc::Solver;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Token {
    Number(i64),
    Add,
    Mul,
    LeftParen,
    RightParen,
}

type Expression = Vec<Token>;

trait Calculator {
    fn eval(exp: &Expression) -> Token;

    fn calc(v: &mut Vec<Token>) {
        if v.len() < 3 {
            return ();
        }

        use Token::*;

        match (v.pop(), v.pop(), v.pop()) {
            (Some(Number(x)), Some(Add), Some(Number(y))) => v.push(Number(x + y)),
            (Some(Number(x)), Some(Mul), Some(Number(y))) => v.push(Number(x * y)),
            _ => panic!("wtf"),
        }
    }
}

struct BasicCalculator;

impl Calculator for BasicCalculator {
    fn eval(exp: &Expression) -> Token {
        let mut stacks: Vec<Vec<Token>> = vec![vec![]];
        let mut i = 0;

        for token in exp.iter() {
            match token {
                Token::Add | Token::Mul => stacks[i].push(*token),
                Token::LeftParen => {
                    stacks.push(Vec::new());
                    i += 1;
                }
                Token::RightParen => {
                    if let Some(n) = stacks[i].pop() {
                        stacks.pop();
                        i -= 1;
                        stacks[i].push(n);
                        Self::calc(&mut stacks[i]);
                    }
                }
                Token::Number(_) => {
                    stacks[i].push(*token);
                    Self::calc(&mut stacks[i]);
                }
            }
        }

        stacks[0][0]
    }
}

struct AdvancedCalculator;

impl Calculator for AdvancedCalculator {
    fn eval(exp: &Expression) -> Token {
        fn _calc_while(v: &mut Vec<Token>) {
            while v.len() > 1 {
                AdvancedCalculator::calc(v);
            }
        }

        fn _calc_while_add(v: &mut Vec<Token>) {
            while v.len() > 1 && v[v.len() - 2] == Token::Add {
                AdvancedCalculator::calc(v);
            }
        }

        let mut stacks: Vec<Vec<Token>> = vec![vec![]];
        let mut i = 0;

        for token in exp.iter() {
            match token {
                Token::Add | Token::Mul => stacks[i].push(*token),
                Token::LeftParen => {
                    stacks.push(Vec::new());
                    i += 1;
                }
                Token::RightParen => {
                    _calc_while(&mut stacks[i]);
                    if let Some(n) = stacks[i].pop() {
                        stacks.pop();
                        i -= 1;
                        stacks[i].push(n);
                        _calc_while_add(&mut stacks[i]);
                    }
                }
                Token::Number(_) => {
                    stacks[i].push(*token);
                    _calc_while_add(&mut stacks[i]);
                }
            }
        }

        _calc_while(&mut stacks[0]);

        stacks[0][0]
    }
}

fn parse(exp: &str) -> Vec<Token> {
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
                while i < len - 1 && t[i + 1].is_digit(10) {
                    i += 1;
                    s.push(t[i]);
                }

                res.push(Token::Number(i64::from_str(&s).unwrap()));
            }
        }

        i += 1;
    }

    res
}

fn calculate<T>(lines: &[&str]) -> i64
where
    T: Calculator,
{
    lines.iter().fold(0, |acc, &line| {
        if let Token::Number(x) = T::eval(&parse(line)) {
            acc + x
        } else {
            panic!("wtf")
        }
    })
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        calculate::<BasicCalculator>(input).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        calculate::<AdvancedCalculator>(input).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            vec![
                Number(1),
                Add,
                LeftParen,
                Number(2),
                Mul,
                Number(3),
                RightParen,
            ],
            parse("1 + (2 * 3)"),
        );

        assert_eq!(
            vec![
                LeftParen,
                LeftParen,
                Number(1),
                Add,
                Number(2),
                RightParen,
                RightParen,
            ],
            parse("(    ( 1       +  2  )   )"),
        );
    }

    #[test]
    fn test_eval_basic() {
        let exp = parse("1 + 2 * 3");
        assert_eq!(Number(9), BasicCalculator::eval(&exp));

        let exp = parse("2 * 3 + (4 * 5)");
        assert_eq!(Number(26), BasicCalculator::eval(&exp));

        let exp = parse("5 + (8 * 3 + 9 + 3 * 4 * 3)");
        assert_eq!(Number(437), BasicCalculator::eval(&exp));
    }

    #[test]
    fn test_eval_advanced() {
        let exp = parse("1 + (2 * 3) + (4 * (5 + 6))");
        assert_eq!(Number(51), AdvancedCalculator::eval(&exp));

        let exp = parse("2 * 3 + (4 * 5)");
        assert_eq!(Number(46), AdvancedCalculator::eval(&exp));

        let exp = parse("5 + (8 * 3 + 9 + 3 * 4 * 3)");
        assert_eq!(Number(1445), AdvancedCalculator::eval(&exp));

        let exp = parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))");
        assert_eq!(Number(669060), AdvancedCalculator::eval(&exp));

        let exp = parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2");
        assert_eq!(Number(23340), AdvancedCalculator::eval(&exp));

        let exp = parse("((((2 * 3) + 4) * 5) + 6)");
        assert_eq!(Number(56), AdvancedCalculator::eval(&exp));

        let exp = parse("(2 * (3 + (4 * (5 + 6))))");
        assert_eq!(Number(94), AdvancedCalculator::eval(&exp));
    }
}
