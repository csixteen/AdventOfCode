#![allow(non_snake_case)]

use std::collections::HashSet;
use std::str::FromStr;

use aoc::Solver;


#[derive(Clone, Debug, PartialEq)]
enum Instruction {
    NOP,
    JMP,
    ACC,
}

#[derive(Default)]
struct Program {
    acc: i32,
    pc: i32,
    visited: HashSet<i32>,
    code: Vec<(Instruction, i32)>,
}

impl Program {
    fn new(code: &Vec<(Instruction, i32)>) -> Self {
        Program {
            code: code.to_vec(),
            ..Default::default()
        }
    }

    fn execute_instruction(&mut self) -> Result<(), i32> {
        use Instruction::*;

        match self.visited.get(&self.pc) {
            Some(_) => Err(self.acc),
            None => {
                self.visited.insert(self.pc);

                match &self.code[self.pc as usize] {
                    (NOP, _) => self.pc += 1,
                    (JMP, n) => self.pc += n,
                    (ACC, n) => {
                        self.acc += n;
                        self.pc += 1;
                    },
                };

                Ok(())
            },
        }
    }

    fn execute(&mut self) -> Result<i32, i32> {
        while (self.pc as usize) < self.code.len() {
            self.execute_instruction()?;
        }

        Ok(self.acc)
    }
}

fn fix_and_execute(code: &Vec<(Instruction, i32)>) -> i32 {
    use Instruction::*;

    for i in 0..code.len() {
        if code[i].0 == ACC { continue; }

        let mut new_code = code.clone();
        new_code[i] = (
            if code[i].0 == NOP { JMP } else { NOP },
            code[i].1,
        );

        let mut p = Program::new(&new_code);
        if let Ok(n) = p.execute() {
            return n;
        }
    }

    unreachable!();
}

fn parse_code(lines: &Vec<&str>) -> Vec<(Instruction, i32)> {
    lines
        .iter()
        .map(|line| {
            let s: Vec<&str> = line.split(" ").collect();
            let op = match s[0] {
                "nop" => Instruction::NOP,
                "jmp" => Instruction::JMP,
                "acc" => Instruction::ACC,
                _ => panic!("Unknown instruction"),
            };

            (op, i32::from_str(s[1]).unwrap())
        })
        .collect()
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        let code = parse_code(input);
        let mut p = Program::new(&code);
        match p.execute() {
            Ok(v) => v.to_string(),
            Err(v) => v.to_string(),
        }
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        let code = parse_code(input);
        fix_and_execute(&code).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CODE: [(Instruction, i32); 9] = [
        (Instruction::NOP, 0),
        (Instruction::ACC, 1),
        (Instruction::JMP, 4),
        (Instruction::ACC, 3),
        (Instruction::JMP, -3),
        (Instruction::ACC, -99),
        (Instruction::ACC, 1),
        (Instruction::JMP, -4),
        (Instruction::ACC, 6),
    ];

    #[test]
    fn test_execute() {
        let mut p = Program::new(&CODE.to_vec());

        assert_eq!(Err(5), p.execute());
    }

    #[test]
    fn test_fix_and_execute() {
        assert_eq!(8, fix_and_execute(&CODE.to_vec()));
    }
}
