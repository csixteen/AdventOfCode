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

// https://adventofcode.com/2020/day/8

#![allow(non_snake_case)]

use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::str::FromStr;

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

fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    let mut file = File::open("data/input.txt")?;

    file.read_to_string(&mut buffer).unwrap();
    let code: Vec<(Instruction, i32)> = buffer
        .trim()
        .split("\n")
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
        .collect();

    let mut p = Program::new(&code);

    println!("Day 8 / Part 1: {:?}", p.execute());
    println!("Day 8 / Part 2: {}", fix_and_execute(&code));

    Ok(())
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
        let mut p = Program::new(&CODE.to_vec());

        assert_eq!(8, fix_and_execute(&CODE.to_vec()));
    }
}
