use std::collections::HashMap;
use std::str::FromStr;

use lazy_static::lazy_static;
use regex::Regex;

use aoc::Solver;

const BITMASK_SIZE: usize = 36;

#[derive(Clone, Copy, Debug, PartialEq)]
enum MaskBit {
    Bit(u8),
    X,
}

trait Emulator {
    fn bitmask(&mut self) -> &mut Vec<MaskBit>;
    fn memory(&mut self) -> &mut HashMap<u64, u64>;
    fn translate_value(&mut self, val: u64) -> u64;
    fn translate_address(&mut self, addr: u64) -> Vec<u64>;

    fn execute(&mut self, lines: &[&str]) -> u64 {
        for line in lines.iter() {
            self.parse_instruction(line);
        }

        self.memory().values().sum()
    }

    fn replace_bitmask(&mut self, s: &String) {
        for (i, c) in s.chars().enumerate() {
            match c {
                'X' => self.bitmask()[i] = MaskBit::X,
                '1' => self.bitmask()[i] = MaskBit::Bit(1),
                '0' => self.bitmask()[i] = MaskBit::Bit(0),
                _ => panic!("Unknown mask bit"),
            }
        }
    }

    fn parse_instruction(&mut self, line: &str) {
        lazy_static! {
            static ref MASK: Regex = Regex::new(r"mask = (\w+)").unwrap();
            static ref APPLY: Regex = Regex::new(r"mem\[(\d+)\] = (\d+)").unwrap();
        }

        if let Some(c) = MASK.captures(line) {
            self.replace_bitmask(&c.get(1).unwrap().as_str().to_owned());
        } else if let Some(c) = APPLY.captures(line) {
            let address = u64::from_str(c.get(1).unwrap().as_str()).unwrap();
            let value = u64::from_str(c.get(2).unwrap().as_str()).unwrap();
            let new_value = self.translate_value(value);
            let new_addrs = self.translate_address(address);

            new_addrs.iter().for_each(|addr| {
                self.memory().insert(*addr, new_value);
            });
        }
    }
}

#[derive(Default)]
struct EmulatorPart1 {
    mask: Vec<MaskBit>,
    mem: HashMap<u64, u64>,
}

impl EmulatorPart1 {
    fn new(size: usize) -> Self {
        Self {
            mask: vec![MaskBit::X; size],
            ..Default::default()
        }
    }
}

impl Emulator for EmulatorPart1 {
    fn bitmask(&mut self) -> &mut Vec<MaskBit> {
        &mut self.mask
    }

    fn memory(&mut self) -> &mut HashMap<u64, u64> {
        &mut self.mem
    }

    fn translate_value(&mut self, val: u64) -> u64 {
        let mut res: u64 = 0;

        for (i, mb) in self.bitmask().iter().rev().enumerate() {
            match mb {
                MaskBit::X => res |= ((val >> i) & 0x1) << i,
                MaskBit::Bit(n) => res |= (*n as u64) << i,
            }
        }

        res
    }

    fn translate_address(&mut self, addr: u64) -> Vec<u64> {
        vec![addr]
    }
}

#[derive(Default)]
struct EmulatorPart2 {
    mask: Vec<MaskBit>,
    mem: HashMap<u64, u64>,
}

impl EmulatorPart2 {
    fn new(size: usize) -> Self {
        Self {
            mask: vec![MaskBit::X; size],
            ..Default::default()
        }
    }
}

impl Emulator for EmulatorPart2 {
    fn bitmask(&mut self) -> &mut Vec<MaskBit> {
        &mut self.mask
    }

    fn memory(&mut self) -> &mut HashMap<u64, u64> {
        &mut self.mem
    }

    fn translate_value(&mut self, val: u64) -> u64 {
        val
    }

    fn translate_address(&mut self, addr: u64) -> Vec<u64> {
        fn comb(n: u32) -> Vec<Vec<MaskBit>> {
            (0..2_usize.pow(n))
                .map(|i| {
                    format!("{:0>36b}", i)
                        .chars()
                        .map(|c| match c {
                            '1' => MaskBit::Bit(1),
                            '0' => MaskBit::Bit(0),
                            _ => panic!(),
                        })
                        .rev()
                        .take(n as usize)
                        .collect::<Vec<MaskBit>>()
                        .iter()
                        .rev()
                        .cloned()
                        .collect()
                })
                .collect::<Vec<Vec<MaskBit>>>()
        }

        fn intermediary_result(a: u64, bm: &Vec<MaskBit>) -> Vec<MaskBit> {
            let mut res: Vec<MaskBit> = Vec::new();

            for i in 0..BITMASK_SIZE {
                match bm[(BITMASK_SIZE - 1) - i] {
                    MaskBit::Bit(0) => res.push(MaskBit::Bit(((a >> i) & 0x1) as u8)),
                    b => res.push(b),
                }
            }

            res.iter().rev().cloned().collect()
        }

        fn update_bitmask(bm: Vec<MaskBit>, u: &Vec<(MaskBit, usize)>) -> Vec<MaskBit> {
            let mut bm = bm;
            u.iter().for_each(|(mb, i)| bm[*i] = *mb);
            bm
        }

        fn intermediary_to_u64(bm: Vec<MaskBit>) -> u64 {
            let s: String = bm
                .iter()
                .map(|b| match b {
                    MaskBit::Bit(1) => '1',
                    MaskBit::Bit(0) => '0',
                    _ => panic!("Malformed intermediary result"),
                })
                .collect();

            u64::from_str_radix(&s, 2).unwrap()
        }

        let im = intermediary_result(addr, self.bitmask());
        let x_pos: Vec<usize> = im
            .iter()
            .enumerate()
            .filter(|&(_, b)| *b == MaskBit::X)
            .map(|(i, _)| i)
            .collect();

        let x_comb: Vec<Vec<(MaskBit, usize)>> = comb(x_pos.len() as u32)
            .iter()
            .map(|v| v.iter().cloned().zip(x_pos.iter().cloned()).collect())
            .collect();

        let mut res = vec![];
        for xc in x_comb.iter() {
            res.push(intermediary_to_u64(update_bitmask(im.clone(), xc)));
        }

        res
    }
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        EmulatorPart1::new(BITMASK_SIZE).execute(input).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        EmulatorPart2::new(BITMASK_SIZE).execute(input).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translate_value() {
        let mut emul = EmulatorPart1::new(BITMASK_SIZE);

        emul.replace_bitmask(&"XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X".to_string());
        assert_eq!(73, emul.translate_value(11));
        assert_eq!(101, emul.translate_value(101));
        assert_eq!(64, emul.translate_value(0));
    }

    #[test]
    fn test_translate_address1() {
        let mut emul = EmulatorPart2::new(BITMASK_SIZE);

        emul.replace_bitmask(&"000000000000000000000000000000X1001X".to_string());
        assert_eq!(vec![26, 27, 58, 59], emul.translate_address(42),);
    }

    #[test]
    fn test_translate_address2() {
        let mut emul = EmulatorPart2::new(BITMASK_SIZE);

        emul.replace_bitmask(&"00000000000000000000000000000000X0XX".to_string());
        assert_eq!(
            vec![16, 17, 18, 19, 24, 25, 26, 27],
            emul.translate_address(26),
        );
    }

    #[test]
    fn test_sum_memory1() {
        let mut emul = EmulatorPart1::new(BITMASK_SIZE);

        assert_eq!(
            165,
            emul.execute(&vec![
                "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                "mem[8] = 11",
                "mem[7] = 101",
                "mem[8] = 0",
            ],),
        );
    }

    #[test]
    fn test_sum_memory2() {
        let mut emul = EmulatorPart2::new(BITMASK_SIZE);

        assert_eq!(
            208,
            emul.execute(&vec![
                "mask = 000000000000000000000000000000X1001X",
                "mem[42] = 100",
                "mask = 00000000000000000000000000000000X0XX",
                "mem[26] = 1",
            ]),
        );
    }
}
