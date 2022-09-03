#![allow(non_snake_case)]

use std::str::FromStr;

use aoc::fs::get_file_contents;

#[derive(Clone,Debug,PartialEq)]
struct Dimension {
    l: i32,
    w: i32,
    h: i32,
}

impl Dimension {
    fn new(l: i32, w: i32, h: i32) -> Self {
        Dimension { l, w, h }
    }

    fn from_str(s: &str) -> Self {
        let parts: Vec<&str> = s.split('x').collect();
        let v: Vec<i32> = parts
            .iter()
            .map(|x| i32::from_str(x).expect("Can't parse number"))
            .collect();
        Self::new(v[0], v[1], v[2])
    }

    fn square_feet(&self) -> i32 {
        let lw = self.l*self.w;
        let wh = self.w*self.h;
        let hl = self.h*self.l;
        let min = lw.min(wh).min(hl);

        2*lw + 2*wh + 2*hl + min
    }

    fn ribbon(&self) -> i32 {
        let lw = 2*self.l + 2*self.w;
        let wh = 2*self.w + 2*self.h;
        let hl = 2*self.h + 2*self.l;
        let min = lw.min(wh).min(hl);
        min + self.l*self.w*self.h
    }
}

fn total_square_feet(lines: &Vec<String>) -> i32 {
    lines.iter().fold(0, |acc, d| acc + Dimension::from_str(d).square_feet())
}

fn total_ribbon(lines: &Vec<String>) -> i32 {
    lines.iter().fold(0, |acc, d| acc + Dimension::from_str(d).ribbon())
}


fn main() -> std::io::Result<()> {
    let lines: Vec<String> = get_file_contents("data/input.txt")?;

    println!("Part 1: {}", total_square_feet(&lines));
    println!("Part 2: {}", total_ribbon(&lines));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_dimensions() {
        assert_eq!(Dimension{ l: 1, w: 2, h: 3 }, Dimension::from_str("1x2x3"));
        assert_eq!(Dimension{ l: 10, w: 20, h: 30 }, Dimension::from_str("10x20x30"));
    }

    #[test]
    fn test_square_feet() {
        assert_eq!(58, Dimension::from_str("2x3x4").square_feet());
        assert_eq!(43, Dimension::from_str("1x1x10").square_feet());
    }

    #[test]
    fn test_ribbon() {
        assert_eq!(34, Dimension::from_str("2x3x4").ribbon());
        assert_eq!(14, Dimension::from_str("1x1x10").ribbon());
    }
}
