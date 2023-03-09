use std::str::FromStr;

use aoc::Solver;

trait Ferry {
    fn manhattan_distance(&mut self) -> i32;
    fn process_instruction(&mut self, i: char, n: usize);
    fn rotate(&mut self, n: usize);
}

#[derive(Default)]
struct NormalFerry {
    q: usize,
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
}

impl NormalFerry {
    fn new() -> Self {
        NormalFerry {
            dx: 1,
            ..Default::default()
        }
    }
}

impl Ferry for NormalFerry {
    fn manhattan_distance(&mut self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    fn process_instruction(&mut self, i: char, n: usize) {
        match i {
            'N' => self.y += n as i32,
            'S' => self.y -= n as i32,
            'E' => self.x += n as i32,
            'W' => self.x -= n as i32,
            'F' => {
                self.x += (n as i32) * self.dx;
                self.y += (n as i32) * self.dy;
            }
            'R' => self.rotate(n),
            'L' => self.rotate(360 - n),
            _ => panic!("Unknown instruction"),
        }
    }

    fn rotate(&mut self, n: usize) {
        let quadrants = vec![(1, 0), (0, -1), (-1, 0), (0, 1)];

        self.q += n / 90;
        self.dx = quadrants[self.q % 4].0;
        self.dy = quadrants[self.q % 4].1;
    }
}

#[derive(Default)]
struct WaypointFerry {
    x: i32,
    y: i32,
    wp_x: i32,
    wp_y: i32,
}

impl WaypointFerry {
    fn new() -> Self {
        WaypointFerry {
            wp_x: 10,
            wp_y: 1,
            ..Default::default()
        }
    }
}

impl Ferry for WaypointFerry {
    fn manhattan_distance(&mut self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    fn process_instruction(&mut self, i: char, n: usize) {
        match i {
            'N' => self.wp_y += n as i32,
            'S' => self.wp_y -= n as i32,
            'E' => self.wp_x += n as i32,
            'W' => self.wp_x -= n as i32,
            'F' => {
                self.x += (n as i32) * self.wp_x;
                self.y += (n as i32) * self.wp_y;
            }
            'R' => self.rotate(n),
            'L' => self.rotate(360 - n),
            _ => panic!("Unknown instruction"),
        }
    }

    fn rotate(&mut self, n: usize) {
        for _ in 0..(n % 360) / 90 {
            let tmp = self.wp_x;
            self.wp_x = self.wp_y;
            self.wp_y = -tmp;
        }
    }
}

fn manhattan_distance(mut ferry: impl Ferry, instructions: &[(char, usize)]) -> i32 {
    for (c, i) in instructions.iter() {
        ferry.process_instruction(*c, *i);
    }

    ferry.manhattan_distance()
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let instructions: Vec<(char, usize)> = input
            .iter()
            .map(|line| {
                (
                    line.chars().nth(0).unwrap(),
                    usize::from_str(&line[1..]).unwrap(),
                )
            })
            .collect();
        let ferry = NormalFerry::new();
        manhattan_distance(ferry, &instructions).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let instructions: Vec<(char, usize)> = input
            .iter()
            .map(|line| {
                (
                    line.chars().nth(0).unwrap(),
                    usize::from_str(&line[1..]).unwrap(),
                )
            })
            .collect();
        let ferry = WaypointFerry::new();
        manhattan_distance(ferry, &instructions).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rotations_normal_ferry() {
        let mut ferry = NormalFerry::new();

        assert_eq!(1, ferry.dx);
        assert_eq!(0, ferry.dy);

        ferry.rotate(90);
        assert_eq!(-1, ferry.dy);
        assert_eq!(0, ferry.dx);

        ferry.rotate(270);
        assert_eq!(1, ferry.dx);
        assert_eq!(0, ferry.dy);
    }

    #[test]
    fn test_manhattan_distance_normal_ferry() {
        let ferry = NormalFerry::new();

        assert_eq!(
            25,
            manhattan_distance(
                ferry,
                &vec![('F', 10), ('N', 3), ('F', 7), ('R', 90), ('F', 11),],
            ),
        );
    }

    #[test]
    fn test_manhattan_distance_waypoint_ferry() {
        let ferry = WaypointFerry::new();

        assert_eq!(
            286,
            manhattan_distance(
                ferry,
                &vec![('F', 10), ('N', 3), ('F', 7), ('R', 90), ('F', 11),],
            ),
        );
    }
}
