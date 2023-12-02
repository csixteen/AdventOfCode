use std::ops::Add;

use aoc::Solver;

pub struct Solution;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct GameSet {
    red: usize,
    green: usize,
    blue: usize,
}

impl Add<&Self> for GameSet {
    type Output = Self;

    fn add(self, rhs: &Self) -> Self::Output {
        GameSet {
            red: self.red + rhs.red,
            green: self.green + rhs.green,
            blue: self.blue + rhs.blue,
        }
    }
}

impl GameSet {
    fn with_red(self, red: usize) -> Self {
        Self { red, ..self }
    }

    fn with_green(self, green: usize) -> Self {
        Self { green, ..self }
    }

    fn with_blue(self, blue: usize) -> Self {
        Self { blue, ..self }
    }

    fn is_valid(&self, config: &GameSet) -> bool {
        self.red <= config.red && self.green <= config.green && self.blue <= config.blue
    }

    fn max(&self, other: &GameSet) -> GameSet {
        GameSet {
            red: self.red.max(other.red),
            green: self.green.max(other.green),
            blue: self.blue.max(other.blue),
        }
    }

    fn power(&self) -> usize {
        self.red * self.green * self.blue
    }
}

impl From<&str> for GameSet {
    fn from(value: &str) -> Self {
        value
            .split(',')
            .map(|s| s.trim())
            .fold(GameSet::default(), |acc, s| {
                let parts: Vec<&str> = s.split(' ').collect();
                match parts[1] {
                    "red" => acc.with_red(parts[0].parse::<usize>().unwrap()),
                    "green" => acc.with_green(parts[0].parse::<usize>().unwrap()),
                    "blue" => acc.with_blue(parts[0].parse::<usize>().unwrap()),
                    _ => unreachable!(),
                }
            })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Game {
    id: usize,
    game_set: Vec<GameSet>,
}

impl Game {
    fn new(id: usize) -> Self {
        Self {
            id,
            ..Default::default()
        }
    }

    fn add_set(&mut self, set: GameSet) -> &mut Self {
        self.game_set.push(set);
        self
    }

    fn is_valid(&self, config: &GameSet) -> bool {
        self.game_set.iter().all(|set| set.is_valid(config))
    }

    fn smallest_config(&self) -> GameSet {
        self.game_set
            .iter()
            .fold(GameSet::default(), |acc, set| acc.max(set))
    }
}

impl From<&str> for Game {
    fn from(value: &str) -> Self {
        let parts: Vec<&str> = value.split(':').collect();
        let id = parts[0]
            .chars()
            .skip(5)
            .collect::<String>()
            .parse::<usize>()
            .unwrap();
        let game = Game::new(id);

        parts[1].split(';').fold(game, |mut acc, set| {
            acc.add_set(GameSet::from(set));
            acc
        })
    }
}

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let config = GameSet {
            red: 12,
            green: 13,
            blue: 14,
        };

        input
            .iter()
            .filter_map(|&s| {
                let game = Game::from(s);
                game.is_valid(&config).then_some(game.id)
            })
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        input
            .iter()
            .map(|&s| {
                let game = Game::from(s);
                game.smallest_config().power()
            })
            .sum::<usize>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_gameset() {
        assert_eq!(
            GameSet {
                red: 4,
                green: 0,
                blue: 3,
            },
            GameSet::from("3 blue, 4 red")
        );
        assert_eq!(
            GameSet {
                red: 1,
                green: 2,
                blue: 6
            },
            GameSet::from("1 red, 2 green, 6 blue"),
        );
    }

    #[test]
    fn test_build_game() {
        assert_eq!(
            Game {
                id: 100,
                game_set: vec![
                    GameSet {
                        red: 4,
                        green: 0,
                        blue: 35,
                    },
                    GameSet {
                        red: 1,
                        green: 2,
                        blue: 6,
                    },
                    GameSet {
                        red: 0,
                        green: 2,
                        blue: 0,
                    },
                ]
            },
            Game::from("Game 100: 35 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
        );
    }

    #[test]
    fn test_part1() {
        let s = Solution;

        assert_eq!(
            "8".to_string(),
            s.part1(&[
                "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
            ])
        );
    }

    #[test]
    fn test_smallest_config() {
        let game = Game::from("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green");
        assert_eq!(
            GameSet {
                red: 4,
                green: 2,
                blue: 6,
            },
            game.smallest_config(),
        );

        let game = Game::from("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue");
        assert_eq!(
            GameSet {
                red: 1,
                green: 3,
                blue: 4,
            },
            game.smallest_config()
        );
    }

    #[test]
    fn test_part2() {
        let s = Solution;

        assert_eq!(
            "2286".to_string(),
            s.part2(&[
                "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
            ])
        );
    }
}
