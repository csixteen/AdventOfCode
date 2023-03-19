use std::cmp::Ordering;
use std::collections::{HashSet, VecDeque};
use std::iter::FromIterator;

use aoc::Solver;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Player {
    You,
    Crab,
}

type Hand = VecDeque<usize>;

trait GamePlay {
    fn play_game(you: &[usize], crab: &[usize]) -> (Player, Hand);

    fn round_winner(you: &Hand, crab: &Hand) -> Player {
        match you[0].cmp(&crab[0]) {
            Ordering::Less => Player::Crab,
            Ordering::Greater => Player::You,
            Ordering::Equal => panic!("Invalid hands: {:?} / {:?}", you, crab),
        }
    }

    fn game_winner(you: &Hand, crab: &Hand) -> Option<Player> {
        if you.is_empty() {
            Some(Player::Crab)
        } else if crab.is_empty() {
            Some(Player::You)
        } else {
            None
        }
    }

    fn score(hand: &Hand) -> usize {
        hand.iter()
            .zip((1..=hand.len()).rev())
            .fold(0_usize, |acc, (card, i)| acc + card * i as usize)
    }
}

struct RegularCombat;

impl GamePlay for RegularCombat {
    fn play_game(hand1: &[usize], hand2: &[usize]) -> (Player, Hand) {
        let mut you = VecDeque::from_iter(hand1.to_vec().iter().cloned());
        let mut crab = VecDeque::from_iter(hand2.to_vec().iter().cloned());

        loop {
            if let Some(p) = Self::game_winner(&you, &crab) {
                return (p, if p == Player::You { you } else { crab });
            }

            match Self::round_winner(&you, &crab) {
                Player::You => {
                    you.rotate_left(1);
                    you.push_back(crab.pop_front().unwrap());
                }
                Player::Crab => {
                    crab.rotate_left(1);
                    crab.push_back(you.pop_front().unwrap());
                }
            }
        }
    }
}

struct RecursiveCombat;

impl RecursiveCombat {
    fn go_recursive(you: &Hand, crab: &Hand) -> bool {
        you.len() > (you[0] as usize) && crab.len() > (crab[0] as usize)
    }

    fn hand_to_string(hand: &Hand) -> String {
        hand.iter()
            .map(|c| format!("{}", c))
            .collect::<Vec<String>>()
            .join("")
    }

    fn play(you: Hand, crab: Hand) -> (Player, Hand) {
        let mut you = you;
        let mut crab = crab;
        let mut past_hands: HashSet<(String, String)> = HashSet::new();

        loop {
            // Before dealing any cards, we need to search for repeated
            // hands in the present game.
            let yp = Self::hand_to_string(&you);
            let cp = Self::hand_to_string(&crab);

            if past_hands.contains(&(yp.to_string(), cp.to_string())) {
                return (Player::You, you);
            }

            // No repeated hands
            past_hands.insert((yp.to_string(), cp.to_string()));

            // Do both players have a hand that has at least as many cards as
            // the value of their top card? If so, we go recursive to determine
            // the winner of the round. If not, the winner of the round is
            // determined using the normal rules.
            let winner = if Self::go_recursive(&you, &crab) {
                let ny = VecDeque::from_iter(you.iter().skip(1).take(you[0]).cloned());
                let nc = VecDeque::from_iter(crab.iter().skip(1).take(crab[0]).cloned());
                let (w, _) = Self::play(ny, nc);
                w
            } else {
                Self::round_winner(&you, &crab)
            };

            match winner {
                Player::You => {
                    you.rotate_left(1);
                    you.push_back(crab.pop_front().unwrap());
                }
                Player::Crab => {
                    crab.rotate_left(1);
                    crab.push_back(you.pop_front().unwrap());
                }
            }

            if let Some(p) = Self::game_winner(&you, &crab) {
                return (p, if p == Player::You { you } else { crab });
            }
        }
    }
}

impl GamePlay for RecursiveCombat {
    fn play_game(hand1: &[usize], hand2: &[usize]) -> (Player, Hand) {
        let you = VecDeque::from_iter(hand1.to_vec().iter().cloned());
        let crab = VecDeque::from_iter(hand2.to_vec().iter().cloned());

        Self::play(you, crab)
    }
}

fn crab_combat<T>(hand1: &[usize], hand2: &[usize]) -> usize
where
    T: GamePlay,
{
    let (_, winner_hand) = T::play_game(hand1, hand2);
    T::score(&winner_hand)
}

const PLAYER1: [usize; 25] = [
    21, 22, 33, 29, 43, 35, 8, 30, 50, 44, 9, 42, 45, 16, 12, 4, 15, 27, 20, 31, 25, 47, 5, 24, 19,
];

const PLAYER2: [usize; 25] = [
    3, 40, 37, 14, 1, 13, 49, 41, 28, 48, 18, 7, 23, 38, 32, 34, 46, 39, 17, 2, 11, 6, 10, 36, 26,
];

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, _input: &[&str]) -> String {
        crab_combat::<RegularCombat>(&PLAYER1, &PLAYER2).to_string()
    }

    fn part2(&self, _input: &[&str]) -> String {
        crab_combat::<RecursiveCombat>(&PLAYER1, &PLAYER2).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regular_combat() {
        assert_eq!(
            306,
            crab_combat::<RegularCombat>(&[9, 2, 6, 3, 1], &[5, 8, 4, 7, 10])
        );
    }

    #[test]
    fn test_recursive_combat() {
        assert_eq!(
            291,
            crab_combat::<RecursiveCombat>(&[9, 2, 6, 3, 1], &[5, 8, 4, 7, 10])
        );
    }
}
