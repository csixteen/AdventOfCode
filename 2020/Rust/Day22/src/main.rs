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

// https://adventofcode.com/2020/day/22

#![allow(non_snake_case)]

use std::cmp::Ordering;
use std::collections::{HashSet,VecDeque};
use std::iter::FromIterator;


#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
enum Player {
    You,
    Crab,
}

type Hand = VecDeque<u32>;

trait GamePlay {
    fn play_game(you: &[u32], crab: &[u32]) -> (Player, Hand);

    fn round_winner(you: &Hand, crab: &Hand) -> Player {
        match you[0].cmp(&crab[0]) {
            Ordering::Less => Player::Crab,
            Ordering::Greater => Player::You,
            Ordering::Equal => panic!("Invalid hands: {:?} / {:?}", you, crab),
        }
    }

    fn game_winner(you: &Hand, crab: &Hand) -> Option<(Player, Hand)> {
        if you.is_empty() {
            Some((Player::Crab, crab.clone()))
        } else if crab.is_empty() {
            Some((Player::You, you.clone()))
        } else {
            None
        }
    }

    fn score(hand: &Hand) -> u32 {
        hand
            .iter()
            .zip((1..=hand.len()).rev())
            .fold(0_u32, |acc, (card, i)| acc + card*i as u32)
    }
}

struct RegularCombat;

impl GamePlay for RegularCombat {
    fn play_game(hand1: &[u32], hand2: &[u32]) -> (Player, Hand) {
        let mut you = VecDeque::from_iter(hand1.to_vec().iter().cloned());
        let mut crab = VecDeque::from_iter(hand2.to_vec().iter().cloned());

        loop {
            if let Some((p, h)) = Self::game_winner(&you, &crab) {
                return (p, h)
            }

            match Self::round_winner(&you, &crab) {
                Player::You => {
                    you.rotate_left(1);
                    you.push_back(crab.pop_front().unwrap());
                },
                Player::Crab => {
                    crab.rotate_left(1);
                    crab.push_back(you.pop_front().unwrap());
                },
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
        hand.iter().map(|c| format!("{}", c)).collect::<Vec<String>>().join("")
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
                return (Player::You, you.to_owned())
            }

            // No repeated hands
            past_hands.insert((yp.to_string(), cp.to_string()));

            // Do both players have a hand that has at least as many cards as
            // the value of their top card? If so, we go recursive to determine
            // the winner of the round. If not, the winner of the round is
            // determined using the normal rules.
            let winner = if Self::go_recursive(&you, &crab) {
                let new_you = VecDeque::from_iter(
                    you.iter().skip(1).take(you[0] as usize).cloned()
                );
                let new_crab = VecDeque::from_iter(
                    crab.iter().skip(1).take(crab[0] as usize).cloned()
                );
                let (w, _) = Self::play(new_you, new_crab);
                w
            } else {
                Self::round_winner(&you, &crab)
            };

            match winner {
                Player::You => {
                    you.rotate_left(1);
                    you.push_back(crab.pop_front().unwrap());
                },
                Player::Crab => {
                    crab.rotate_left(1);
                    crab.push_back(you.pop_front().unwrap());
                },
            }

            if let Some((p, h)) = Self::game_winner(&you, &crab) {
                return (p, h)
            }
        }
    }
}

impl GamePlay for RecursiveCombat {
    fn play_game(hand1: &[u32], hand2: &[u32]) -> (Player, Hand) {
        let you = VecDeque::from_iter(hand1.to_vec().iter().cloned());
        let crab = VecDeque::from_iter(hand2.to_vec().iter().cloned());

        Self::play(you, crab)
    }
}

fn crab_combat<T>(hand1: &[u32], hand2: &[u32]) -> u32
where
    T: GamePlay
{
    let (_, winner_hand) = T::play_game(hand1, hand2);
    T::score(&winner_hand)
}

const PLAYER1: [u32; 25] = [
    21, 22, 33, 29, 43,
    35, 8,  30, 50, 44,
    9,  42, 45, 16, 12,
    4,  15, 27, 20, 31,
    25, 47, 5,  24, 19,
];

const PLAYER2: [u32; 25] = [
    3,  40, 37, 14, 1,
    13, 49, 41, 28, 48,
    18, 7,  23, 38, 32,
    34, 46, 39, 17, 2,
    11, 6,  10, 36, 26,
];

fn main() {
    println!("Day 22 / Part 1: {}", crab_combat::<RegularCombat>(&PLAYER1, &PLAYER2));
    println!("Day 22 / Part 2: {}", crab_combat::<RecursiveCombat>(&PLAYER1, &PLAYER2));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regular_combat() {
        assert_eq!(306, crab_combat::<RegularCombat>(&[9,2,6,3,1], &[5,8,4,7,10]));
    }

    #[test]
    fn test_recursive_combat() {
        assert_eq!(291, crab_combat::<RecursiveCombat>(&[9,2,6,3,1], &[5,8,4,7,10]));
    }
}
