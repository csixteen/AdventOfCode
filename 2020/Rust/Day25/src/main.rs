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

// https://adventofcode.com/2020/day/25

#![allow(non_snake_case)]

fn calculate_loop_size(subj: i64, pubkey: i64) -> usize {
    let mut l = 1;
    let mut val = subj;

    loop {
        if val == pubkey { return l; }
        val *= subj;
        val = val % 20201227;
        l += 1;
    }
}

fn generate_encryption_key(loop_size: usize, pubkey: i64) -> i64 {
    (0..loop_size).fold(1, |acc, _| {
        let v = acc * pubkey;
        v % 20201227
    })
}

fn combo_breaker<'a>(pubkey1: i64, pubkey2: i64) -> Result<i64, &'a str> {
    let loop1 = calculate_loop_size(7, pubkey1);
    let key1 = generate_encryption_key(loop1, pubkey2);
    let loop2 = calculate_loop_size(7, pubkey2);
    let key2 = generate_encryption_key(loop2, pubkey1);

    if key1 == key2 { Ok(key1) }
    else { Err("Keys don't match") }
}

const CARD_PUBKEY: i64 = 11239946;
const DOOR_PUBKEY: i64 = 10464955;

fn main() {
    println!("Day 25 / Part 1: {:?}", combo_breaker(CARD_PUBKEY, DOOR_PUBKEY));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_loop_size() {
        assert_eq!(8, calculate_loop_size(7, 5764801));
        assert_eq!(11, calculate_loop_size(7, 17807724));
    }

    #[test]
    fn test_generate_encryption_key() {
        assert_eq!(14897079, generate_encryption_key(8, 17807724));
        assert_eq!(14897079, generate_encryption_key(11, 5764801));
    }
}
