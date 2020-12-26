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

// https://adventofcode.com/2020/day/24

#![allow(non_snake_case)]

use std::collections::HashMap;

use aoc::fs::get_file_contents;


fn process_steps(line: &String) -> (i32, i32) {
    let len = line.len();
    let (mut x, mut y) = (0, 0);
    let mut i = 0;

    while i < len {
        match line.chars().nth(i).unwrap() {
            'e' => x += 2,
            'w' => x -= 2,
            'n' => {
                y += 1;
                i += 1;
                x += if line.chars().nth(i).unwrap() == 'e' { 1 } else { -1 };
            },
            's' => {
                y -= 1;
                i += 1;
                x += if line.chars().nth(i).unwrap() == 'e' { 1 } else { -1 };
            },
            _ => panic!("Unknown direction"),
        }

        i += 1;
    }

    (x, y)
}

fn black_tiles(lines: &Vec<String>) -> usize {
    let tiles: HashMap<(i32, i32), i32> = lines
        .iter()
        .fold(HashMap::new(), |mut acc, line| {
            let (x, y) = process_steps(line);
            acc.entry((x, y)).and_modify(|e| { *e *= -1 }).or_insert(1);
            acc
        });

    tiles.values().filter(|&v| *v == 1).count()
}

fn neighbours(x: i32, y: i32) -> Vec<(i32, i32)> {
    vec![
        (x-1, y+1),
        (x-2, y),
        (x-1, y-1),
        (x+1, y-1),
        (x+2, y),
        (x+1, y+1),
    ]
}

fn process_day(tiles: HashMap<(i32, i32), i32>) -> HashMap<(i32, i32), i32> {
    let mut ret = HashMap::new();

    ret
}

fn art_exhibit(lines: &Vec<String>, days: usize) -> usize {
    let mut tiles: HashMap<(i32, i32), i32> = lines
        .iter()
        .fold(HashMap::new(), |mut acc, line| {
            let (x, y) = process_steps(line);
            acc.entry((x, y)).and_modify(|e| { *e *= -1 }).or_insert(1);
            acc
        });

    for _ in 0..days {
        tiles = process_day(tiles);
    }

    tiles.values().filter(|&v| *v == 1).count()
}

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/input.txt")?;

    println!("Day 24 / Part 1: {}", black_tiles(&lines));
    println!("Day 24 / Part 2: {}", art_exhibit(&lines, 100));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_steps() {
        assert_eq!((3, -1), process_steps(&"ese".to_string()));
        assert_eq!((0, 0), process_steps(&"esewnw".to_string()));
    }

    #[test]
    fn test_black_tiles() {
        assert_eq!(
            10,
            black_tiles(
                &vec![
                    "sesenwnenenewseeswwswswwnenewsewsw".to_string(),
                    "neeenesenwnwwswnenewnwwsewnenwseswesw".to_string(),
                    "seswneswswsenwwnwse".to_string(),
                    "nwnwneseeswswnenewneswwnewseswneseene".to_string(),
                    "swweswneswnenwsewnwneneseenw".to_string(),
                    "eesenwseswswnenwswnwnwsewwnwsene".to_string(),
                    "sewnenenenesenwsewnenwwwse".to_string(),
                    "wenwwweseeeweswwwnwwe".to_string(),
                    "wsweesenenewnwwnwsenewsenwwsesesenwne".to_string(),
                    "neeswseenwwswnwswswnw".to_string(),
                    "nenwswwsewswnenenewsenwsenwnesesenew".to_string(),
                    "enewnwewneswsewnwswenweswnenwsenwsw".to_string(),
                    "sweneswneswneneenwnewenewwneswswnese".to_string(),
                    "swwesenesewenwneswnwwneseswwne".to_string(),
                    "enesenwswwswneneswsenwnewswseenwsese".to_string(),
                    "wnwnesenesenenwwnenwsewesewsesesew".to_string(),
                    "nenewswnwewswnenesenwnesewesw".to_string(),
                    "eneswnwswnwsenenwnwnwwseeswneewsenese".to_string(),
                    "neswnwewnwnwseenwseesewsenwsweewe".to_string(),
                    "wseweeenwnesenwwwswnew".to_string(),
                ]
            ),
        );
    }

    //#[test]
    //fn test_art_exhibit() {
    //    assert_eq!(
    //        15,
    //        art_exhibit(
    //            &vec![
    //                "sesenwnenenewseeswwswswwnenewsewsw".to_string(),
    //                "neeenesenwnwwswnenewnwwsewnenwseswesw".to_string(),
    //                "seswneswswsenwwnwse".to_string(),
    //                "nwnwneseeswswnenewneswwnewseswneseene".to_string(),
    //                "swweswneswnenwsewnwneneseenw".to_string(),
    //                "eesenwseswswnenwswnwnwsewwnwsene".to_string(),
    //                "sewnenenenesenwsewnenwwwse".to_string(),
    //                "wenwwweseeeweswwwnwwe".to_string(),
    //                "wsweesenenewnwwnwsenewsenwwsesesenwne".to_string(),
    //                "neeswseenwwswnwswswnw".to_string(),
    //                "nenwswwsewswnenenewsenwsenwnesesenew".to_string(),
    //                "enewnwewneswsewnwswenweswnenwsenwsw".to_string(),
    //                "sweneswneswneneenwnewenewwneswswnese".to_string(),
    //                "swwesenesewenwneswnwwneseswwne".to_string(),
    //                "enesenwswwswneneswsenwnewswseenwsese".to_string(),
    //                "wnwnesenesenenwwnenwsewesewsesesew".to_string(),
    //                "nenewswnwewswnenesenwnesewesw".to_string(),
    //                "eneswnwswnwsenenwnwnwwseeswneewsenese".to_string(),
    //                "neswnwewnwnwseenwseesewsenwsweewe".to_string(),
    //                "wseweeenwnesenwwwswnew".to_string(),
    //            ],
    //            1,
    //        ),
    //    );
    //}
}
