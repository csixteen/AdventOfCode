use std::collections::HashSet;

use aoc::Solver;

type Tiles = HashSet<(i32, i32)>;

fn process_steps(line: &str) -> (i32, i32) {
    let len = line.len();
    let (mut x, mut y) = (0, 0);
    let mut i = 0;

    while i < len {
        match line.chars().nth(i) {
            Some('e') => x += 2,
            Some('w') => x -= 2,
            Some('n') => {
                y += 1;
                i += 1;
                x += if line.chars().nth(i) == Some('e') {
                    1
                } else {
                    -1
                };
            }
            Some('s') => {
                y -= 1;
                i += 1;
                x += if line.chars().nth(i) == Some('e') {
                    1
                } else {
                    -1
                };
            }
            _ => panic!("Unknown direction"),
        }

        i += 1;
    }

    (x, y)
}

fn black_tiles(lines: &[&str]) -> Tiles {
    lines.iter().fold(HashSet::new(), |mut acc, &line| {
        let (x, y) = process_steps(line);
        if !acc.remove(&(x, y)) {
            acc.insert((x, y));
        }
        acc
    })
}

fn neighbours(x: i32, y: i32) -> Vec<(i32, i32)> {
    vec![
        (x - 1, y + 1),
        (x - 2, y),
        (x - 1, y - 1),
        (x + 1, y - 1),
        (x + 2, y),
        (x + 1, y + 1),
    ]
}

fn process_day(tiles: &Tiles) -> Tiles {
    let mut ret: Tiles = HashSet::new();
    let to_process: Tiles = tiles.iter().fold(HashSet::new(), |mut acc, &(x, y)| {
        acc.insert((x, y));
        neighbours(x, y).iter().for_each(|&(x1, y1)| {
            acc.insert((x1, y1));
        });
        acc
    });

    for &(x, y) in to_process.iter() {
        let c = neighbours(x, y)
            .iter()
            .filter(|&(x1, y1)| tiles.contains(&(*x1, *y1)))
            .count();

        match tiles.contains(&(x, y)) {
            true => {
                if c > 0 && c < 3 {
                    ret.insert((x, y));
                }
            }
            false => {
                if c == 2 {
                    ret.insert((x, y));
                }
            }
        }
    }

    ret
}

fn art_exhibit(lines: &[&str], days: usize) -> usize {
    (0..days)
        .fold(black_tiles(lines), |acc, _| process_day(&acc))
        .len()
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        black_tiles(input).len().to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        art_exhibit(input, 100).to_string()
    }
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
            black_tiles(&vec![
                "sesenwnenenewseeswwswswwnenewsewsw",
                "neeenesenwnwwswnenewnwwsewnenwseswesw",
                "seswneswswsenwwnwse",
                "nwnwneseeswswnenewneswwnewseswneseene",
                "swweswneswnenwsewnwneneseenw",
                "eesenwseswswnenwswnwnwsewwnwsene",
                "sewnenenenesenwsewnenwwwse",
                "wenwwweseeeweswwwnwwe",
                "wsweesenenewnwwnwsenewsenwwsesesenwne",
                "neeswseenwwswnwswswnw",
                "nenwswwsewswnenenewsenwsenwnesesenew",
                "enewnwewneswsewnwswenweswnenwsenwsw",
                "sweneswneswneneenwnewenewwneswswnese",
                "swwesenesewenwneswnwwneseswwne",
                "enesenwswwswneneswsenwnewswseenwsese",
                "wnwnesenesenenwwnenwsewesewsesesew",
                "nenewswnwewswnenesenwnesewesw",
                "eneswnwswnwsenenwnwnwwseeswneewsenese",
                "neswnwewnwnwseenwseesewsenwsweewe",
                "wseweeenwnesenwwwswnew",
            ])
            .len(),
        );
    }

    #[test]
    fn test_art_exhibit() {
        assert_eq!(
            2208,
            art_exhibit(
                &vec![
                    "sesenwnenenewseeswwswswwnenewsewsw",
                    "neeenesenwnwwswnenewnwwsewnenwseswesw",
                    "seswneswswsenwwnwse",
                    "nwnwneseeswswnenewneswwnewseswneseene",
                    "swweswneswnenwsewnwneneseenw",
                    "eesenwseswswnenwswnwnwsewwnwsene",
                    "sewnenenenesenwsewnenwwwse",
                    "wenwwweseeeweswwwnwwe",
                    "wsweesenenewnwwnwsenewsenwwsesesenwne",
                    "neeswseenwwswnwswswnw",
                    "nenwswwsewswnenenewsenwsenwnesesenew",
                    "enewnwewneswsewnwswenweswnenwsenwsw",
                    "sweneswneswneneenwnewenewwneswswnese",
                    "swwesenesewenwneswnwwneseswwne",
                    "enesenwswwswneneswsenwnewswseenwsese",
                    "wnwnesenesenenwwnenwsewesewsesesew",
                    "nenewswnwewswnenesenwnesewesw",
                    "eneswnwswnwsenenwnwnwwseeswneewsenese",
                    "neswnwewnwnwseenwseesewsenwsweewe",
                    "wseweeenwnesenwwwswnew",
                ],
                100,
            ),
        );
    }
}
