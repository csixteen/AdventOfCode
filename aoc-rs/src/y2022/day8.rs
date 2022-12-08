use aoc::Solver;
use std::fmt;
use std::fmt::Formatter;

// A tree is a tuple with 5 elements, where the first
// element is its height and the other 4 elements are
// properties of the adjacent positions.
// (left, up, right, down).
//
//           Up (2)
// Left (1) Tree (0) Right (3)
//           Down (4)
//
// In part 1, the adjacent positions hold the height of
// the tallest tree that can be seen from that position.
// In part 1, the adjacent positions hold the number of
// trees that can be seen from that position.
#[derive(Debug, Eq, PartialEq)]
struct Tree(u16, u16, u16, u16, u16);

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}, {}, {}, {}, {})",
            self.0, self.1, self.2, self.3, self.4
        )
    }
}

impl Tree {
    // ----------------- Part 1 -------------------
    fn max_left(&self) -> u16 {
        self.0.max(self.1)
    }

    fn max_up(&self) -> u16 {
        self.0.max(self.2)
    }

    fn max_right(&self) -> u16 {
        self.0.max(self.3)
    }

    fn max_down(&self) -> u16 {
        self.0.max(self.4)
    }

    // A tree is visible if its height is greater than any of the highest
    // surrounding heights.
    fn is_visible(&self) -> bool {
        self.0 > self.1 || self.0 > self.2 || self.0 > self.3 || self.0 > self.4
    }

    // ----------------- Part 2 -------------------
    fn scenic_score(&self) -> u16 {
        self.1 * self.2 * self.3 * self.4
    }
}

// A forest is simply a grid of Tree structs.
type Forest = Vec<Vec<Tree>>;

pub struct Solution;

impl Solution {
    fn build_forest(input: &Vec<&str>) -> Forest {
        input
            .iter()
            .fold(Vec::with_capacity(input.len()), |mut acc, &line| {
                acc.push(
                    line.chars()
                        .map(|c| Tree(c.to_digit(10).unwrap() as u16, 0, 0, 0, 0))
                        .collect(),
                );
                acc
            })
    }

    fn build_forest_part1(input: &Vec<&str>) -> Forest {
        let mut forest: Forest = Solution::build_forest(input);

        let (rows, cols) = (forest.len(), forest[0].len());
        for row in 0..rows {
            for col in 0..cols {
                if row > 0 {
                    forest[row][col].2 = forest[row - 1][col].max_up();
                }
                if col > 0 {
                    forest[row][col].1 = forest[row][col - 1].max_left();
                }
            }
        }

        for row in (0..rows).rev() {
            for col in (0..cols).rev() {
                if row < rows - 1 {
                    forest[row][col].4 = forest[row + 1][col].max_down();
                }
                if col < cols - 1 {
                    forest[row][col].3 = forest[row][col + 1].max_right();
                }
            }
        }

        forest
    }

    fn build_forest_part2(input: &Vec<&str>) -> Forest {
        todo!()
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        let forest = Solution::build_forest_part1(input);
        let (rows, cols) = (forest.len(), forest[0].len());
        let base = cols * 2 + (rows - 2) * 2;
        (base
            + forest
                .iter()
                .skip(1)
                .take(rows - 2)
                .map(|row| {
                    row.iter()
                        .skip(1)
                        .take(cols - 2)
                        .filter(|&t| t.is_visible())
                        .count()
                })
                .sum::<usize>())
        .to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        let forest = Solution::build_forest_part2(input);
        forest
            .iter()
            .map(|row| row.iter().map(|tree| tree.scenic_score()).max().unwrap())
            .max()
            .unwrap()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_forest_part1() {
        assert_eq!(
            vec![
                vec![
                    Tree(3, 0, 0, 3, 6),
                    Tree(0, 3, 0, 3, 5),
                    Tree(3, 3, 0, 0, 5)
                ],
                vec![
                    Tree(2, 0, 3, 5, 6),
                    Tree(5, 2, 0, 5, 5),
                    Tree(5, 5, 3, 0, 3)
                ],
                vec![
                    Tree(6, 0, 3, 5, 0),
                    Tree(5, 6, 5, 3, 0),
                    Tree(3, 6, 5, 0, 0),
                ]
            ],
            Solution::build_forest_part1(&vec!["303", "255", "653"])
        )
    }

    #[test]
    fn part1() {
        let solver = Solution;
        assert_eq!(
            "21",
            solver.part1(&vec!["30373", "25512", "65332", "33549", "35390"])
        );
    }

    #[test]
    fn test_build_forest_part2() {
        assert_eq!(
            vec![
                vec![
                    Tree(3, 0, 0, 2, 2),
                    Tree(0, 1, 0, 1, 1),
                    Tree(3, 2, 0, 0, 1)
                ],
                vec![
                    Tree(2, 0, 1, 1, 1),
                    Tree(5, 1, 1, 1, 1),
                    Tree(5, 1, 1, 0, 1)
                ],
                vec![
                    Tree(6, 0, 2, 2, 0),
                    Tree(5, 1, 1, 1, 0),
                    Tree(3, 1, 1, 0, 0),
                ]
            ],
            Solution::build_forest_part2(&vec!["303", "255", "653"])
        )
    }

    #[test]
    fn part2() {
        let solver = Solution;
        assert_eq!(
            "8",
            solver.part2(&vec!["30373", "25512", "65332", "33549", "35390"])
        );
    }
}
