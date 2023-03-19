use std::collections::HashMap;

use itertools::Itertools;

use aoc::img::tile::Tile;
use aoc::Solver;

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let raw_tiles = input
            .iter()
            .map(|&l| l.to_owned())
            .group_by(|line| !line.is_empty());
        let grouped: Vec<Vec<String>> = raw_tiles
            .into_iter()
            .map(|(_, group)| group.collect::<Vec<String>>())
            .filter(|g| !g[0].is_empty())
            .collect();

        // Build HashSet with Tiles
        let tiles: HashMap<i32, Tile> = grouped.iter().fold(HashMap::new(), |mut acc, t| {
            let tile = Tile::new(t);
            acc.insert(tile.id, tile);
            acc
        });

        // Builds a HashMap that maps the edges to all the Tiles
        // that use them.
        let mut edges: HashMap<String, Vec<&Tile>> = HashMap::new();

        for tile in tiles.values() {
            for e in tile.all_edges() {
                edges.entry(e).or_insert(Vec::new()).push(tile);
            }
        }

        // Finds the corners, which are the Tiles that only have two
        // edges shared with other Tiles.
        let mut corners: Vec<&Tile> = Vec::with_capacity(4);

        for t in tiles.values() {
            let c = t
                .edges()
                .iter()
                .fold(0, |acc, e| acc + (edges.get(e).unwrap().len() - 1));

            if c == 2 {
                corners.push(t);
            }
        }

        // Calculates the product of the four corners
        let p = corners
            .iter()
            .map(|t| t.id)
            .fold(1_i64, |acc, id| acc * (id as i64));

        p.to_string()
    }

    fn part2(&self, _input: &[&str]) -> String {
        todo!()
    }
}
