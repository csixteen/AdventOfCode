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

// https://adventofcode.com/2020/day/20

#![allow(non_snake_case)]

use std::collections::HashMap;

use aoc::fs::get_file_contents;
use itertools::Itertools;

mod img;

use img::{tile::Tile,image::Image};

fn main() -> std::io::Result<()> {
    let lines = get_file_contents("data/sample.txt")?;
    let raw_tiles = lines.into_iter().group_by(|line| !line.is_empty());
    let grouped: Vec<Vec<String>> = raw_tiles
        .into_iter()
        .map(|(_, group)| group.collect::<Vec<String>>())
        .filter(|g| !g[0].is_empty())
        .collect();

    // Build HashSet with Tiles
    let tiles: HashMap<i32, Tile> = grouped
        .iter()
        .fold(HashMap::new(), |mut acc, t| {
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
        let c = t.edges().iter().fold(0, |acc, e| {
            acc + (edges.get(e).unwrap().len() - 1)
        });

        if c == 2 {
            corners.push(t);
        }
    }

    // Calculates the product of the four corners
    let p = corners.iter().map(|t| t.id).fold(1_i64, |acc, id| acc * (id as i64));

    println!("Day 20 / Part 1: {}", p);

    Ok(())
}
