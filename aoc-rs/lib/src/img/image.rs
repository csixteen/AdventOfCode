#![allow(non_snake_case)]
use std::collections::HashMap;

use crate::img::tile::{Tile, TileEdge};

type EdgeMap<'a> = HashMap<String, Vec<&'a Tile>>;

#[derive(Default, Debug)]
pub struct Image {
    tiles: Vec<Vec<Tile>>,
    N: usize,
}

impl Image {
    pub fn new(size: usize) -> Self {
        Image {
            N: size,
            ..Default::default()
        }
    }

    pub fn init(&mut self, corner: &Tile, edge_map: &EdgeMap) {
        let mut c = corner.clone();
        loop {
            let top_len = edge_map.get(&c.top_edge()).unwrap().len();
            let left_len = edge_map.get(&c.left_edge()).unwrap().len();

            if top_len == 1 && left_len == 1 {
                break;
            } else {
                c = c.rotate();
            }
        }

        self.tiles.push(vec![c]);
    }

    fn choose_tile(&self, pt: &Tile, pe: String, e: TileEdge, edge_map: &EdgeMap) -> Tile {
        let candidate = edge_map
            .get(&pe)
            .unwrap()
            .iter()
            .filter(|&t| t.id != pt.id)
            .nth(0)
            .unwrap();

        for c in candidate.transformations().take(9) {
            let ee = c.edge(e);
            if pe == ee {
                return c;
            }
        }

        unreachable!(); // Hopefully
    }

    /*
     * Places the tiles one at a time, by traversing all the rows from
     * top to the bottom, from left to right. The next tile is chosen
     * based on the right / bottom edges of the previous tile.
     */
    pub fn place_tiles(&mut self, edge_map: &EdgeMap) {
        for row in 0..self.N {
            /*
             * We won't need to initialize the leftmost cell if we're on
             * the first row, because we did it already on the `init`
             * method.
             */
            if row > 0 {
                let c = self.choose_tile(
                    &self.tiles[row - 1][0],
                    self.tiles[row - 1][0].bottom_edge(),
                    TileEdge::Top,
                    edge_map,
                );

                self.tiles.push(vec![c]);
            }

            // Leftmost column already handled either in `init` or
            // in the previous block.
            for col in 1..self.N {
                let c = self.choose_tile(
                    &self.tiles[row][col - 1],
                    self.tiles[row][col - 1].right_edge(),
                    TileEdge::Left,
                    edge_map,
                );

                self.tiles[row].push(c);
            }
        }
    }
}
