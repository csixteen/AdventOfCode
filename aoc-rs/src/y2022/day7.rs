use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::str::FromStr;

use aoc::Solver;

#[derive(Debug)]
struct Node {
    is_dir: bool,
    size: usize,
    parent: Option<RefCell<Weak<Node>>>,
    children: RefCell<HashMap<String, Rc<Node>>>,
}

impl Node {
    fn new_node(size: usize, is_dir: bool, parent: Option<Weak<Node>>) -> Node {
        Node {
            is_dir,
            size,
            parent: parent.map(RefCell::new),
            children: RefCell::new(HashMap::new()),
        }
    }

    fn new_dir(parent: Option<Weak<Node>>) -> Node {
        Node::new_node(0, true, parent)
    }

    fn add_node(self: &Rc<Self>, name: &str, size: usize, is_dir: bool) -> Rc<Node> {
        let parent = Some(Rc::downgrade(self));
        let node = Rc::new(Node::new_node(size, is_dir, parent));
        self.children
            .borrow_mut()
            .insert(name.to_string(), node.clone());
        node
    }

    fn add_file(self: &Rc<Self>, name: &str, size: usize) -> Rc<Node> {
        self.add_node(name, size, false)
    }

    fn add_dir(self: &Rc<Self>, name: &str) -> Rc<Node> {
        self.add_node(name, 0, true)
    }

    fn total_size(&self) -> usize {
        self.size
            + self
                .children
                .borrow()
                .values()
                .map(|child| child.total_size())
                .sum::<usize>()
    }

    fn cd(self: &Rc<Self>, dir: &str) -> Rc<Node> {
        if dir == ".." {
            self.parent.as_ref().unwrap().borrow().upgrade().unwrap()
        } else {
            match self.children.borrow().get(dir) {
                None => self.add_dir(dir),
                Some(node) => node.clone(),
            }
        }
    }

    fn dir_sizes(&self) -> Vec<usize> {
        let curr = if self.is_dir {
            vec![self.total_size()]
        } else {
            Vec::new()
        };
        [
            curr,
            self.children
                .borrow()
                .values()
                .flat_map(|child| child.dir_sizes())
                .collect(),
        ]
        .concat()
    }
}

pub struct Solution;

impl Solution {
    fn build_tree(input: &[&str]) -> Rc<Node> {
        let root = Rc::new(Node::new_dir(None));
        let mut curr = root.clone();
        for &line in input.iter().skip(1) {
            let parts: Vec<_> = line.split(' ').collect();
            if parts[0] == "$" && parts[1] == "ls" {
                continue;
            } else if parts[0] == "$" && parts[1] == "cd" {
                curr = curr.cd(parts[2]);
            } else if parts[0] == "dir" {
                curr.add_dir(parts[1]);
            } else {
                curr.add_file(parts[1], usize::from_str(parts[0]).unwrap());
            }
        }
        root
    }
}

impl Solver for Solution {
    fn part1(&self, input: &Vec<&str>) -> String {
        let root = Solution::build_tree(input);
        root.dir_sizes()
            .iter()
            .filter(|&size| *size < 100000)
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, input: &Vec<&str>) -> String {
        let root = Solution::build_tree(input);
        let target_size = 30000000 - (70000000 - root.total_size());
        let mut min = usize::MAX;
        for size in root.dir_sizes() {
            if size >= target_size {
                min = min.min(size);
            }
        }

        min.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_total_size_empty_dir() {
        let dir = Node::new_dir(None);
        assert_eq!(0, dir.total_size());
    }

    #[test]
    fn test_total_size_dir_with_files() {
        let dir = Rc::new(Node::new_dir(None));
        dir.add_file("foobar.txt", 123);
        assert_eq!(123, dir.total_size());
    }

    #[test]
    fn test_total_size_dir_with_subdirs() {
        let dir = Rc::new(Node::new_dir(None));
        let dir_a = dir.add_dir("a");
        dir_a.add_file("foobar.txt", 123);
        assert_eq!(123, dir.total_size());
    }

    #[test]
    fn part1() {
        let solver = Solution;
        assert_eq!(
            "95437",
            solver.part1(&vec![
                "$ cd /",
                "$ ls",
                "dir a",
                "14848514 b.txt",
                "8504156 c.dat",
                "dir d",
                "$ cd a",
                "$ ls",
                "dir e",
                "29116 f",
                "2557 g",
                "62596 h.lst",
                "$ cd e",
                "$ ls",
                "584 i",
                "$ cd ..",
                "$ cd ..",
                "$ cd d",
                "$ ls",
                "4060174 j",
                "8033020 d.log",
                "5626152 d.ext",
                "7214296 k",
            ])
        );
    }
}
