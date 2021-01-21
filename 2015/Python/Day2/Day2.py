#!/usr/bin/env python3
# https://adventofcode.com/2015/day/2

from typing import Iterator
import sys


def part1(boxes: Iterator[str]) -> int:
    total = 0

    for box in boxes:
        dim = sorted(list(map(int, box.split("x"))))
        total += (2*dim[0]*dim[1] + 2*dim[1]*dim[2] + 2*dim[0]*dim[2]) + dim[0]*dim[1]

    return total


def part2(boxes: Iterator[str]) -> int:
    total = 0

    for box in boxes:
        dim = sorted(list(map(int, box.split("x"))))
        total += (2*dim[0] + 2*dim[1]) + dim[0]*dim[1]*dim[2]

    return total


if __name__ == "__main__":
    boxes = sys.stdin.readlines()

    print("Day 2 / Part 1:", part1(boxes))
    print("Day 2 / Part 2:", part2(boxes))
