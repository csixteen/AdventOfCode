#!/usr/bin/env python3
# https://adventofcode.com/2015/day/1

import sys


def part1(_input: str) -> int:
    ret = 0
    for c in _input:
        if c == "(":
            ret += 1
        else:
            ret -= 1

    return ret


def part2(_input: str) -> int:
    i, ret = 1, 0
    for c in _input:
        if c == "(":
            ret += 1
        else:
            ret -= 1

        if ret == -1:
            return i

        i += 1

    return i


if __name__ == "__main__":
    _input = sys.stdin.read().strip()

    print("Day 1 / Part 1:", part1(_input))
    print("Day 1 / Part 2:", part2(_input))
