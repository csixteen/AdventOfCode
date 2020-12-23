#!/usr/bin/env python3

# https://adventofcode.com/2020/day/23
# Part 2

from __future__ import annotations
from typing import Dict


MAX_ID = 1000000
ROUNDS = 10000000


class ListNode:
    def __init__(self, val: int, n: ListNode=None, p: ListNode=None):
        self.val = val
        self.next = n
        self.prev = p

    def __str__(self) -> str:
        nodes = []
        tmp = self.next
        while self.val != tmp.val:
            nodes.append(tmp.prev.val)
            tmp = tmp.next

        nodes.append(tmp.prev.val)
        nodes.append(tmp.val) # Shows the tail pointing to the head

        return " -> ".join(map(str, nodes))


def play_game(head: ListNode, all_nodes: Dict[ListNode,int], r: int) -> int:
    curr = head

    for _ in range(r):
        # Pick up the next three nodes
        tmp1 = curr.next
        tmp3 = tmp1.next.next
        end = tmp3.next
        curr.next = end
        end.prev = curr

        tmp_vals = {tmp1.val, tmp1.next.val, tmp1.next.next.val}

        # Pick up the destination
        dest_id = curr.val-1
        if dest_id == 0:
            dest_id = MAX_ID

        while dest_id in tmp_vals:
            dest_id -= 1
            if dest_id == 0:
                dest_id = MAX_ID

        # We now need to fix all the next and prev pointers, so 
        # that at the end we have the following configuration:
        # dest_node -> tmp1 -> ... -> tmp3 -> (old dest_node next)
        dest_node = all_nodes[dest_id]
        tmp3.next = dest_node.next
        tmp1.prev = dest_node
        tmp3.next.prev = tmp3
        dest_node.next = tmp1

        curr = all_nodes[curr.val].next


    a, b = all_nodes[1].next.val, all_nodes[1].next.next.val

    print(f"{a} * {b} = {a*b}")


def main():
    initial_input = [2, 4, 7, 8, 1, 9, 3, 5, 6]

    all_nodes = {}
    head, tail, previous = None, None, None

    # Start by building the circular doubly-linked list
    for i in range(MAX_ID):
        if i < len(initial_input):
            val = initial_input[i]
        else:
            val = i + 1

        node = ListNode(val, p=previous)
        all_nodes[val] = node

        if previous:
            previous.next = node

        previous = node
        tail = node

        if not head:
            head = node

    head.prev = tail
    tail.next = head

    play_game(head, all_nodes, ROUNDS)


if __name__ == "__main__":
    main()
