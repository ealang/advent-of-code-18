from re import findall
from typing import List, Tuple
from dataclasses import dataclass
from heapq import heappush, heappop

from common import Octree, Pt3
from .geo import Nanobot, bbox, mdist, contains_pt, in_bbox

def parse_input(input_str: str) -> List[Nanobot]:
    def parse_line(line):
        i = list(map(int, findall(r'-?\d+', line)))
        return Nanobot(Pt3(*i[:3]), i[3])
    return [
        parse_line(line) for line in input_str.splitlines()
    ]

def part1(bots: List[Nanobot]) -> int:
    maxbot = max(bots, key=lambda b: b.r)
    return sum(
        1 for bot in bots
        if contains_pt(bot.pt, maxbot)
    )

def part2(bots: List[Nanobot]) -> int:
    @dataclass(frozen=True)
    class HeapItem:
        octree: Octree
        n: int
        dist: int

        @property
        def key(self) -> Tuple[int, int]:
            return (self.n, -self.dist)

        def __lt__(self, other: 'HeapItem') -> bool:
            return self.key > other.key

    def push(vol: Octree) -> None:
        vol_bots = [bot for bot in bots if in_bbox(vol, bot)]
        if vol_bots:
            heappush(to_visit, HeapItem(
                vol,
                len(vol_bots),
                mdist(vol.ub, Pt3(0, 0, 0))
            ))

    to_visit: List[HeapItem] = []
    push(bbox(bots))
    while to_visit:
        item = heappop(to_visit)
        if item.octree.volume == 1:
            return item.dist
        for sub_bbox in item.octree.octants:
            push(sub_bbox)

    return -1

def main():

    with open('day23/input.txt') as fp:
        bots = parse_input(fp.read())
    print(part1(bots)) # 599
    print(part2(bots)) # 94481130
main()
