from typing import NamedTuple, List, Iterable, Set

Pt = NamedTuple('Pt', [('x', int), ('y', int), ('z', int), ('t', int)])

def parse(filename: str) -> List[Pt]:
    with open(filename) as fp:
        return [
            Pt(*map(int, line.split(',')))
            for line in fp.readlines()
        ]

def mdist(pt1: Pt, pt2: Pt) -> int:
    return abs(pt1.x - pt2.x) + \
           abs(pt1.y - pt2.y) + \
           abs(pt1.z - pt2.z) + \
           abs(pt1.t - pt2.t)

def points_within(dist: int, pt: Pt, pts: Iterable[Pt]) -> Iterable[Pt]:
    for other in pts:
        if other != pt and mdist(pt, other) <= dist:
            yield other

def part1(pts: List[Pt]) -> int:
    count = 0
    found: Set[Pt] = set()
    for starting_pt in pts:
        if starting_pt not in found:
            count += 1
            to_visit = {starting_pt}
            while to_visit:
                pt = to_visit.pop()
                found.add(pt)
                to_visit.update(set(points_within(3, pt, pts)) - found)
    return count

if __name__ == '__main__':
    print(part1(parse('day25/input.txt')))
