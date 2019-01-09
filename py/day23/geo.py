from typing import List, NamedTuple
from common import Octree, Pt3

Nanobot = NamedTuple('Nanobot', [('pt', Pt3), ('r', int)])

def mdist(a: Pt3, b: Pt3) -> int:
    return abs(a.x - b.x) + \
           abs(a.y - b.y) + \
           abs(a.z - b.z)

def contains_pt(pt: Pt3, bot: Nanobot) -> bool:
    return mdist(bot.pt, pt) <= bot.r

def bbox(bots: List[Nanobot]) -> Octree:
    xvals = [bot.pt.x for bot in bots]
    yvals = [bot.pt.y for bot in bots]
    zvals = [bot.pt.z for bot in bots]
    lb = Pt3(min(xvals), min(yvals), min(zvals))
    ub = Pt3(max(xvals), max(yvals), max(zvals))
    return Octree(lb, ub)

def in_bbox(octree: Octree, bot: Nanobot) -> bool:
    def dist(val, lower, upper):
        if lower <= val <= upper:
            return 0
        return min(
            abs(val - lower),
            abs(val - upper)
        )

    return sum(
        dist(p, lower, upper)
        for p, (lower, upper) in zip(bot.pt, octree.bounds)
    ) <= bot.r
