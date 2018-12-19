import re
from collections import namedtuple

Pt = namedtuple('Pt', ('x', 'y'))

def parse_clay(filename):
    def pt_range(axis, p, s1, s2):
        return (
            Pt(p, s) if axis == 'x' else Pt(s, p)
            for s in range(s1, s2 + 1)
        )

    clay = set()
    with open(filename) as fp:
        for line in fp.readlines():
            axis = line[0]
            p, s1, s2 = map(int, re.findall(r'\d+', line))
            clay.update(pt_range(axis, p, s1, s2))
    return clay

def clay_bounds(clay):
    minx = min(pt.x for pt in clay)
    miny = min(pt.y for pt in clay)
    maxx = max(pt.x for pt in clay)
    maxy = max(pt.y for pt in clay)
    return [minx, miny, maxx, maxy]

def render_str(clay, visited, blocked):
    def char(pt):
        if pt in blocked:
            return '~'
        if pt in visited:
            return '|'
        if pt in clay:
            return '#'
        return ' '

    [minx, miny, maxx, maxy] = clay_bounds(clay)
    return '\n'.join(
        ''.join(
            char(Pt(x, y))
            for x in range(minx - 1, maxx + 2)
        )
        for y in range(miny - 1, maxy + 2)
    )

def reachable_from(pt, clay, blocked):
    left = Pt(pt.x - 1, pt.y)
    right = Pt(pt.x + 1, pt.y)
    down = Pt(pt.x, pt.y + 1)

    if down in clay or down in blocked:
        return {p for p in (left, right) if p not in clay}
    return {down}

def runwater(water_pt, clay):
    visited = set()
    flowing = set()
    blocked = set()
    to_visit = [water_pt]

    [_, _, _, ybound] = clay_bounds(clay)

    while to_visit:
        pt = to_visit.pop()
        visited.add(pt)

        if pt.y > ybound:
            flowing.add(pt)
            continue

        reachable = reachable_from(pt, clay, blocked)
        unvisited = reachable - visited
        if unvisited:
            to_visit.append(pt)
            to_visit.extend(unvisited)
        elif flowing & reachable:
            flowing.add(pt)
            blocked.discard(pt)
            to_visit.extend(reachable & blocked)
        else:
            blocked.add(pt)

    return visited, blocked

if __name__ == '__main__':
    water_pt = Pt(500, 0)
    clay = parse_clay('input.txt')

    visited, blocked = runwater(water_pt, clay)
    print(render_str(clay, visited, blocked))

    [_, miny, _, maxy] = clay_bounds(clay)
    print(sum(1 for p in visited if p.y >= miny and p.y <= maxy)) # 31471
    print(sum(1 for p in blocked if p.y >= miny and p.y <= maxy)) # 24169
