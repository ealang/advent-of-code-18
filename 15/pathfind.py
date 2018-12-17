from collections import deque

PATH_IMPASSE = 0
PATH_PASSABLE = 1
PATH_TARGET = 2

def positions_around(pos):
    x, y = pos
    yield (x, y - 1)
    yield (x - 1, y)
    yield (x + 1, y)
    yield (x, y + 1)

def pathfind_to_targets(pos, cellval):
    '''
    Pathfind from a given position.

    @param pos - start (x, y) position
    @param cellval - function that provides value at cell: PATH_IMPASSE,
                     PATH_PASSABLE, or PATH_TARGET
    @return - list of reachable targets (distance, first step pos, end pos)
    '''
    to_visit = deque([(0, None, pos)])
    breadcrums = {}
    targets = []

    def first_move(pos):
        prev = None
        while breadcrums[pos]:
            prev = pos
            pos = breadcrums[pos]
        return prev

    while to_visit:
        dist, prevpos, pos = to_visit.pop()
        if pos not in breadcrums:
            breadcrums[pos] = prevpos

            cell = cellval(pos)
            if cell == PATH_TARGET:
                targets.append((dist, first_move(pos), pos))
            elif cell == PATH_PASSABLE:
                to_visit.extendleft((dist + 1, pos, npos) for npos in positions_around(pos))

    return targets

def pathfind_from_targets(start, targets, cellval):
    '''
    Pathfind from targets to player.

    @param pos - start (x, y) position
    @param targets - targets to start search from
    @param cellval - function that provides value at cell: PATH_IMPASSE, PATH_PASSABLE
    @return - list of reachable targets (distance, first step pos)
    '''
    distmap = {}
    to_visit = deque((0, pos) for pos in targets)

    while to_visit:
        dist, pos = to_visit.pop()
        if pos not in distmap:
            distmap[pos] = dist
            to_visit.extendleft(
                (dist + 1, npos)
                for npos in positions_around(pos)
                if cellval(npos) == PATH_PASSABLE
            )

    return [
        (distmap[pos], pos)
        for pos in positions_around(start)
        if pos in distmap
    ]
