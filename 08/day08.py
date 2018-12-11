from itertools import islice

def meta_iter(data):
    def visit(it):
        nchildren = next(it)
        nmeta = next(it)

        for _ in range(nchildren):
            yield from visit(it)

        for meta in islice(it, nmeta):
            yield meta

    return visit(iter(data))

def node_value(data):
    def visit(it):
        nchildren = next(it)
        nmeta = next(it)

        children = [visit(it) for _ in range(nchildren)]
        meta = islice(it, nmeta)

        if nchildren == 0:
            return sum(meta)
        return sum(
            children[i - 1] if i <= nchildren else 0 for i in meta
        )

    return visit(iter(data))

def parse(filename):
    with open(filename) as fp:
        return [int(num) for num in fp.read().split()]

def part1(data):
    return sum(meta_iter(data))

def part2(data):
    return node_value(data)

if __name__ == '__main__':
    data = parse('day08.txt')
    print(part1(data))
    print(part2(data))
