from re import findall
from typing import List, NamedTuple

Pt = NamedTuple('Pt', (('x', int), ('y', int), ('z', int)))
Nanobot = NamedTuple('Nanobot', (('pt', Pt), ('r', int)))

def parse_input(input_str: str) -> List[Nanobot]:
    def parse_line(line):
        i = list(map(int, findall(r'-?\d+', line)))
        return Nanobot(Pt(*i[:3]), i[3])
    return [
        parse_line(line) for line in input_str.splitlines()
    ]

def mdist(a: Pt, b: Pt) -> int:
    return abs(a.x - b.x) + \
           abs(a.y - b.y) + \
           abs(a.z - b.z)

def part1(bots: List[Nanobot]) -> int:
    maxbot = max(bots, key=lambda b: b.r)
    return sum(
        1 for bot in bots
        if mdist(maxbot.pt, bot.pt) <= maxbot.r
    )

if __name__ == '__main__':
    with open('input.txt') as fp:
        bots = parse_input(fp.read())
    print(part1(bots)) # 599
