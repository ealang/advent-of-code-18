from collections import namedtuple
from itertools import count

Cart = namedtuple('Cart', ('id', 'x', 'y', 'd', 'count'))

move_table = {
    'v': (0, 1),
    '^': (0, -1),
    '<': (-1, 0),
    '>': (1, 0),
}

turn_table_single = {
    ('^', '/'): '>',
    ('^', '\\'): '<',
    ('^', '|'): '^',

    ('v', '/'): '<',
    ('v', '\\'): '>',
    ('v', '|'): 'v',

    ('>', '/'): '^',
    ('>', '\\'): 'v',
    ('>', '-'): '>',

    ('<', '/'): 'v',
    ('<', '\\'): '^',
    ('<', '-'): '<',
}

turn_table_cross = {
    ('^', 0): '<',
    ('<', 0): 'v',
    ('v', 0): '>',
    ('>', 0): '^',

    ('^', 1): '^',
    ('<', 1): '<',
    ('v', 1): 'v',
    ('>', 1): '>',

    ('^', 2): '>',
    ('<', 2): '^',
    ('v', 2): '<',
    ('>', 2): 'v',
}

def next_cart_state(tracks, cart):
    dx, dy = move_table[cart.d]
    x = cart.x + dx
    y = cart.y + dy

    track = tracks[y][x]
    if track == '+':
        d = turn_table_cross[(cart.d, cart.count)]
        count = (cart.count + 1) % 3
        return cart._replace(x=x, y=y, d=d, count=count)

    d = turn_table_single[(cart.d, track)]
    return cart._replace(x=x, y=y, d=d)

def parse_input(string):
    carts = []
    ids = count()
    for y, row in enumerate(string.splitlines()):
        for x, cell in enumerate(row):
            if cell in {'<', '>', '^', 'v'}:
                carts.append(Cart(next(ids), x, y, d=cell, count=0))
    tracks = string.replace('>', '-') \
                   .replace('<', '-') \
                   .replace('^', '|') \
                   .replace('v', '|') \
                   .splitlines()
    return (tracks, carts)