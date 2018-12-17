from collections import namedtuple
from itertools import count
from pathfind import pathfind_from_targets, PATH_IMPASSE, PATH_PASSABLE

Player = namedtuple('Player', ('id', 'team', 'x', 'y', 'hp'))

CELL_GOBLIN = 'G'
CELL_ELF = 'E'
CELL_EMPTY = '.'
CELL_WALL = '#'
ATTACK_POWER = 3
STARTING_HP = 200

def render_str_game(level, players):
    pos = {(p.x, p.y): p for p in players}
    def rendercell(x, y, cell):
        if (x, y) in pos:
            return pos[(x, y)].team
        return cell
    return '\n'.join(
        ''.join(rendercell(x, y, cell) for x, cell in enumerate(row))
        for y, row in enumerate(level)
    )

def parse_input(string):
    level = []
    players = []
    ids = count()
    for y, row in enumerate(string.splitlines()):
        for x, cell in enumerate(row):
            if cell in {CELL_GOBLIN, CELL_ELF}:
                players.append(Player(next(ids), cell, x, y, STARTING_HP))
        level.append(row.replace(CELL_GOBLIN, CELL_EMPTY) \
                        .replace(CELL_ELF, CELL_EMPTY))
    return level, players

def move_step(level, players, player_id):
    player = players[player_id]
    team = player.team
    playerpos = (player.x, player.y)
    alies_pos = {(p.x, p.y) for p in players.values() if p.team == team}
    enemies_pos = {(p.x, p.y) for p in players.values() if p.team != team}

    def next_to_enemy(pos):
        return any((p in enemies_pos) for p in positions_around(pos[0], pos[1]))

    def cellval(pos):
        x, y = pos
        if level[y][x] == CELL_WALL or pos in alies_pos:
            return PATH_IMPASSE
        return PATH_PASSABLE

    def pickorder(target):
        (dist, (sx, sy)) = target
        return (dist, sy, sx)

    if not next_to_enemy(playerpos):
        targets = pathfind_from_targets(
            playerpos,
            [(p.x, p.y) for p in players.values() if p.team != team],
            cellval
        )
        if targets:
            (_, move) = min(targets, key=pickorder)
            return move
    return None

def positions_around(x, y):
    yield (x, y - 1)
    yield (x - 1, y)
    yield (x + 1, y)
    yield (x, y + 1)

def attack_step(players, player_id):
    player = players[player_id]
    target_pos = {(p.x, p.y): p for p in players.values() if p.team != player.team}
    order_by_hp = lambda p: (p.hp, p.y, p.x)
    can_hit_opponents = [
        target_pos[p]
        for p in positions_around(player.x, player.y)
        if p in target_pos
    ]
    if can_hit_opponents:
        opponent = min(can_hit_opponents, key=order_by_hp)
        return (opponent.x, opponent.y)
    return None

def game_finished(players):
    return len({p.team for p in players.values()}) == 1

def run_game(level, init_players, show=False):
    players = {p.id: p for p in init_players}
    occupied = {(p.x, p.y): p.id for p in init_players}
    turnorder = lambda i: (players[i].y, players[i].x)
    completed_rounds = 0

    while not game_finished(players):
        round_aborted = False

        if show:
            print(render_str_game(level, players.values()))

        for i in list(sorted(players.keys(), key=turnorder)):
            if i in players:
                if game_finished(players):
                    round_aborted = True
                    break

                p = players[i]
                move = move_step(level, players, i)
                if move:
                    old_pos = (p.x, p.y)
                    new_pos = (move[0], move[1])
                    p = p._replace(x=new_pos[0], y=new_pos[1])
                    del occupied[old_pos]
                    occupied[new_pos] = i
                    players[i] = p

                attack = attack_step(players, i)
                if attack:
                    attack_pos = (attack[0], attack[1])
                    attacked_i = occupied[attack_pos]
                    attacked_pl = players[attacked_i]
                    attacked_pl = attacked_pl._replace(hp=attacked_pl.hp - ATTACK_POWER)
                    players[attacked_i] = attacked_pl
                    if attacked_pl.hp <= 0:
                        del players[attacked_i]
                        del occupied[attack_pos]
        if not round_aborted:
            completed_rounds += 1

    if show:
        print(render_str_game(level, players.values()))

    return (completed_rounds, [p.hp for p in players.values()])

def part1(filename, show=False):
    with open(filename) as fp:
        level, players = parse_input(fp.read())
    completed, hp = run_game(level, players, show)
    return completed * sum(hp)

if __name__ == '__main__':
    assert part1('example0.txt') == 27730
    assert part1('example1.txt') == 36334
    assert part1('example2.txt') == 39514
    assert part1('example3.txt') == 27755
    assert part1('example4.txt') == 28944
    assert part1('example5.txt') == 18740
    print(part1('day15.txt', show=True))
