import re
from copy import deepcopy
from dataclasses import dataclass, replace
from functools import partial
from itertools import chain
from typing import Callable, List, Set, Mapping, Tuple, Dict, Optional

@dataclass
class Group:
    initiative: int
    attack_damage: int
    attack_type: str
    starting_hp: int
    starting_units: int
    immunities: Set[str]
    weaknesses: Set[str]
    units_lost: int

    def take_damage(self, damage: int) -> None:
        self.units_lost = min(
            self.units_lost + damage // self.starting_hp,
            self.starting_units
        )

    @property
    def effective_power(self) -> int:
        return self.units_remaining * self.attack_damage

    @property
    def units_remaining(self) -> int:
        return self.starting_units - self.units_lost

    @property
    def alive(self) -> bool:
        return self.units_remaining > 0

@dataclass
class Army:
    groups: List[Group]

    def __getitem__(self, i: int) -> Group:
        return self.groups[i]

    def with_boost(self, boost: int) -> 'Army':
        return replace(self, groups=[
            replace(group, attack_damage=group.attack_damage + boost)
            for group in self.groups
        ])

    @property
    def alive(self) -> bool:
        return any(group.alive for group in self.groups)

    @property
    def units_remaining(self) -> int:
        return sum(group.units_remaining for group in self.groups)

def parse_army(string: str) -> Army:
    def parse_group(group: str) -> Group:
        parse_list = lambda s: [s.strip() for s in s.split(',')]
        units = re.search(r'(\d+) units', group)
        hp = re.search(r'(\d+) hit points', group)
        attack = re.search(r'attack that does (\d+) (\w+) damage', group)
        initiative = re.search(r'initiative (\d+)', group)
        immunities = re.search(r'immune to ([\w ,]+)', group)
        weaknesses = re.search(r'weak to ([\w ,]+)', group)
        if units and hp and attack and initiative:
            return Group(
                initiative=int(initiative.group(1)),
                attack_damage=int(attack.group(1)),
                attack_type=attack.group(2),
                starting_hp=int(hp.group(1)),
                starting_units=int(units.group(1)),
                immunities=set(parse_list(immunities.group(1)) if immunities else []),
                weaknesses=set(parse_list(weaknesses.group(1)) if weaknesses else []),
                units_lost=0,
            )
        raise Exception(group)

    return Army([
        parse_group(group)
        for group in string.splitlines()
    ])

def load_army(filename: str) -> Army:
    with open(filename) as fp:
        return parse_army(fp.read())

def damage_done(attacker: Group, defender: Group) -> int:
    if attacker.attack_type in defender.immunities:
        return 0
    if attacker.attack_type in defender.weaknesses:
        return attacker.effective_power * 2
    return attacker.effective_power

def plan_attack(attackers: Army, defenders: Army) -> Mapping[int, int]:
    '''Return attacker to defender mapping.'''
    def turn_weight(attacker: Group) -> Tuple[int, int]:
        return (attacker.effective_power, attacker.initiative)

    def attack_weight(attacker_i: int, defender_i: int) -> Tuple[int, int, int]:
        attacker = attackers[attacker_i]
        defender = defenders[defender_i]
        return (
            damage_done(attacker, defender),
            defender.effective_power,
            defender.initiative
        )

    defenders_avail = {
        i
        for i, group in enumerate(defenders.groups)
        if group.alive
    }

    turn_order = sorted(
        (i for i, attacker in enumerate(attackers.groups) if attacker.alive),
        key=lambda attacker_i: turn_weight(attackers[attacker_i]),
        reverse=True
    )

    plan: Dict[int, int] = {}
    for attacker_i in turn_order:
        if defenders_avail:
            defender_i = max(
                defenders_avail,
                key=partial(attack_weight, attacker_i)
            )
            damage = damage_done(attackers[attacker_i], defenders[defender_i])
            if damage:
                plan[attacker_i] = defender_i
                defenders_avail.discard(defender_i)
        else:
            break
    return plan

def part1(army1: Army, army2: Army) -> Tuple[int, int]:
    '''Simulate a battle.'''
    def label(l, it):
        for item in it:
            yield (l, item)

    def unit_counts():
        return (army1.units_remaining, army2.units_remaining)

    army1 = deepcopy(army1)
    army2 = deepcopy(army2)
    armies = [army1, army2]
    while all(army.alive for army in armies):
        attacks = sorted(
            (
                (
                    armies[army_i][attacker_i],
                    armies[(army_i + 1) % 2][defender_i],
                )
                for army_i, (attacker_i, defender_i)
                in chain(
                    label(0, plan_attack(army1, army2).items()),
                    label(1, plan_attack(army2, army1).items()),
                )
            ),
            key=lambda a_d: a_d[0].initiative,
            reverse=True
        )

        old_counts = unit_counts()
        for attacker, defender in attacks:
            defender.take_damage(
                damage_done(attacker, defender)
            )
        if old_counts == unit_counts():
            return (-1, 0)  # tie

    if army1.alive:
        return (0, army1.units_remaining)
    return (1, army2.units_remaining)

def part2(immune: Army, infection: Army) -> Optional[int]:
    def wins_with_boost(boost):
        winner, _ = part1(immune.with_boost(boost), infection)
        return winner == 0

    def binsearch(lo: int, hi: int, p: Callable[[int], bool]) -> Optional[int]:
        if lo >= hi:
            return None

        m = (lo + hi) // 2
        if p(m):
            result = binsearch(lo, m, p)
            if result is None:
                return m
            return min(result, m)
        return binsearch(m + 1, hi, p)

    winning_boost = binsearch(
        0, 10000, wins_with_boost
    )
    if winning_boost is not None:
        _, units = part1(immune.with_boost(winning_boost), infection)
        return units
    return None

def main() -> None:
    immune = load_army('day24/input-immune-system.txt')
    infection = load_army('day24/input-infection.txt')
    print(part1(immune, infection)) # 9878
    print(part2(immune, infection)) # 10954

main()
