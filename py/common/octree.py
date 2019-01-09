from dataclasses import dataclass
from itertools import product
from typing import Iterable
from .pt3 import Pt3

@dataclass(frozen=True)
class Octree:
    lb: Pt3
    ub: Pt3

    @property
    def nonempty(self):
        return all(
            lower <= upper for lower, upper in
            zip(self.lb, self.ub)
        )

    @property
    def volume(self) -> int:
        return (self.ub.x + 1 - self.lb.x) * \
               (self.ub.y + 1 - self.lb.y) * \
               (self.ub.z + 1 - self.lb.z)

    @property
    def bounds(self):
        return zip(self.lb, self.ub)

    @property
    def octants(self) -> Iterable['Octree']:
        def bounds(low, lb, ub):
            return [
                lb, (lb + ub) // 2
            ] if low else [
                (lb + ub) // 2 + 1, ub
            ]

        for x, y, z in product((True, False), repeat=3):
            lb, ub = zip(
                bounds(x, self.lb.x, self.ub.x),
                bounds(y, self.lb.y, self.ub.y),
                bounds(z, self.lb.z, self.ub.z),
            )
            octant = Octree(
                Pt3(*lb),
                Pt3(*ub),
            )
            if octant.nonempty:
                yield octant
