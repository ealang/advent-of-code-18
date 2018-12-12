from collections import namedtuple
from itertools import count

Link = namedtuple('Link', ('left_i', 'right_i', 'value'))

class CircularList:
    '''A partially implemented circular linked list.'''

    def __init__(self):
        self._nodes = {}
        self._gen = count()
        self._cur_i = None

    def _insert_first(self, value):
        self._cur_i = next(self._gen)
        self._nodes[self._cur_i] = Link(self._cur_i, self._cur_i, value)

    @property
    def _left_i(self):
        return self._nodes[self._cur_i].left_i

    @property
    def _right_i(self):
        return self._nodes[self._cur_i].right_i

    def insert_right(self, value):
        if self._cur_i is None:
            self._insert_first(value)
        else:
            left_i = self._cur_i
            mid_i = next(self._gen)
            right_i = self._right_i
            self._nodes[mid_i] = Link(left_i, right_i, value)
            self._nodes[left_i] = self._nodes[left_i]._replace(right_i=mid_i)
            self._nodes[right_i] = self._nodes[right_i]._replace(left_i=mid_i)

    def move_right(self):
        self._cur_i = self._right_i

    def move_left(self):
        self._cur_i = self._left_i

    def delete_move_right(self):
        cur_i = self._cur_i
        left_i = self._left_i
        right_i = self._right_i
        self._nodes[left_i] = self._nodes[left_i]._replace(right_i=right_i)
        self._nodes[right_i] = self._nodes[right_i]._replace(left_i=left_i)
        self.move_right()
        return self._nodes.pop(cur_i).value
