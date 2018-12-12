class Node:
    # pylint: disable=too-few-public-methods
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

class CircularList:
    '''A partially implemented circular linked list.'''

    def __init__(self):
        self._cur = None

    def _insert_first(self, value):
        node = Node(value)
        node.left = node
        node.right = node
        self._cur = node

    @property
    def _right(self):
        return self._cur.right

    @property
    def _left(self):
        return self._cur.left

    def insert_right(self, value):
        if self._cur is None:
            self._insert_first(value)
        else:
            left = self._cur
            right = self._right
            mid = Node(value, left, right)
            left.right = mid
            right.left = mid

    def move_right(self):
        self._cur = self._cur.right

    def move_left(self):
        self._cur = self._cur.left

    def delete_move_right(self):
        left = self._left
        right = self._right
        left.right = right
        right.left = left
        value = self._cur.value
        self.move_right()
        return value
