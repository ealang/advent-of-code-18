from abc import ABC

class Node(ABC):
    pass

class LeafNode(Node):
    def __init__(self, value=None, warp=None):
        self.value = value
        self.warp = warp

class InnerNode(Node):
    def __init__(self, edges):
        self.edges = edges
