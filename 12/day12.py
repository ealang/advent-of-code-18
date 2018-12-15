from node import LeafNode, InnerNode

def empty_tree(n):
    if n > 0:
        return InnerNode({char: empty_tree(n - 1) for char in ['.', '#']})
    return LeafNode()

def trace_path(node, path):
    if not path:
        return node
    return trace_path(node.edges[path[0]], path[1:])

def generate_rule_tree(mapping):
    tree = empty_tree(5)
    for path, result in mapping.items():
        leaf = trace_path(tree, path)
        leaf.value = result
        leaf.warp = trace_path(tree, path[1:])
    return tree

def apply_rules(rules, init_state):
    def apply(node, seq):
        if isinstance(node, LeafNode):
            yield node.value
            yield from apply(node.warp, seq)
        elif seq:
            yield from apply(node.edges[seq[0]], seq[1:])
    return '..' + ''.join(apply(rules, init_state)) + '..'

def part_1(rules, init_state):
    n = 20
    ends = '..' * n
    state = ends + init_state + ends
    for _ in range(n):
        state = apply_rules(rules, state)
        print(state)
    result = 0
    for i, pot in enumerate(state):
        if pot == '#':
            result += i - len(ends)
    return result

def parse_input(filename):
    with open(filename) as fp:
        init = fp.readline().split()[2]
        fp.readline()
        mapping = dict(map(str.strip, line.split(' => ')) for line in fp.readlines())
    return (init, mapping)

if __name__ == '__main__':
    init_state, mapping = parse_input('day12.txt')
    rules = generate_rule_tree(mapping)
    print(part_1(rules, init_state))
