from cartlib import next_cart_state, parse_input

def sim_steps(tracks, carts):
    cart_order = lambda cart: (cart.y, cart.x)
    while True:
        next_carts = []
        for old in sorted(carts, key=cart_order):
            new = next_cart_state(tracks, old)
            next_carts.append(new)
            yield (old, new)
        carts = next_carts

def first_crash_pos(tracks, carts):
    occupied = {(cart.x, cart.y) for cart in carts}
    for old, new in sim_steps(tracks, carts):
        old_pos = (old.x, old.y)
        new_pos = (new.x, new.y)
        if new_pos in occupied:
            return new_pos
        occupied.add(new_pos)
        occupied.remove(old_pos)

def find_survivor_pos(tracks, carts):
    surviving = {cart.id for cart in carts}
    occupied = {(cart.x, cart.y): cart.id for cart in carts}
    for old, new in sim_steps(tracks, carts):
        if new.id in surviving:
            old_pos = (old.x, old.y)
            new_pos = (new.x, new.y)
            if len(surviving) == 1:
                return old_pos

            collided_with = occupied.get(new_pos)
            if collided_with is not None:
                surviving.remove(new.id)
                surviving.remove(collided_with)
                del occupied[new_pos]
            else:
                occupied[new_pos] = new.id
            del occupied[old_pos]

if __name__ == '__main__':
    with open('input.txt') as fp:
        day13 = fp.read()
    tracks, carts = parse_input(day13)
    print(first_crash_pos(tracks, carts))
    print(find_survivor_pos(tracks, carts))
