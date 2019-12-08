from collections import defaultdict

def get_input():
    with open('../data/6.txt', 'r') as f:
        return [tuple(line.strip().split(')')) for line in f]

def get_orbits():
    return { b : a for a, b in get_input() }

def get_depth(orbits, key):
    return len(get_ancestors(orbits, key))

def get_ancestors(orbits, key):
    ancestors = []
    while key in orbits:
        key = orbits[key]
        ancestors.append(key)
    return ancestors

def get_total_orbits(orbits):
    return sum(get_depth(orbits, x) for x in orbits.keys())

def get_distance(orbits, a, b):
    an_a, an_b = get_ancestors(orbits, a), get_ancestors(orbits, b)
    common_an = next(x for x in an_a if x in an_b)
    return an_a.index(common_an) + an_b.index(common_an)

orbits = get_orbits()
print(get_total_orbits(orbits))
print(get_distance(orbits, 'YOU', 'SAN'))
