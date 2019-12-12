from functools import reduce
from itertools import permutations
import re

moon_regex = re.compile('<x=([-\d]+), y=([-\d]+), z=([-\d]+)>')

class Moon:

    def __init__(self, pos):
        self.pos = list(pos)
        self.vel = [0 for _ in range(len(pos))]

    @property
    def potential_energy(self):
        return sum(map(abs, self.pos))

    @property
    def kinetic_energy(self):
        return sum(map(abs, self.vel))

    @property
    def total_energy(self):
        return self.potential_energy * self.kinetic_energy

def parse_moon(line):
    match = moon_regex.match(line)
    return Moon((int(match.group(1)), int(match.group(2)), int(match.group(3))))

def get_input():
    with open('../data/12.txt', 'r') as f:
        return [parse_moon(line) for line in f]

def update_velocities(moons):
    for m1, m2 in permutations(moons, 2):
        for idx in range(len(m1.pos)):
            if m1.pos[idx] < m2.pos[idx]:
                m1.vel[idx] += 1
            if m1.pos[idx] > m2.pos[idx]:
                m1.vel[idx] -= 1

def step(moons):
    update_velocities(moons)
    for moon in moons:
        for idx in range(len(moon.pos)):
            moon.pos[idx] += moon.vel[idx]

def flattened_period(moons, idx):
    seen = {}
    flattened_moons = [Moon([m.pos[idx]]) for m in moons]
    steps = 0
    while tuple((m.pos[0], m.vel[0]) for m in flattened_moons) not in seen:
        seen[tuple((m.pos[0], m.vel[0]) for m in flattened_moons)] = steps
        step(flattened_moons)
        steps += 1
    return steps

def gcd(x, y):
    while y != 0:
        x, y = y, x % y
    return x

def lcm(x, y):
    return x * y // gcd(x, y)

def part1():
    moons = get_input()
    for _ in range(1000):
        step(moons)
    return sum(moon.total_energy for moon in moons)

def part2():
    moons = get_input()
    periods = [flattened_period(moons, idx) for idx in range(3)]
    # Process is reversible, so no tail to worry about
    return reduce(lcm, periods)

print(part1())
print(part2())
