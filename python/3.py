def parse_step(step):
    direction = step[0]
    distance = int(step[1:])
    return (direction, distance)

def parse_wire(wire):
    steps = wire.split(',')
    return [parse_step(step) for step in steps]

def get_input():
    with open('../data/3.txt', 'r') as file:
        return [parse_wire(line.strip()) for line in file]

def step(location, direction):
    x, y = location
    if direction == 'U':
        return (x, y + 1)
    if direction == 'D':
        return (x, y - 1)
    if direction == 'R':
        return (x + 1, y)
    if direction == 'L':
        return (x - 1, y)

def get_positions_and_distances(wire):
    location, steps, positions, distances = (0, 0), 0, set(), {}

    for direction, distance in wire:
        for x in range(1, distance + 1):
            location = step(location, direction)
            steps += 1
            if location not in positions:
                positions.add(location)
                distances[location] = steps

    return positions, distances

def get_intersections(*wires):
    positions_and_distances = [get_positions_and_distances(wire) for wire in wires]
    common_positions = set.intersection(*[x[0] for x in positions_and_distances])
    return [(p, [x[1][p] for x in positions_and_distances]) for p in common_positions]

def manhattan_distance(p1, p2):
    return sum(abs(x - y) for x, y in zip(p1, p2))

def get_nearest_intersection(wires):
    intersections = get_intersections(*wires)
    return min(manhattan_distance((0, 0), x[0]) for x in intersections)

def get_intersection_with_least_steps(wires):
    intersections = get_intersections(*wires)
    return min(sum(x[1]) for x in intersections)

def part1():
    print(get_nearest_intersection(get_input()))

def part2():
    print(get_intersection_with_least_steps(get_input()))

part1()
part2()
