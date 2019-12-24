def get_input():
    with open('../data/24.txt', 'r') as f:
        text_input = [[1 if c == '#' else 0 for c in line] for line in f]
        return {(x, y) for x in range(5) for y in range(5) if text_input[y][x] == 1}

def neighbours(x, y):
    return [n for n in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)) if 0 <= n[0] < 5 and 0 <= n[1] < 5]

def neighbours2(x, y, d):
    neighbours = [n for n in ((x - 1, y, d), (x + 1, y, d), (x, y - 1, d), (x, y + 1, d)) if 0 <= n[0] < 5 and 0 <= n[1] < 5 and n != (2, 2)]
    if (x, y) == (2, 1): neighbours += [(a, 0, d + 1) for a in range(5)]
    if (x, y) == (1, 2): neighbours += [(0, a, d + 1) for a in range(5)]
    if (x, y) == (3, 2): neighbours += [(4, a, d + 1) for a in range(5)]
    if (x, y) == (2, 3): neighbours += [(a, 4, d + 1) for a in range(5)]
    if y == 0: neighbours.append((2, 1, d - 1))
    if x == 0: neighbours.append((1, 2, d - 1))
    if x == 4: neighbours.append((3, 2, d - 1))
    if y == 4: neighbours.append((2, 3, d - 1))
    return neighbours

def tick(bugs):
    new_bugs = set()
    for y in range(5):
        for x in range(5):
            adjacent_bugs = sum(n in bugs for n in neighbours(x, y))
            if (x, y) in bugs and adjacent_bugs == 1:
                new_bugs.add((x, y))
            elif (x, y) not in bugs and (adjacent_bugs == 1 or adjacent_bugs == 2):
                new_bugs.add((x, y))
    return new_bugs

def tick2(bugs):
    new_bugs = set()
    min_d, max_d = min(d for x, y, d in bugs), max(d for x, y, d in bugs)
    for d in range(min_d - 1, max_d + 2):
        for x in range(5):
            for y in range(5):
                adjacent_bugs = sum(n in bugs for n in neighbours2(x, y, d))
                if (x, y, d) in bugs and adjacent_bugs == 1:
                    new_bugs.add((x, y, d))
                elif (x, y, d) not in bugs and (adjacent_bugs == 1 or adjacent_bugs == 2):
                    new_bugs.add((x, y, d))
    return new_bugs


def biodiversity(bugs):
    return sum(1 << n for n in range(25) if (n % 5, n // 5) in bugs)

def part1():
    bugs, seen = get_input(), set()
    while ''.join(''.join(str(x) for x in row) for row in bugs) not in seen:
        seen.add(''.join(''.join(str(x) for x in row) for row in bugs))
        bugs = tick(bugs)
    return biodiversity(bugs)

def part2():
    bugs = {(x, y, 0) for (x, y) in get_input()}
    for _ in range(200):
        bugs = tick2(bugs)
    return len(bugs)

print(part1())
print(part2())
