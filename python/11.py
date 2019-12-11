from intcode import IntcodeVm

def get_input():
    with open('../data/11.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

def turn(direction, lr):
    if lr == 1:
        return {'U': 'R', 'R': 'D', 'D': 'L', 'L': 'U'}[direction]
    else:
        return {'U': 'L', 'L': 'D', 'D': 'R', 'R': 'U'}[direction]

def move(pos, direction):
    x, y = pos
    if direction == 'U':
        return (x, y + 1)
    elif direction == 'R':
        return (x + 1, y)
    elif direction == 'D':
        return (x, y - 1)
    elif direction == 'L':
        return (x - 1, y)

def paint(prog, initial_white_tiles = []):
    vm = IntcodeVm(prog)
    visited, whites, pos, direction = set(), set(initial_white_tiles), (0, 0), 'U'

    while True:
        vm.send(1 if pos in whites else 0)
        try:
            paint, lr = vm.receive(), vm.receive()
            visited.add(pos)
            if paint == 1:
                whites.add(pos)
            elif paint == 0:
                whites.discard(pos)
            direction = turn(direction, lr)
            pos = move(pos, direction)
        except StopIteration:
            break

    return visited, whites

def part1(prog):
    return len(paint(prog)[0])

def part2(prog):
    white_tiles = paint(prog, [(0, 0)])[1]

    minx = min(t[0] for t in white_tiles)
    maxx = max(t[0] for t in white_tiles)
    miny = min(t[1] for t in white_tiles)
    maxy = max(t[1] for t in white_tiles)

    output = [[' ' for _ in range(maxx - minx + 1)] for _ in range(maxy - miny + 1)]
    for t in white_tiles:
        output[maxy - t[1] + miny - 1][t[0] - minx] = 'X'
    return '\n'.join(''.join(x) for x in output)

print(part1(get_input()))
print(part2(get_input()))
