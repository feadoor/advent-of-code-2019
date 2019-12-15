from collections import deque
from intcode import IntcodeVm

def get_input():
    with open('../data/15.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

def code(d):
    return {'N': 1, 'S': 2, 'E': 4, 'W': 3}[d]

def opposite(d):
    return {'N': 'S', 'S': 'N', 'E': 'W', 'W': 'E'}[d]

def move(pos, d):
    x, y = pos
    if d == 'N': return (x, y + 1)
    elif d == 'S': return (x, y - 1)
    elif d == 'E': return (x + 1, y)
    elif d == 'W': return (x - 1, y)

def fully_explore(vm):
    visited = {(0, 0)}
    valid = {(0, 0)}
    destination = None
    pos = (0, 0)

    stack = [('OUT', 'N'), ('OUT', 'S'), ('OUT', 'E'), ('OUT', 'W')]
    while stack:
        mode, d = stack.pop()
        vm.send(code(d))
        result = vm.receive()
        if result != 0: pos = move(pos, d)
        if mode == 'OUT' and result != 0:
            valid.add(pos)
            stack.append(('IN', opposite(d)))
            if result == 2: destination = pos
            for next_d in 'NSEW':
                if move(pos, next_d) not in visited:
                    visited.add(move(pos, next_d))
                    stack.append(('OUT', next_d))
    return destination, valid

def bfs(locations, src):
    distances = {src: 0}
    queue = deque([(src, 0)])
    while queue:
        pos, dist = queue.popleft()
        for d in 'NSEW':
            next_loc = move(pos, d)
            if next_loc in locations and next_loc not in distances:
                distances[next_loc] = dist + 1
                queue.append((move(pos, d), dist + 1))
    return distances

def part1():
    program = get_input()
    vm = IntcodeVm(program)
    dest, locations = fully_explore(vm)
    distances = bfs(locations, dest)
    return distances[(0, 0)]

def part2():
    program = get_input()
    vm = IntcodeVm(program)
    dest, locations = fully_explore(vm)
    distances = bfs(locations, dest)
    return max(distances.values())

print(part1())
print(part2())
