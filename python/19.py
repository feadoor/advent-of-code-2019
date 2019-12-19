from intcode import IntcodeVm
from itertools import count

def get_input():
    with open('../data/19.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

PROGRAM = get_input()

def is_beam(x, y):
    vm = IntcodeVm(PROGRAM)
    vm.send(x)
    vm.send(y)
    return vm.receive() == 1

def get_first_bounding_box(dx, dy):
    starts, ends = [], []
    for x in count(0):

        start_y = 0 if not starts else starts[-1]
        while not is_beam(x, start_y): start_y += 1
        starts.append(start_y)

        for y in count(start_y + 1):
            if not is_beam(x, y): break
        ends.append(y - 1)

        if (x >= dx - 1 and ends[x] - starts[x] >= dy - 1 and starts[x] + dy - 1 <= ends[x - dx + 1]):
            return (x - dx + 1, starts[x])

def part1():
    return sum(is_beam(x, y) for x in range(50) for y in range(50))

def part2():
    (x, y) = get_first_bounding_box(100, 100)
    return 10000 * x + y

print(part1())
print(part2())
