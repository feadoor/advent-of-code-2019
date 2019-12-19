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

def get_count(width, height):
    start_y, count = 0, 0
    for x in range(0, width):
        while not is_beam(x, start_y): start_y += 1
        for y in range(start_y, height):
            if is_beam(x, y): count += 1
            else: break
    return count

def get_first_bounding_box(dx, dy):
    y = 0
    for x in count(0):
        while not is_beam(x, y): y += 1
        if not is_beam(x, y + dy - 1): continue
        if x >= dx - 1 and is_beam(x - dx + 1, y + dy - 1): return (x - dx + 1, y)

def part1():
    return get_count(50, 50)

def part2():
    (x, y) = get_first_bounding_box(100, 100)
    return 10000 * x + y

print(part1())
print(part2())
