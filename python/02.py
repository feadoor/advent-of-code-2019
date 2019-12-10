import itertools
from intcode import IntcodeVm

def get_input():
    with open('../data/02.txt', 'r') as file:
        for line in file:
            return list(map(int, line.split(',')))

def part1():
    program = get_input()
    program[1], program[2] = 12, 2
    vm = IntcodeVm(program, [])
    for _ in vm.outputs: pass
    return vm[0]

def part2():
    for a, b in itertools.product(range(100), range(100)):
        program = get_input()
        program[1], program[2] = a, b
        vm = IntcodeVm(program)
        for _ in vm.outputs: pass
        if vm[0] == 19690720:
            return 100 * a + b

print(part1())
print(part2())
