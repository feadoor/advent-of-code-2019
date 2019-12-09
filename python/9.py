from intcode import IntcodeVm

def get_input():
    with open('../data/09.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

def part1():
    program = get_input()
    *_, output = IntcodeVm(program, [1]).outputs
    return output

def part2():
    program = get_input()
    *_, output = IntcodeVm(program, [2]).outputs
    return output

print(part1())
print(part2())
