from intcode import IntcodeVm

def get_input():
    with open('../data/05.txt', 'r') as file:
        for line in file:
            return list(map(int, line.split(',')))

def part1():
    program = get_input()
    *_, output = IntcodeVm(program, [1]).outputs
    return output

def part2():
    program = get_input()
    *_, output = IntcodeVm(program, [5]).outputs
    return output

print(part1())
print(part2())
