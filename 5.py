from intcode import IntcodeVm

def get_input():
    with open('data/5.txt', 'r') as file:
        for line in file:
            return list(map(int, line.split(',')))

def part1():
    program = get_input()
    *_, output = IntcodeVm(program, [1]).run()
    return output

def part2():
    program = get_input()
    *_, output = IntcodeVm(program, [5]).run()
    return output

print(part1())
print(part2())
