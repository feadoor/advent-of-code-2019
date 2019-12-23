from intcode import IntcodeVm

def get_input():
    with open('../data/21.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

MAIN_PROGRAM = get_input()

def run_springscript(instructions):
    vm = IntcodeVm(MAIN_PROGRAM)
    for instr in instructions:
        for c in instr: vm.send(ord(c))
        vm.send(ord('\n'))

    outputs = list(vm.outputs)
    if outputs[-1] > 255:
        print(outputs[-1])
    else:
        print(''.join(chr(c) for c in outputs))

def part1():
    run_springscript(['NOT A J', 'NOT B T', 'NOT C T', 'OR T J', 'AND D J', 'WALK'])

def part2():
    run_springscript(['NOT A J', 'NOT B T', 'OR T J', 'NOT C T', 'OR T J', 'AND D J', 'NOT E T', 'NOT T T', 'OR H T', 'AND T J', 'RUN'])

part1()
part2()
