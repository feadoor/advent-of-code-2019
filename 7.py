from intcode import IntcodeVm
from itertools import permutations

def get_input():
    with open('data/7.txt', 'r') as file:
        for line in file:
            return list(map(int, line.split(',')))

def part1():
    program, best_output = get_input(), -float("inf")
    for phases in permutations(range(5), 5):
        signal = 0
        for phase in phases:
            *_, signal = IntcodeVm(program, [phase, signal]).run()
        if signal > best_output:
            best_output = signal
    return best_output

def part2():
    program, best_output = get_input(), -float("inf")
    for phases in permutations(range(5, 10), 5):
        current_amp, signal = 0, 0
        amps = [IntcodeVm(program, [phases[idx]]) for idx in range(5)]
        emitters = [amp.run() for amp in amps]
        while True:
            try:
                amps[current_amp].add_input(signal)
                signal = next(emitters[current_amp])
                current_amp = (current_amp + 1) % 5
            except StopIteration:
                if signal > best_output:
                    best_output = signal
                break

    return best_output

print(part1())
print(part2())
