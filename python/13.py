from intcode import IntcodeVm
from time import sleep

def get_input():
    with open('../data/13.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

def part1():
    program = get_input()
    vm, screen = IntcodeVm(program, []), {}
    while True:
        try:
            a, b, c = vm.receive(), vm.receive(), vm.receive()
            screen[(a, b)] = c
        except StopIteration:
            break
    return sum(v == 2 for v in screen.values())

def part2():
    program = get_input()
    program[0] = 2
    vm, screen, score = IntcodeVm(program, []), {}, 0
    paddle_x, ball_x = 0, 0

    def update():
        nonlocal score, paddle_x, ball_x
        while True:
            a, b, c = vm.receive(), vm.receive(), vm.receive()
            if a is None or b is None or c is None:
                break
            if (a, b) != (-1, 0):
                screen[(a, b)] = c
                if c == 3: paddle_x = a
                if c == 4: ball_x = a
            else: score = c

    def joystick_pos():
        if paddle_x < ball_x: return 1
        elif paddle_x > ball_x: return -1
        else: return 0

    while True:
        try:
            update()
            vm.send(joystick_pos())
        except StopIteration:
            break

    return score

print(part1())
print(part2())
