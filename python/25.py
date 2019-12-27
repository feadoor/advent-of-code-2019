from intcode import IntcodeVm

def get_input():
    with open('../data/25.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

MAIN_PROGRAM = get_input()

def play_game():
    vm = IntcodeVm(MAIN_PROGRAM)
    while True:
        while True:
            try:
                char = next(vm.outputs)
                if char is None: break
                print(chr(char), end='')
            except StopIteration:
                return
        command = input('Enter command: ')
        for char in command + '\n':
            vm.send(ord(char))

play_game()
