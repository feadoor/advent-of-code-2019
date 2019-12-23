from intcode import IntcodeVm

def get_input():
    with open('../data/23.txt', 'r') as f:
        for line in f:
            return list(map(int, line.split(',')))

MAIN_PROGRAM = get_input()

def part1():
    network = [IntcodeVm(MAIN_PROGRAM) for _ in range(50)]
    for idx, vm in enumerate(network):
        vm.send(idx)

    while True:
        for vm in network:

            a, b, c = vm.receive(), vm.receive(), vm.receive()
            if a is None:
                vm.send(-1)
            elif a is not None and 0 <= a < 50:
                network[a].send(b)
                network[a].send(c)
            elif a is not None and a == 255:
                return c

def part2():
    network = [IntcodeVm(MAIN_PROGRAM) for _ in range(50)]
    for idx, vm in enumerate(network):
        vm.send(idx)

    nat_x, nat_y = 0, 0
    last_restart = None

    while True:

        while True:
            sent_packets = False
            for vm in network:
                a, b, c = vm.receive(), vm.receive(), vm.receive()
                if a is None:
                    vm.send(-1)
                elif a is not None and 0 <= a < 50:
                    network[a].send(b)
                    network[a].send(c)
                    sent_packets = True
                elif a is not None and a == 255:
                    nat_x, nat_y = b, c

            if not sent_packets: break

        if nat_y == last_restart: return nat_y

        network[0].send(nat_x)
        network[0].send(nat_y)
        last_restart = nat_y

print(part1())
print(part2())
