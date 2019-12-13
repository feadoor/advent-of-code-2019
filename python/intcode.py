from collections import deque

class IntcodeVm:

    def __init__(self, program, initial_inputs = []):
        self.runner = IntcodeRunner(program, initial_inputs)
        self.inputs, self.outputs = self.runner.run()
        self.inputs.send(None)

    def __getitem__(self, index):
        return self.runner[index]

    @property
    def memory(self):
        return self.runner.program

    def send(self, value):
        self.inputs.send(value)

    def receive(self):
        return next(self.outputs)


class IntcodeMemory:

    def __init__(self, memory):
        self.memory = memory

    def __getitem__(self, index):
        if index > len(self.memory) - 1:
            self.memory += [0] * (index - len(self.memory) + 1)
        return self.memory[index]

    def __setitem__(self, index, val):
        if index > len(self.memory) - 1:
            self.memory += [0] * (index - len(self.memory) + 1)
        self.memory[index] = val


class IntcodeRunner:

    def __init__(self, program, initial_inputs = []):
        self.program = IntcodeMemory(program[:])
        self.pc = 0
        self.relative_base = 0
        self.inputs = deque(initial_inputs)

    def __getitem__(self, index):
        return self.program[index]

    def read(self):
        self.pc += 1
        return self.program[self.pc - 1]

    def write(self, idx, val):
        self.program[idx] = val
        return val

    def read_dest(self, modes):
        mode = modes.pop() if len(modes) > 0 else 0
        return self.read() + (self.relative_base if mode == 2 else 0)

    def load(self, modes):
        mode = modes.pop() if len(modes) > 0 else 0
        if mode == 2:
            return self.load_relative()
        elif mode == 1:
            return self.load_immediate()
        else:
            return self.load_position()

    def load_position(self):
        return self.program[self.read()]

    def load_immediate(self):
        return self.read()

    def load_relative(self):
        return self.program[self.read() + self.relative_base]

    def get_opcode(self):
        raw_opcode = self.read()
        opcode, raw_opcode = raw_opcode % 100, raw_opcode // 100
        modes = list(map(int, str(raw_opcode)))
        return opcode, modes

    def opcode1(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read_dest(modes)
        self.write(dest, arg1 + arg2)

    def opcode2(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read_dest(modes)
        self.write(dest, arg1 * arg2)

    def opcode3(self, modes):
        try:
            input = self.inputs.popleft()
            self.write(self.read_dest(modes), input)
        except IndexError as e:
            self.pc -= 1
            raise e

    def opcode4(self, modes):
        return self.load(modes)

    def opcode5(self, modes):
        src, target = self.load(modes), self.load(modes)
        if src != 0:
            self.pc = target

    def opcode6(self, modes):
        src, target = self.load(modes), self.load(modes)
        if src == 0:
            self.pc = target

    def opcode7(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read_dest(modes)
        self.write(dest, 1 if arg1 < arg2 else 0)

    def opcode8(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read_dest(modes)
        self.write(dest, 1 if arg1 == arg2 else 0)

    def opcode9(self, modes):
        arg1 = self.load(modes)
        self.relative_base += arg1

    def opcode99(self, modes):
        pass

    def step(self):
        opcode, modes = self.get_opcode()
        return getattr(self, 'opcode' + str(opcode))(modes)

    def add_input(self, input):
        self.inputs.append(input)

    def needs_input(self):
        opcode, _ = self.get_opcode()
        self.pc -= 1
        return opcode == 3 and len(self.inputs) == 0

    def run(self):

        def outputs():
            while self.program[self.pc] != 99:
                try:
                    output = self.step()
                    if output is not None:
                        yield output
                except IndexError:
                    yield None

        def inputs():
            while True:
                value = yield
                self.inputs.append(value)

        return (inputs(), outputs())