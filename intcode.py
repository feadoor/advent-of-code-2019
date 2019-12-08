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


class IntcodeRunner:

    def __init__(self, program, initial_inputs = []):
        self.program = program[:]
        self.pc = 0
        self.inputs = deque(initial_inputs)

    def __getitem__(self, index):
        return self.program[index]

    def read(self):
        self.pc += 1
        return self.program[self.pc - 1]

    def load(self, modes):
        mode_type = modes.pop() if len(modes) > 0 else 0
        if mode_type == 0:
            return self.load_position()
        else:
            return self.load_immediate()

    def load_position(self):
        return self.program[self.read()]

    def load_immediate(self):
        return self.read()

    def write(self, idx, val):
        self.program[idx] = val
        return val

    def get_opcode(self):
        raw_opcode = self.read()
        opcode, raw_opcode = raw_opcode % 100, raw_opcode // 100
        modes = list(map(int, str(raw_opcode)))
        return opcode, modes

    def opcode1(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read()
        self.write(dest, arg1 + arg2)

    def opcode2(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read()
        self.write(dest, arg1 * arg2)

    def opcode3(self, modes):
        dest = self.read()
        self.write(dest, self.inputs.popleft())

    def opcode4(self, modes):
        return self.load(modes)

    def opcode5(self, modes):
        src, dest = self.load(modes), self.load(modes)
        if src != 0:
            self.pc = dest

    def opcode6(self, modes):
        src, dest = self.load(modes), self.load(modes)
        if src == 0:
            self.pc = dest

    def opcode7(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read()
        self.write(dest, 1 if arg1 < arg2 else 0)

    def opcode8(self, modes):
        arg1, arg2, dest = self.load(modes), self.load(modes), self.read()
        self.write(dest, 1 if arg1 == arg2 else 0)

    def opcode99(self, modes):
        pass

    def step(self):
        opcode, modes = self.get_opcode()
        return getattr(self, 'opcode' + str(opcode))(modes)

    def add_input(self, input):
        self.inputs.append(input)

    def run(self):

        def outputs():
            while self.program[self.pc] != 99:
                output = self.step()
                if output is not None:
                    yield output

        def inputs():
            while True:
                value = yield
                self.inputs.append(value)

        return (inputs(), outputs())