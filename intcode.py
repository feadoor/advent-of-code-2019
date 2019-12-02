class IntcodeVm:

    def __init__(self, program):
        self.program = program
        self.pc = 0

    def __getitem__(self, index):
        return self.program[index]

    def read(self):
        self.pc += 1
        return self.program[self.pc - 1]

    def load(self):
        return self.program[self.read()]

    def write(self, idx, val):
        self.program[idx] = val
        return val

    def opcode1(self):
        arg1, arg2, dest = self.load(), self.load(), self.read()
        self.write(dest, arg1 + arg2)

    def opcode2(self):
        arg1, arg2, dest = self.load(), self.load(), self.read()
        self.write(dest, arg1 * arg2)

    def opcode99(self):
        pass

    def step(self):
        opcode = self.read()
        getattr(self, 'opcode' + str(opcode))()

    def run(self):
        yield self
        while self.program[self.pc] != 99:
            self.step()
            yield self
