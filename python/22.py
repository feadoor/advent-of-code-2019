def get_input():
    with open('../data/22.txt', 'r') as f:
        return [line for line in f]

def bezout(a, b):
    s, old_s = 0, 1
    t, old_t = 1, 0
    r, old_r = b, a
    while r != 0:
        q = old_r // r
        old_r, r = r, old_r - q * r
        old_s, s = s, old_s - q * s
        old_t, t = t, old_t - q * t
    return (old_s, old_t) if old_s * a + old_t * b >= 0 else (-old_s, -old_t)

def mod_inverse(a, m):
    s, t = bezout(a, m)
    return s % m

def get_quick_shuffle(instructions, size):
    mul, plus = 1, 0
    for instr in instructions:
        if instr.startswith('deal with increment'):
            amt = int(instr.split(' ')[-1])
            mul, plus = (mul * amt) % size, (plus * amt) % size
        elif instr.startswith('deal into new stack'):
            mul, plus = (-mul) % size, (-plus - 1) % size
        elif instr.startswith('cut'):
            amt = int(instr.split(' ')[-1])
            mul, plus = mul, (plus - amt) % size
    return mul, plus

def compose(shuffle1, shuffle2, size):
    mul1, plus1 = shuffle1
    mul2, plus2 = shuffle2
    return (mul2 * mul1) % size, (mul2 * plus1 + plus2) % size

def iterate(shuffle, times, size):
    ans = (1, 0)
    worker = shuffle
    while times > 0:
        if times % 2 == 1:
            ans = compose(ans, worker, size)
        worker = compose(worker, worker, size)
        times //= 2
    return ans

def part1():
    mul, plus = get_quick_shuffle(get_input(), 10007)
    return (2019 * mul + plus) % 10007

def part2():
    shuffle = get_quick_shuffle(get_input(), 119315717514047)
    mul, plus = iterate(shuffle, 101741582076661, 119315717514047)
    return ((2020 - plus) * mod_inverse(mul, 119315717514047)) % 119315717514047

print(part1())
print(part2())
