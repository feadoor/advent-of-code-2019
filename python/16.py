# Get the input numbers
def get_input():
    with open('../data/16.txt', 'r') as f:
        for line in f:
            return [int(d) for d in line]

# A slow FFT according to the actual problem description
def transform_single(nums):

    partial_sums = [0] * (len(nums) + 1)
    for n in range(1, len(nums) + 1):
        partial_sums[n] = partial_sums[n - 1] + nums[n - 1]

    def segment(l, r):
        if l < 0: l = 0
        if l > len(nums): l = len(nums)
        if r < 0: r = 0
        if r > len(nums): r = len(nums)
        return partial_sums[r] - partial_sums[l]

    output = []
    for n in range(1, len(nums) + 1):
        total = 0
        for k in range(n - 1, len(nums) + 1, 4 * n):
            total += segment(k, k + n)
            total -= segment(k + 2 * n, k + 3 * n)
        output.append(abs(total) % 10)

    return output

# Calculates the Bezout coefficients s, t such that as + bt = g, where g is
# the greatest common divisor of a and b.
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

# Calculates the inverse of a mod m
def inverse(a, m):
    s, t = bezout(a, m)
    return s % m

# Return the largest power of p that divides n, and the part of n which is coprime to p
def max_power(n, p):
    power = 0
    while n % p == 0:
        n //= p
        power += 1
    return n, power

# The binomial coefficients (base choose base), (base + 1 choose base), ..., (base + sz - 1 choose base), all modulo 10
def binomials_mod10(base, sz):
    twos, fives, curr = 0, 0, base
    free_binoms = [1]
    binoms = [1]

    while len(binoms) < sz:

        numer, denom = curr + 1, curr - base + 1

        numer, numer2 = max_power(numer, 2)
        numer, numer5 = max_power(numer, 5)
        denom, denom2 = max_power(denom, 2)
        denom, denom5 = max_power(denom, 5)

        curr, twos, fives = curr + 1, twos + numer2 - denom2, fives + numer5 - denom5
        free_binoms.append((free_binoms[-1] * numer * inverse(denom, 10)) % 10)
        binoms.append((free_binoms[-1] * pow(2, twos, 10) * pow(5, fives, 10)) % 10)

    return binoms

# Uses clever maths to do everything very quickly
def shortcut_output(nums, iterations, offset, length):

    # The clever maths only works if our output is in the second half of the signal
    if offset < len(nums) // 2:
        raise Exception(f'The offset {offset} is too small for input length {len(nums)}')

    offset -= len(nums) // 2
    half_length = (len(nums) + 1) // 2
    binoms = binomials_mod10(iterations - 1, half_length - offset)

    result = []
    for n in range(offset, offset + length):
        result.append(sum(binoms[m - n] * nums[m + len(nums) // 2] for m in range(n, half_length)) % 10)

    return result


def part1():
    nums = get_input()
    for _ in range(100):
        nums = transform_single(nums)
    return ''.join(str(d) for d in nums[:8])

def part2():
    nums = get_input() * 10000
    offset = int(''.join(str(d) for d in nums[:7]))
    return ''.join(str(d) for d in shortcut_output(nums, 100, offset, 8))

print(part1())
print(part2())
