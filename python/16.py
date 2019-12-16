from itertools import cycle, islice

def get_input():
    with open('../data/16.txt', 'r') as f:
        for line in f:
            return [int(d) for d in line]

def get_pattern(idx):
    return islice(cycle([0] * idx + [1] * idx + [0] * idx + [-1] * idx), 1, None)

def get_new_number(nums, idx):
    return abs(sum(x * y for x, y in zip(nums, get_pattern(idx)))) % 10

def transform(nums):

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

def part1():
    nums = get_input()
    for _ in range(100):
        nums = transform(nums)
    return nums[:8]

def part2():
    nums = get_input() * 10000
    offset = int(''.join(str(d) for d in nums[:7]))
    for _ in range(100):
        nums = transform(nums)
    return nums[offset:offset + 8]

print(part1())
print(part2())
