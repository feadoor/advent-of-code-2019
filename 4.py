from collections import Counter

def meets_part_1(num):
    digits = str(num)
    return list(digits) == sorted(digits) and any(x >= 2 for x in Counter(digits).values())

def meets_part_2(num):
    digits = str(num)
    return list(digits) == sorted(digits) and any(x == 2 for x in Counter(digits).values())

print(len(list(filter(meets_part_1, range(235741, 706949)))))
print(len(list(filter(meets_part_2, range(235741, 706949)))))
