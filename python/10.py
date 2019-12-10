from math import atan2

def get_field():
    with open('../data/10.txt', 'r') as f:
        return [line for line in f]

def get_asteroids():
    field, asteroids = get_field(), []
    for x, line in enumerate(field):
        for y, ch in enumerate(line):
            if ch == '#':
                asteroids.append((x, y))
    return asteroids

def gcd(x, y):
    while y != 0:
        x, y = y, x % y
    return x

def gradient(x, y):
    if y == 0 and x < 0:
        return (-1, 0)
    elif y == 0 and x > 0:
        return (1, 0)
    else:
        return (x // abs(gcd(x, y)), y // abs(gcd(x, y)))

def angle_from_gradient(x, y):
    return atan2(y, x)

def count_visible_from(asteroids, x, y):
    relative_positions = [(a - x, b - y) for (a, b) in asteroids]
    gradients = [gradient(a, b) for (a, b) in relative_positions if (a, b) != (0, 0)]
    return len(set(gradients))

def best_asteroid(asteroids):
    return max(asteroids, key=lambda a: count_visible_from(asteroids, a[0], a[1]))

def visible_asteroids_by_gradient(asteroids, x, y):
    result = {}
    for (a, b) in asteroids:
        grad = gradient(a - x, b - y)
        existing_asteroid = result.get(grad, (float("inf"), float("inf")))
        if abs(a - x) < abs(existing_asteroid[0] - x) or abs(b - y) < abs(existing_asteroid[1] - y):
            result[grad] = (a, b)
    return result

def destroyed_asteroids(asteroids, x, y):
    survivors = set(asteroids) - {(x, y)}
    while survivors:
        next_asteroids = visible_asteroids_by_gradient(survivors, x, y)
        sorted_gradients = sorted(list(next_asteroids.keys()), key=lambda g: -angle_from_gradient(g[0], g[1]))
        yield from (next_asteroids[g] for g in sorted_gradients)
        survivors = survivors - set(next_asteroids.values())

def part1():
    asteroids = get_asteroids()
    return max(count_visible_from(asteroids, x, y) for (x, y) in asteroids)

def part2():
    dead, asteroids = 0, get_asteroids()
    x, y = best_asteroid(asteroids)
    print(list(destroyed_asteroids(asteroids, x, y)))
    (dx, dy) = next(a for idx, a in enumerate(destroyed_asteroids(asteroids, x, y)) if idx == 199)
    return 100 * dy + dx

print(part1())
print(part2())
