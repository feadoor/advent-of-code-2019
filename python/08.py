ROWS, COLS = 25, 6
SZ = ROWS * COLS

def get_input():
    with open('../data/08.txt', 'r') as f:
        for line in f:
            return layers(line)

def layers(image):
    layers = len(image) // SZ
    return [image[x : x + SZ] for x in range(0, SZ * layers, SZ)]

def best_layer(layers):
    return min(layers, key=lambda l: l.count('0'))

def part1(layers):
    l = best_layer(layers)
    return l.count('1') * l.count('2')

def decode(layers):
    def pixel(layers, x, y):
        return next(l[ROWS * y + x] for l in layers if l[ROWS * y + x] != '2')
    image = [[pixel(layers, x, y) for x in range(ROWS)] for y in range(COLS)]
    return '\n'.join(''.join('X' if int(p) else ' ' for p in l) for l in image)

print(part1(get_input()))
print(decode(get_input()))
