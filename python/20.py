from collections import defaultdict, deque

def get_maze():
    with open('../data/20.txt', 'r') as f:
        return [list(line.strip('\n')) for line in f]

def get_labels(maze):
    labels = {}
    reverse_labels = defaultdict(list)

    for y in range(len(maze)):
        for x in range(len(maze[y])):
            if maze[y][x] == '.':
                if y >= 2 and 'A' <= maze[y - 1][x] <= 'Z' and 'A' <= maze[y - 2][x] <= 'Z':
                    reverse_labels[maze[y - 2][x] + maze[y - 1][x]].append((x, y))
                    labels[(x, y)] = maze[y - 2][x] + maze[y - 1][x]
                if y < len(maze) - 2 and 'A' <= maze[y + 1][x] <= 'Z' and 'A' <= maze[y + 2][x] <= 'Z':
                    reverse_labels[maze[y + 1][x] + maze[y + 2][x]].append((x, y))
                    labels[(x, y)] = maze[y + 1][x] + maze[y + 2][x]
                if x >= 2 and 'A' <= maze[y][x - 1] <= 'Z' and 'A' <= maze[y][x - 2] <= 'Z':
                    reverse_labels[maze[y][x - 2] + maze[y][x - 1]].append((x, y))
                    labels[(x, y)] = maze[y][x - 2] + maze[y][x - 1]
                if x < len(maze[y]) - 2 and 'A' <= maze[y][x + 1] <= 'Z' and 'A' <= maze[y][x + 2] <= 'Z':
                    reverse_labels[maze[y][x + 1] + maze[y][x + 2]].append((x, y))
                    labels[(x, y)] = maze[y][x + 1] + maze[y][x + 2]

    return labels, reverse_labels

def is_outer(maze, pos):
    return pos[0] == 2 or pos[1] == 2 or pos[0] == len(maze[pos[1]]) - 3 or pos[1] == len(maze) - 3

def neighbours(pos, maze, labels, reverse_labels):
    x, y, z = pos
    nbs = [(x - 1, y, z), (x + 1, y, z), (x, y + 1, z), (x, y - 1, z)]
    if (x, y) in labels:
        for other_x, other_y in reverse_labels[labels[(x, y)]]:
            if (other_x, other_y) != (x, y):
                if is_outer(maze, (x, y)):
                    nbs.append((other_x, other_y, z - 1))
                else:
                    nbs.append((other_x, other_y, z + 1))
    return [(a, b, c) for a, b, c in nbs if 0 <= b < len(maze) and 0 <= a < len(maze[b]) and maze[b][a] == '.']

def bfs_without_depth(maze, labels, reverse_labels, start, end):
    queue, visited = deque([((start[0], start[1], 0), 0)]), {start}
    while queue:
        curr_pos, dist = queue.popleft()
        for x, y, z in neighbours(curr_pos, maze, labels, reverse_labels):
            if (x, y) not in visited:
                visited.add((x, y))
                queue.append(((x, y, 0), dist + 1))
                if (x, y) == end: return dist + 1

def bfs_with_depth(maze, labels, reverse_labels, start, end):
    queue, visited = deque([(start, 0)]), {start}
    while queue:
        curr_pos, dist = queue.popleft()
        for x, y, z in neighbours(curr_pos, maze, labels, reverse_labels):
            if z >= 0 and (x, y, z) not in visited:
                visited.add((x, y, z))
                queue.append(((x, y, z), dist + 1))
                if (x, y, z) == end: return dist + 1

def part1():
    maze = get_maze()
    labels, reverse_labels = get_labels(maze)
    start, end = reverse_labels['AA'][0], reverse_labels['ZZ'][0]
    return bfs_without_depth(maze, labels, reverse_labels, start, end)

def part2():
    maze = get_maze()
    labels, reverse_labels = get_labels(maze)
    start, end = reverse_labels['AA'][0], reverse_labels['ZZ'][0]
    return bfs_with_depth(maze, labels, reverse_labels, (start[0], start[1], 0), (end[0], end[1], 0))

print(part1())
print(part2())
