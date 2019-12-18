from collections import deque
from string import ascii_uppercase, ascii_lowercase

MEMO = {}

def get_maze_part1():
    with open('../data/18a.txt', 'r') as f:
        return [list(line) for line in f]

def get_maze_part2():
    with open('../data/18b.txt', 'r') as f:
        return [list(line) for line in f]

def update_at(lst, idx, val):
    return lst[:idx] + [val] + lst[idx + 1:]

def shortest_paths_from(maze, start_pos):

    def neighbours(x, y):
        nbs = [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]
        return [(a, b) for (a, b) in nbs if 0 <= b < len(maze) and 0 <= a < len(maze[b])]

    queue, visited, paths = deque([(start_pos, 0, set())]), {start_pos}, {}
    while queue:
        curr_pos, dist, keys = queue.popleft()
        for x, y in neighbours(*curr_pos):
            if (x, y) not in visited:
                visited.add((x, y))
                if maze[y][x] == '#': continue
                elif 'A' <= maze[y][x] <= 'Z':
                    queue.append(((x, y), dist + 1, keys | {maze[y][x].lower()}))
                elif 'a' <= maze[y][x] <= 'z':
                    paths[maze[y][x]] = ((x, y), dist + 1, keys)
                    queue.append(((x, y), dist + 1, keys | {maze[y][x]}))
                else:
                    queue.append(((x, y), dist + 1, keys))

    return paths

def _shortest_path_memo(paths, positions, keys_remaining):

    if len(keys_remaining) == 0: return 0

    key = (tuple(sorted(positions)), ''.join(sorted(keys_remaining)))
    if key in MEMO: return MEMO[key]

    answer = min(
        dist + _shortest_path_memo(paths, update_at(positions, robot_idx, pos), keys_remaining - {key})
        for robot_idx in range(len(positions))
        for (key, (pos, dist, req_keys)) in paths[positions[robot_idx]].items()
        if key in keys_remaining and all(k not in keys_remaining for k in req_keys)
    )
    MEMO[key] = answer
    return answer

def _shortest_path(paths, positions, keys_remaining):
    MEMO = {}
    return _shortest_path_memo(paths, positions, keys_remaining)

def shortest_path(maze):
    start_positions = [(x, y) for y in range(len(maze)) for x in range(len(maze[y])) if maze[y][x] == '@']
    keys = {maze[y][x]: (x, y) for y in range(len(maze)) for x in range(len(maze[y])) if 'a' <= maze[y][x] <= 'z'}
    paths = {pos: shortest_paths_from(maze, pos) for pos in start_positions + list(keys.values())}
    return _shortest_path(paths, start_positions, set(keys.keys()))

def part1():
    maze = get_maze_part1()
    return shortest_path(maze)

def part2():
    maze = get_maze_part2()
    return shortest_path(maze)

print(part1())
print(part2())
