from collections import defaultdict

def parse_chemical(s):
    amt, name = s.strip().split(' ')
    return int(amt), name

def parse_line(line):
    inputs, output = line.split(' => ')
    in_chemicals = [parse_chemical(s) for s in inputs.split(',')]
    out_chemical = parse_chemical(output)
    return in_chemicals, out_chemical

def get_reactions():
    with open('../data/14.txt', 'r') as f:
        return [parse_line(line) for line in f]

def transmute(ingredients, recipe_book):

    if not all(name == 'ORE' for name in ingredients):

        ingredient_lists = { x : [name for amt, name in recipe_book[x][1]] for x in ingredients if x != 'ORE' }
        for leaf_ingredient in [x for x in ingredients if x != 'ORE' and not any(x in ingredient_lists[y] for y in ingredients if y != 'ORE')]:

            amount = ingredients[leaf_ingredient]
            recipe = recipe_book[leaf_ingredient]
            qty = (amount + recipe[0] - 1) // recipe[0]
            for amt, name in recipe_book[leaf_ingredient][1]:
                ingredients[name] += qty * amt
            del ingredients[leaf_ingredient]

    return ingredients

def ore_required_for_fuel(fuel, recipe_book):
    ingredients = {ingredient : 0 for ingredient in recipe_book}
    ingredients['ORE'] = 0
    ingredients['FUEL'] = fuel
    while any(x != 'ORE' for x in ingredients):
        ingredients = transmute(ingredients, recipe_book)
    return ingredients['ORE']

def part1():
    reactions = get_reactions()
    recipe_book = {output[1] : (output[0] ,inputs) for (inputs, output) in reactions}
    return ore_required_for_fuel(1, recipe_book)

def part2():
    reactions = get_reactions()
    recipe_book = {output[1] : (output[0] ,inputs) for (inputs, output) in reactions}
    fuel, step = 1, 1
    while ore_required_for_fuel(fuel + step, recipe_book) < 1000000000000:
        step *= 2
    while step > 0:
        if ore_required_for_fuel(fuel + step, recipe_book) < 1000000000000:
            fuel += step
        step //= 2
    return fuel

print(part1())
print(part2())
