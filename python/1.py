def get_module_weights():
    with open('../data/1.txt', 'r') as file:
        return [int(line) for line in file]

def fuel_for_mass(mass):
    return mass // 3 - 2

def recursive_fuel_for_mass(mass):
    fuel = fuel_for_mass(mass)
    return 0 if fuel <= 0 else fuel + recursive_fuel_for_mass(fuel)

def get_total_fuel_requirements():
    return sum(map(fuel_for_mass, get_module_weights()))

def get_recursive_fuel_requirements():
    return sum(map(recursive_fuel_for_mass, get_module_weights()))

def part1():
    return get_total_fuel_requirements()

def part2():
    return get_recursive_fuel_requirements()

print(part1())
print(part2())
