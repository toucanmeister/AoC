use std::fs;

fn part1(input: &String) -> String {
    let mut floor = 0;
    for c in input.chars() {
        if c == '(' {
            floor += 1;
        } else {
            floor -= 1;
        }
    }
    return floor.to_string();
}

fn part2(input: &String) -> String {
    let mut floor = 0;
    let mut count = 0;
    for c in input.chars() {
        if c == '(' {
            floor += 1;
        } else {
            floor -= 1;
        }
        count += 1;
        if floor == -1 {
            return count.to_string();
        }
    }
    return count.to_string();
}

fn main() {
    let input = fs::read_to_string("input")
        .expect("Could not read file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
