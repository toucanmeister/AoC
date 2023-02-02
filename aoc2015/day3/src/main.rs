use std::fs;

type Point = (i32, i32);
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn do_move(direction: &Direction, (x,y): Point) -> Point {
    match direction {
        Direction::Up => (x, y+1),
        Direction::Down => (x, y-1),
        Direction::Left => (x-1, y),
        Direction::Right => (x+1, y),
    }
}

fn parse_directions(input: &String) -> Vec<Direction> {
    let to_direction = |c| 
        match c {
            '^' => Direction::Up,
            'v' => Direction::Down,
            '<' => Direction::Left,
            '>' => Direction::Right,
            _ => panic!("Expected ^,v,< or >"),
        };
    input.chars().map(to_direction).collect()
}

fn part1(input: &String) -> String {
    let directions = parse_directions(input);
    let mut current: Point = (0,0);
    let mut visited: Vec<Point> = Vec::from([(0,0)]);
    for direction in directions.iter() {
        current = do_move(direction, current);
        if !visited.contains(&current) {
            visited.push(current.clone());
        }
    }
    visited.len().to_string()
}

fn part2(input: &String) -> String {
    String::from("hi")
}


fn main() {
    let input = fs::read_to_string("input")
        .expect("Could not read file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}