use std::fs;

fn to_paper(line: &str) -> i32 {
    let dims: Vec<i32> = line.split('x')
        .map(|x| x.parse::<i32>()
        .expect("Expected integer"))
        .collect();
    let a = dims[0]*dims[1];
    let b = dims[1]*dims[2];
    let c = dims[2]*dims[0];
    let sides = [a,b,c];
    let smallest_side = sides.iter().min().unwrap();
    2*a + 2*b + 2*c + smallest_side
}

fn part1(input: &String) -> String {
    let paper: i32 = input.lines().map(to_paper).sum();
    paper.to_string()
}

fn part2(input: &String) -> String {
    return String::from("hello");
}

fn main() {
    let input = fs::read_to_string("input")
        .expect("Could not read file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
