use std::fs;

type Dims = (i32, i32, i32);

fn parse_dims(line: &str) -> Dims {
    let dims: Vec<i32> = line.split('x')
        .map(|x| x.parse::<i32>()
        .expect("Expected integer"))
        .collect();
    (dims[0], dims[1], dims[2])
}

fn to_paper(line: &str) -> i32 {
    let (dim1, dim2, dim3) = parse_dims(line);
    let a = dim1*dim2;
    let b = dim2*dim3;
    let c = dim3*dim1;
    let sides = [a,b,c];
    let smallest_side = sides.iter().min().unwrap();
    2*a + 2*b + 2*c + smallest_side
}

fn part1(input: &String) -> String {
    let paper: i32 = input.lines().map(to_paper).sum();
    paper.to_string()
}

fn to_ribbon(line: &str) -> i32 {
    let (dim1, dim2, dim3) = parse_dims(line);
    let perims = [2*dim1+2*dim2, 2*dim1+2*dim3, 2*dim2+2*dim3];
    let smallest_perim = perims.iter().min().unwrap();
    let volume = dim1*dim2*dim3;
    smallest_perim + volume
}

fn part2(input: &String) -> String {
    let ribbon: i32 = input.lines().map(to_ribbon).sum();
    ribbon.to_string()
}

fn main() {
    let input = fs::read_to_string("input")
        .expect("Could not read file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
