use std::fs;
use std::str;

fn at_least_3_vowels(line: &str) -> bool {
    let vowels = ['a', 'e', 'i', 'o', 'u'];
    line.chars().filter(|c| vowels.contains(c)).count() >= 3
}

fn double_letter(line: &str) -> bool {
    let ascii = line.as_bytes();
    ascii.windows(2).any(|substring| substring[0] == substring[1])
}

fn no_specials(line: &str) -> bool {
    let specials = ["ab", "cd", "pq", "xy"];
    specials.iter().all(|x| !line.contains(x))
}

fn is_nice(line: &&str) -> bool {
    let requirements = [
        at_least_3_vowels(line), 
        double_letter(line), 
        no_specials(line),
    ];
    requirements.iter().fold(true, |x,y| x && *y)
}

fn part1(input: &String) -> String {
    input.lines()
        .filter(is_nice)
        .count()
        .to_string()
}

fn without_pair(line: &str, p: &str) -> String {
    let startindex = line.find(p).unwrap();
    let mut newline = String::from(line);
    newline.remove(startindex);
    newline.remove(startindex); // each remove takes away one half of the pair
    newline.insert(startindex, '_');
    newline.insert(startindex, '_'); // Insert this placeholder to avoid matching something like aabb -> ab
    newline
}

fn double_pair(line: &str) -> bool {
    let ascii = line.as_bytes();
    ascii.windows(2)
        .map(|p| str::from_utf8(p).expect("Expected ascii character"))
        .map(|p| (p, without_pair(line.clone(), p))) // remove the first instance of the pair
        .any(|(p,l)| l.contains(p)) // see if pair exists a second time
}

fn separated_repeat(line: &str) -> bool {
    let ascii = line.as_bytes();
    ascii.windows(3)
        .any(|s| s[0]==s[2])
}

fn is_nice_2(line: &&str) -> bool {
    let requirements = [
        double_pair(line),
        separated_repeat(line),
    ];
    requirements.iter().fold(true, |x,y| x && *y)
}

fn part2(input: &String) -> String {
    input.lines()
        .filter(is_nice_2)
        .count()
        .to_string()
}

fn main() {
    let input = fs::read_to_string("input")
        .expect("Could not read file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}