use std::fs;
use md5;

fn has_leading_zeros(string: String, n: usize) -> bool {
    string.chars()
        .enumerate()
        .filter(|(i,_)| *i < n)
        .all(|(_, c)| c == '0')
}

fn find_md5_hash_with_leading_zeros(input: &String, num_leading_zeros: usize) -> String {
    let prefix = input.as_bytes();
    let mut n = 1;
    loop {
        let n_string = n.to_string();
        let suffix = n_string.as_bytes();
        let key = [prefix, suffix].concat();
        let digest = md5::compute(key);
        let digest_hex = format!("{:x}", digest);
        if has_leading_zeros(digest_hex, num_leading_zeros) {
            return n.to_string();
        }
        n += 1;
    }
}

fn part1(input: &String) -> String {
    find_md5_hash_with_leading_zeros(input, 5)
}


fn part2(input: &String) -> String {
    find_md5_hash_with_leading_zeros(input, 6)
}

fn main() {
    let input = fs::read_to_string("input")
        .expect("Could not read file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}