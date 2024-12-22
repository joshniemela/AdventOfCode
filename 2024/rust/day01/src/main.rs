use anyhow::Result;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
fn read_input<P>(path: P) -> Result<([i32; 1000], [i32; 1000])>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut lefts = [0; 1000];
    let mut rights = [0; 1000];
    // We have a number, then three spaces, then a number
    for (i, line) in reader.lines().flatten().enumerate() {
        let mut parts = line.split("   ");
        let left = parts.next().unwrap().parse::<i32>()?;
        let right = parts.next().unwrap().parse::<i32>()?;
        lefts[i] = left;
        rights[i] = right;
    }
    Ok((lefts, rights))
}

// Precondition: lefts and rights are sorted
fn part1(lefts: [i32; 1000], rights: [i32; 1000]) -> i32 {
    let mut diffs = 0;
    for (left, right) in lefts.iter().zip(rights.iter()) {
        diffs += (left - right).abs();
    }
    diffs
}

// Precondition: lefts and rights are sorted
fn part2(lefts: [i32; 1000], rights: [i32; 1000]) -> i32 {
    let mut similarity_score = 0;

    let mut left_index = 0;
    let mut right_index = 0;

    // Bounds check
    while left_index < lefts.len() && right_index < rights.len() {
        match lefts[left_index].cmp(&rights[right_index]) {
            std::cmp::Ordering::Less => {
                // Left is less than right
                left_index += 1;
            }
            std::cmp::Ordering::Greater => {
                // Right is less than left
                right_index += 1;
            }
            std::cmp::Ordering::Equal => {
                // We have a match
                similarity_score += lefts[left_index];
                right_index += 1;
            }
        }
    }

    similarity_score
}

fn main() {
    let (mut lefts, mut rights) = read_input("../../inputs/01").unwrap();
    // Sort both arrays
    lefts.sort_unstable();
    rights.sort_unstable();

    println!("Part 1: {}", part1(lefts, rights));

    println!("Part 2: {}", part2(lefts, rights));
}
