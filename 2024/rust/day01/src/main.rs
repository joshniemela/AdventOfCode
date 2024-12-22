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

fn part1(mut lefts: [i32; 1000], mut rights: [i32; 1000]) -> i32 {
    // Sort both arrays
    lefts.sort_unstable();
    rights.sort_unstable();

    let mut diffs = 0;
    for (left, right) in lefts.iter().zip(rights.iter()) {
        diffs += (left - right).abs();
    }
    diffs
}

fn main() {
    let (lefts, rights) = read_input("../../../inputs/01").unwrap();
    println!("Part 1: {}", part1(lefts, rights));
}
