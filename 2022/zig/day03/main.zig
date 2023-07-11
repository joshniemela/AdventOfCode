const std = @import("std");
const util = @import("util");

pub fn hashFn(char: u8) u8 {
    if (char <= 'Z') {
        return char - 'A' + 26;
    } else {
        return char - 'a';
    }
}

var uniqueItems: [52]bool = std.mem.zeroes([52]bool);
pub fn compareRucksacks(ruckSack: []const u8) u32 {
    const halfLength = (ruckSack.len / 2);
    // put the first half of the rucksack into the uniqueItems array
    var i: u8 = 0;
    while (i < halfLength) {
        uniqueItems[hashFn(ruckSack[i])] = true;
        i += 1;
    }
    var matchingSum: u32 = 0;
    // check the second half of the rucksack against the uniqueItems array
    while (i < ruckSack.len) {
        if (uniqueItems[hashFn(ruckSack[i])]) {
            matchingSum += hashFn(ruckSack[i]) + 1;
            break;
        }
        i += 1;
    }
    uniqueItems = std.mem.zeroes([52]bool);
    return matchingSum;
}

pub fn main() !void {
    var lines = std.mem.splitScalar(u8, (try util.readDayInput(3)), '\n');
    var part1: u32 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        part1 += compareRucksacks(line);
    }
    std.debug.print("Part 1: {}\n", .{part1});
}
