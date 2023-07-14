const std = @import("std");
const util = @import("util");

pub fn priority(char: u8) u8 {
    if (char <= 'Z') {
        return char - 'A' + 26;
    } else {
        return char - 'a';
    }
}

pub fn compareRucksacks(ruckSack: []const u8, matches: *[52]bool) u32 {
    const halfLength = (ruckSack.len / 2);
    // put the first half of the rucksack into the uniqueItems array
    var i: u8 = 0;
    while (i < halfLength) {
        matches[priority(ruckSack[i])] = true;
        i += 1;
    }
    var matchingSum: u32 = 0;
    // check the second half of the rucksack against the uniqueItems array
    while (i < ruckSack.len) {
        if (matches[priority(ruckSack[i])]) {
            matchingSum += priority(ruckSack[i]) + 1;
            break;
        }
        i += 1;
    }
    // clear the matches array
    i = 0;
    while (i < halfLength) {
        matches[priority(ruckSack[i])] = false;
        i += 1;
    }

    return matchingSum;
}

pub fn filterRucksack(ruckSack: []const u8, intersects: *[52]bool) void {
    var ruckSackContains: [52]bool = [_]bool{false} ** 52;
    for (ruckSack) |char| {
        ruckSackContains[priority(char)] = true;
    }

    // BITWISE AND each array
    var i: u8 = 0;
    while (i < 52) : (i += 1) {
        intersects[i] = intersects[i] and ruckSackContains[i];
    }
}

pub fn findFirstTrue(arr: *[52]bool) !u8 {
    var i: u8 = 0;
    // the array is guaranteed to have at least one true value
    while (i < arr.len) : (i += 1) {
        if (arr[i]) {
            return i;
        }
    }
    return error.InvalidInput;
}

pub fn main() !void {
    var uniqueItems: [52]bool = [_]bool{false} ** 52;
    var threeIntersect: [52]bool = [_]bool{true} ** 52;
    var lines = std.mem.splitScalar(u8, (try util.readDayInput(3)), '\n');
    var part1: u32 = 0;
    var part2: u32 = 0;

    var mod3: u8 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        filterRucksack(line, &threeIntersect);
        part1 += compareRucksacks(line, &uniqueItems);

        if (mod3 == 2) {
            part2 += try findFirstTrue(&threeIntersect) + 1;
            threeIntersect = [_]bool{true} ** 52;
        }

        mod3 = @mod(mod3 + 1, 3);
    }
    std.debug.print("Part 1: {}\n", .{part1});
    std.debug.print("Part 2: {}\n", .{part2});
}
