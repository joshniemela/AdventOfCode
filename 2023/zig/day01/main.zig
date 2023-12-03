const std = @import("std");
const util = @import("util");
//const data = @embedFile("test-input");

pub fn main() !void {
    var data = try util.readInput(1);
    var lines = std.mem.tokenize(u8, data, "\n");
    const sum = part1(&lines);
    std.debug.print("sum: {}\n", .{sum});
}

test "day01-part1" {
    var data = try util.readTestInput(1);
    var lines = std.mem.tokenize(u8, data, "\n");
    const sum = part1(&lines);
    std.debug.print("part1: {}\n", .{sum});
    try std.testing.expect(sum == 142);
}

fn is_numeric(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn parse_as_u8(c: u8) u8 {
    return c - '0';
}
// take a string and find the first and last digit
fn get_start_and_end(s: []const u8) [2]u8 {
    var values: [2]u8 = undefined;
    var i: usize = 0;
    while (i < s.len) {
        if (is_numeric(s[i])) {
            values[0] = parse_as_u8(s[i]);
            break;
        }
        i += 1;
    }
    i = s.len - 1;
    while (i >= 0) {
        if (is_numeric(s[i])) {
            values[1] = parse_as_u8(s[i]);
            break;
        }
        i -= 1;
    }
    return values;
}

fn add_start_end(s: []const u8) u32 {
    var values = get_start_and_end(s);
    return 10 * values[0] + values[1];
}
// take a reference to std.mem.TokenIterator(u8, .any) and return the sum of all the numbers
fn part1(lines: *std.mem.TokenIterator(u8, .any)) u32 {
    var sum: u32 = 0;
    while (lines.next()) |line| {
        sum += add_start_end(line);
    }
    return sum;
}
