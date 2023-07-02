const std = @import("std");
const util = @import("util");

pub fn rockPaperScissors1(line: *const [3]u8) u8 {
    var opponent: u8 = line[0];
    var you: u8 = line[2];
    return you - 'X' + 3 * ((you - opponent - 1) % 3) + 1;
}

pub fn rockPaperScissors2(line: *const [3]u8) u8 {
    var opponent: u8 = line[0];
    var you: u8 = line[2];
    return 3 * (you - 'X') + ((you + opponent - 1) % 3 ) + 1;
}

pub fn main() !void {
    var lines = std.mem.splitScalar(u8, (try util.readDayInput(2)), '\n');
    var part1: u32 = 0;
    var part2: u32 = 0;
    while (lines.next()) |line| {
        // ensure that the line is exactly 3 bytes long
        if (line.len == 0) {
            continue;
        }
        part1 += rockPaperScissors1(line[0..3]);
        part2 += rockPaperScissors2(line[0..3]);
    }
    std.debug.print("Part 1: {}\n", .{part1});
    std.debug.print("Part 2: {}\n", .{part2});
}
