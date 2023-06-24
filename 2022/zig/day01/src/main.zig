const std = @import("std");
const max = std.math.maxInt(u32);

pub fn update_best_of_three(best_of_three: *[3]u32, current_max: u32) void {
    if (current_max > best_of_three[0]) {
        best_of_three[2] = best_of_three[1];
        best_of_three[1] = best_of_three[0];
        best_of_three[0] = current_max;
    } else if (current_max > best_of_three[1]) {
        best_of_three[2] = best_of_three[1];
        best_of_three[1] = current_max;
    } else if (current_max > best_of_three[2]) {
        best_of_three[2] = current_max;
    }
}

pub fn main() !void {
    var lines = std.mem.splitScalar(u8, @embedFile("input"), '\n');

    var best_of_three = [3]u32{ 0, 0, 0 };

    var current_sum: u32 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            update_best_of_three(&best_of_three, current_sum);
            current_sum = 0;
        } else {
            var value = try std.fmt.parseUnsigned(u32, line, 10);
            current_sum += value;
        }
    }
    std.debug.print("Best of three are: {}\n", .{@as(@Vector(3, u32), best_of_three)});
    std.debug.print("Sum of three is: {}\n", .{@reduce(.Add, @as(@Vector(3, u32), best_of_three))});
}
