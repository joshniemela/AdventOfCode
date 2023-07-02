const std = @import("std");
const kib = 1024;

pub fn readFile(path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const stat = try file.stat();
    const fileSize = stat.size;
    return try file.reader().readAllAlloc(allocator, fileSize);
}

pub fn readDayInput(day: u32) ![]const u8 {
    var buf: [16 * kib]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const allocator = fba.allocator();

    // pad day with 0 if needed
    var path = try std.fmt.allocPrint(allocator, "../inputs/{:0>2}", .{day});
    defer allocator.free(path);
    return readFile(path, allocator);
}

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
    // print pwd
    // var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    // std.debug.print("PWD: {s}\n", .{try std.os.getcwd(&buf)});

    // get time
    var start = std.time.microTimestamp();
    var lines = std.mem.splitScalar(u8, (try readDayInput(1)), '\n');
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

    std.debug.print("Time: {}µs\n", .{(std.time.microTimestamp() - start)});
}

test "day01" {
    var lines = std.mem.splitScalar(u8, (try readFile("src/test-input", std.heap.page_allocator)), '\n');
    var best_of_one: u32 = 0;
    var current_sum: u32 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            if (current_sum > best_of_one) {
                best_of_one = current_sum;
            }
            current_sum = 0;
        } else {
            var value = try std.fmt.parseUnsigned(u32, line, 10);
            current_sum += value;
        }
    }
    std.debug.assert(best_of_one == 24000);
}
