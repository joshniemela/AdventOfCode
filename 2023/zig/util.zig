const std = @import("std");
const kib = 1024;

pub fn readFile(path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const stat = try file.stat();
    const fileSize = stat.size;
    return try file.reader().readAllAlloc(allocator, fileSize);
}

pub fn readInput(day: u32) ![]const u8 {
    var buf: [64 * kib]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const allocator = fba.allocator();

    // pad day with 0 if needed
    var path = try std.fmt.allocPrint(allocator, "../inputs/{:0>2}", .{day});
    defer allocator.free(path);
    return readFile(path, allocator);
}

pub fn readTestInput(day: u32) ![]const u8 {
    var buf: [8 * kib]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const allocator = fba.allocator();

    // pad day with 0 if needed
    var path = try std.fmt.allocPrint(allocator, "../test_inputs/{:0>2}", .{day});
    defer allocator.free(path);
    return readFile(path, allocator);
}
