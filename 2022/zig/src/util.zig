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
    var buf: [64 * kib]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const allocator = fba.allocator();

    // pad day with 0 if needed
    const path = try std.fmt.allocPrint(allocator, "../inputs/{:0>2}", .{day});
    defer allocator.free(path);
    return readFile(path, allocator);
}

// data structures

// N-Dimensional vector with elements of type T
pub fn NCoord(comptime dim: u32, comptime T: type) type {
    return @Vector(dim, T);
}

// Get the nth component of a vector, x y z w etc
// v = NCoord(3, f32)
// v.x = 1.0
// v.y = 2.0
// v.z = 3.0
//
// getComponent(v, 0) == 1.0
pub fn getComponent(v: NCoord, n: u32) v.T {
    return v[n];
}

pub fn getX(v: NCoord) v.T {
    return v[0];
}

pub fn getY(v: NCoord) v.T {
    return v[1];
}

pub fn getZ(v: NCoord) v.T {
    return v[2];
}

// create a 2D grid of type T
pub fn Grid(comptime T: type) type {
    return struct {
        const Self = @This();
        grid: []T,
    };
}
