const std = @import("std");

const MAX_DAYS: u8 = 25;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // `b.option()` returns a `?T` - use `orelse` to provide a default value
    const day: u8 = b.option(u8, "day", "Which day to run") orelse 1;
    if (day > MAX_DAYS) @panic("Day option must be from 1-25 (inclusive).");

    const root_source_file = b.fmt("src/day{:0>2}/main.zig", .{day});

    const run_step = b.step("run", "Run a day's executable");

    const exe = b.addExecutable(.{
        .name = b.fmt("day_{:0>2}", .{day}),
        .root_source_file = b.path(root_source_file),
        .target = target,
        .optimize = optimize,
    });

    exe.root_module.addImport("util", b.createModule(.{
        .root_source_file = b.path("src/util.zig"),
    }));

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run a day's unit tests");

    const exe_test = b.addTest(.{
        .name = b.fmt("day_{:0>2}", .{day}),
        .root_source_file = b.path(root_source_file),
        .target = target,
        .optimize = optimize,
    });

    test_step.dependOn(&exe_test.step);
}
