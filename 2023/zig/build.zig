const std = @import("std");
const fs = std.fs;

pub fn setup_day(
    b: *std.Build,
    target: std.zig.CrossTarget,
    optimize: std.builtin.Mode,
    day: u32,
) void {
    const path = b.fmt("day{:0>2}", .{day});
    const root_src = b.fmt("{s}/main.zig", .{path});

    const exe = b.addExecutable(.{
        .name = path,
        .root_source_file = .{ .path = root_src },
        .target = target,
        .optimize = optimize,
    });

    const util = b.addModule("util", .{
        .source_file = .{ .path = "util.zig" },
    });
    exe.addModule("util", util);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step(b.fmt("run{:0>2}", .{day}), "Run the executable for a given day");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = root_src },
        .target = target,
        .optimize = optimize,
    });
    unit_tests.addModule("util", util);

    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step(b.fmt("test{:0>2}", .{day}), "Run unit tests for a given day");
    test_step.dependOn(&run_unit_tests.step);
}

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const days_dir = fs.cwd().openIterableDir(".", .{}) catch unreachable;
    var iter_dir = days_dir.iterate();
    while (iter_dir.next() catch unreachable) |entry| {
        //std.debug.print("entry: {s}\n", .{entry.name}); //exists for debugging
        if (std.mem.startsWith(u8, entry.name, "day") and (entry.kind == .directory)) {
            const day = std.fmt.parseInt(u32, entry.name[3..], 10) catch unreachable;
            setup_day(b, target, optimize, day);
        }
    }
}
