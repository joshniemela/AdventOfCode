const std = @import("std");

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

    var day: u8 = 1;
    while (day <= 25) : (day += 1) {
        const dayString = b.fmt("day{:0>2}", .{day});
        const sourceDir = b.fmt("src/{s}", .{dayString});
        const sourceFile = b.fmt("{s}/main.zig", .{sourceDir});
        // If the sourceFile does not exist, then we skip
        var cwd = std.fs.cwd().openDir(".", .{ .access_sub_paths = true }) catch {
            std.debug.print("Failed to open current directory\n", .{});
            continue;
        };
        const dayDir = cwd.openDir(dayString, .{ .access_sub_paths = true }) catch continue;
        _ = dayDir;

        const exe = b.addExecutable(.{
            .name = "zig",
            .root_source_file = b.path(sourceFile),
            .target = target,
            .optimize = optimize,
        });

        exe.root_module.addImport("util", b.createModule(.{
            .root_source_file = b.path("src/util.zig"),
        }));

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

        // This creates a build step. It will be visible in the `zig build --help` menu,
        // and can be selected like this: `zig build run`
        // This will evaluate the `run` step rather than the default, which is "install".
        const run_step_name = b.fmt("run_{s}", .{dayString});
        const run_step_desc = b.fmt("Run the executable for day {s}", .{dayString});
        const run_step = b.step(run_step_name, run_step_desc);
        run_step.dependOn(&run_cmd.step);

        const exe_unit_tests = b.addTest(.{
            .root_source_file = b.path(sourceFile),
            .target = target,
            .optimize = optimize,
        });

        const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

        // Similar to creating the run step earlier, this exposes a `test` step to
        // the `zig build --help` menu, providing a way for the user to request
        // running the unit tests.
        const test_step_name = b.fmt("test_{s}", .{dayString});
        const test_step_desc = b.fmt("Run unit tests for day {s}", .{dayString});
        const test_step = b.step(test_step_name, test_step_desc);
        test_step.dependOn(&run_exe_unit_tests.step);
    }
}
