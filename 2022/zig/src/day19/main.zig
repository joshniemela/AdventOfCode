const std = @import("std");
const util = @import("../util.zig");
const Timer = std.time.Timer;

const Resources = packed struct {
    ore: u8,
    clay: u8,
    obsidian: u8,
    geodes: u8,
    ore_bots: u4,
    clay_bots: u4,
    obsidian_bots: u4,
};

const Blueprint = struct {
    ore_bot_ore_cost: u8,
    clay_bot_ore_cost: u8,
    obsidian_bot_ore_cost: u8,
    obsidian_bot_clay_cost: u8,
    geode_bot_ore_cost: u8,
    geode_bot_obsidian_cost: u8,
};

const MayBuild = struct {
    max_ore_bots: u8,
    max_clay_bots: u8,
    max_obsidian_bots: u8,
    fn new(bp: *const Blueprint) MayBuild {
        // The max amount of bots to build is the one where any bot can be made in 1 turn
        var max_ore_cost: u8 = 0;
        var max_clay_cost: u8 = 0;
        var max_obsidian_cost: u8 = 0;
        max_ore_cost = @max(max_ore_cost, bp.ore_bot_ore_cost);
        max_ore_cost = @max(max_ore_cost, bp.clay_bot_ore_cost);
        max_ore_cost = @max(max_ore_cost, bp.obsidian_bot_ore_cost);
        max_ore_cost = @max(max_ore_cost, bp.geode_bot_ore_cost);
        max_clay_cost = @max(max_clay_cost, bp.obsidian_bot_clay_cost);
        max_obsidian_cost = @max(max_obsidian_cost, bp.geode_bot_obsidian_cost);

        return MayBuild{
            .max_ore_bots = max_ore_cost,
            .max_clay_bots = max_clay_cost,
            .max_obsidian_bots = max_obsidian_cost,
            // No cap on geode bots
        };
    }
};

var states_hit: u64 = 0;

const State = struct {
    resources: Resources,
    buildable: MayBuild,
    minutes: u8,

    fn can_build_ore_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.ore_bot_ore_cost and
            self.resources.ore_bots < self.buildable.max_ore_bots;
    }

    fn can_build_clay_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.clay_bot_ore_cost and
            self.resources.clay_bots < self.buildable.max_clay_bots;
    }

    fn can_build_obsidian_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.obsidian_bot_ore_cost and
            self.resources.clay >= bp.obsidian_bot_clay_cost and
            self.resources.obsidian_bots < self.buildable.max_obsidian_bots;
    }

    fn can_build_geode_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.geode_bot_ore_cost and
            self.resources.obsidian >= bp.geode_bot_obsidian_cost;
    }

    fn build_ore_bot(self: State, bp: *const Blueprint, minutes_limit: u8) State {
        var new_state = self;
        // Wait until we either run out of time or until we can build a geode bot
        while (!new_state.can_build_ore_bot(bp)) {
            new_state = new_state.step();
            if (new_state.minutes >= minutes_limit) {
                return new_state;
            }
        }
        new_state = new_state.step();
        new_state.resources.ore -= bp.ore_bot_ore_cost;
        new_state.resources.ore_bots += 1;
        return new_state;
    }

    fn build_clay_bot(self: State, bp: *const Blueprint, minutes_limit: u8) State {
        var new_state = self;
        while (!new_state.can_build_clay_bot(bp)) {
            new_state = new_state.step();
            if (new_state.minutes >= minutes_limit) {
                return new_state;
            }
        }
        new_state = new_state.step();
        new_state.resources.ore -= bp.clay_bot_ore_cost;
        new_state.resources.clay_bots += 1;
        return new_state;
    }

    fn build_obsidian_bot(self: State, bp: *const Blueprint, minutes_limit: u8) State {
        var new_state = self;
        while (!new_state.can_build_obsidian_bot(bp)) {
            new_state = new_state.step();
            if (new_state.minutes >= minutes_limit) {
                return new_state;
            }
        }
        new_state = new_state.step();
        new_state.resources.ore -= bp.obsidian_bot_ore_cost;
        new_state.resources.clay -= bp.obsidian_bot_clay_cost;
        new_state.resources.obsidian_bots += 1;
        return new_state;
    }

    fn build_geode_bot(self: State, bp: *const Blueprint, minutes_limit: u8) State {
        var new_state = self;
        while (!new_state.can_build_geode_bot(bp)) {
            new_state = new_state.step();
            if (new_state.minutes >= minutes_limit) {
                return new_state;
            }
        }
        new_state = new_state.step();
        new_state.resources.ore -= bp.geode_bot_ore_cost;
        new_state.resources.obsidian -= bp.geode_bot_obsidian_cost;
        const time_left = minutes_limit - new_state.minutes;
        new_state.resources.geodes += time_left;
        return new_state;
    }

    fn step(self: State) State {
        var new_state = self;
        new_state.minutes += 1;
        new_state.resources.ore += self.resources.ore_bots;
        new_state.resources.clay += self.resources.clay_bots;
        new_state.resources.obsidian += self.resources.obsidian_bots;
        return new_state;
    }

    fn upper_bound_geodes(self: State, minutes_limit: u8) u8 {
        const time_left = minutes_limit - self.minutes;
        // The bound is the current number of geodes,
        // plus the number of geodes that can be made by the existing geode bots,
        // plus the number of geodes that can be made by future geode bots assuming one is made each minute.
        // If we are further than 16 minutes away, we assume at most 255 geodes can be made.
        if (time_left > 16) {
            return self.resources.geodes + 255;
        }
        return self.resources.geodes +
            time_left * (time_left - 1) / 2;
    }
};

pub fn solution(state: State, bp: *const Blueprint, minutes_limit: u8, best_geodes: *u8) !u8 {
    // If we have reached the time limit, return the number of geodes we have
    if (state.minutes >= minutes_limit) {
        return state.resources.geodes;
    }
    // No matter what, this state cannot exceed the upper bound of geode and can be discarded
    if (state.upper_bound_geodes(minutes_limit) <= best_geodes.*) {
        return 0;
    }

    // Find the maximum number of geodes that can be made by searching all options
    var max_geodes = state.resources.geodes;
    max_geodes = @max(max_geodes, try solution(state.build_geode_bot(bp, minutes_limit), bp, minutes_limit, best_geodes));
    max_geodes = @max(max_geodes, try solution(state.build_obsidian_bot(bp, minutes_limit), bp, minutes_limit, best_geodes));
    max_geodes = @max(max_geodes, try solution(state.build_clay_bot(bp, minutes_limit), bp, minutes_limit, best_geodes));
    max_geodes = @max(max_geodes, try solution(state.build_ore_bot(bp, minutes_limit), bp, minutes_limit, best_geodes));
    if (max_geodes > best_geodes.*) {
        best_geodes.* = max_geodes;
    }
    states_hit += 4;

    return max_geodes;
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

test "test util" {
    var timer = try Timer.start();

    const testBlueprint = Blueprint{
        .ore_bot_ore_cost = 4,
        .clay_bot_ore_cost = 2,
        .obsidian_bot_ore_cost = 3,
        .obsidian_bot_clay_cost = 14,
        .geode_bot_ore_cost = 2,
        .geode_bot_obsidian_cost = 7,
    };
    const expected = 9;

    const starting_resources = Resources{
        .ore = 0,
        .clay = 0,
        .obsidian = 0,
        .geodes = 0,
        .ore_bots = 1,
        .clay_bots = 0,
        .obsidian_bots = 0,
    };

    const starting_buildable = MayBuild.new(&testBlueprint);

    const starting_state = State{
        .minutes = 0,
        .resources = starting_resources,
        .buildable = starting_buildable,
    };

    var temp: u8 = 0;

    const best_geodes: *u8 = &temp;
    const result = solution(starting_state, &testBlueprint, 24, best_geodes);
    // read and print in seconds (it starts as nano seconds)
    const elapsed = timer.read() / 1_000;
    std.debug.print("Elapsed: {} µs\n", .{elapsed});
    std.debug.print("States hit: {}\n", .{states_hit});
    try std.testing.expectEqual(expected, result);
}
