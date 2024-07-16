const std = @import("std");
const util = @import("../util.zig");
const Timer = std.time.Timer;

const State = struct {
    minutes: u32,
    ore: u32,
    clay: u32,
    obsidian: u32,
    geodes: u32,
    ore_bots: u32,
    clay_bots: u32,
    obsidian_bots: u32,
    geode_bots: u32,

    fn can_build_ore_bot(self: State, bp: *const Blueprint) bool {
        return self.ore >= bp.ore_bot_ore_cost;
    }
    fn can_build_clay_bot(self: State, bp: *const Blueprint) bool {
        return self.ore >= bp.clay_bot_ore_cost;
    }
    fn can_build_obsidian_bot(self: State, bp: *const Blueprint) bool {
        return self.ore >= bp.obsidian_bot_ore_cost and self.clay >= bp.obsidian_bot_clay_cost;
    }
    fn can_build_geode_bot(self: State, bp: *const Blueprint) bool {
        return self.ore >= bp.geode_bot_ore_cost and self.obsidian >= bp.geode_bot_obsidian_cost;
    }

    fn build_ore_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.ore -= bp.ore_bot_ore_cost;
        new_state.ore_bots += 1;
        return new_state;
    }

    fn build_clay_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.ore -= bp.clay_bot_ore_cost;
        new_state.clay_bots += 1;
        return new_state;
    }

    fn build_obsidian_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.ore -= bp.obsidian_bot_ore_cost;
        new_state.clay -= bp.obsidian_bot_clay_cost;
        new_state.obsidian_bots += 1;
        return new_state;
    }

    fn build_geode_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.ore -= bp.geode_bot_ore_cost;
        new_state.obsidian -= bp.geode_bot_obsidian_cost;
        new_state.geode_bots += 1;
        return new_state;
    }

    fn step(self: State) State {
        var new_state = self;
        new_state.minutes += 1;
        new_state.ore += self.ore_bots;
        new_state.clay += self.clay_bots;
        new_state.obsidian += self.obsidian_bots;
        new_state.geodes += self.geode_bots;
        return new_state;
    }

    fn upper_bound_geodes(self: State, minutes_limit: u32) u32 {
        const time_left = minutes_limit - self.minutes;
        // The bound is the current number of geodes,
        // plus the number of geodes that can be made by the existing geode bots,
        // plus the number of geodes that can be made by future geode bots assuming one is made each minute.
        return self.geodes + self.geode_bots * time_left + time_left * (time_left - 1) / 2;
    }
};

const StateHashMap = std.AutoHashMap(State, u32);

const Blueprint = struct {
    ore_bot_ore_cost: u32,
    clay_bot_ore_cost: u32,
    obsidian_bot_ore_cost: u32,
    obsidian_bot_clay_cost: u32,
    geode_bot_ore_cost: u32,
    geode_bot_obsidian_cost: u32,
};

pub fn solution(state: State, bp: *const Blueprint, minutes_limit: u32, memo: *StateHashMap, best_geodes: *u32) !u32 {
    if (state.minutes >= minutes_limit) {
        return state.geodes;
    }
    // No matter what, this state cannot exceed the upper bound of geode and can be discarded
    if (state.upper_bound_geodes(minutes_limit) <= best_geodes.*) {
        return 0;
    }
    if (memo.contains(state)) {
        return memo.get(state).?;
    }
    // Find the maximum number of geodes that can be made
    var max_geodes = state.geodes;
    if (state.can_build_geode_bot(bp)) {
        max_geodes = @max(max_geodes, try solution(state.step().build_geode_bot(bp), bp, minutes_limit, memo, best_geodes));
    }
    if (state.can_build_obsidian_bot(bp)) {
        max_geodes = @max(max_geodes, try solution(state.step().build_obsidian_bot(bp), bp, minutes_limit, memo, best_geodes));
    }
    if (state.can_build_clay_bot(bp)) {
        max_geodes = @max(max_geodes, try solution(state.step().build_clay_bot(bp), bp, minutes_limit, memo, best_geodes));
    }
    if (state.can_build_ore_bot(bp)) {
        max_geodes = @max(max_geodes, try solution(state.step().build_ore_bot(bp), bp, minutes_limit, memo, best_geodes));
    }
    max_geodes = @max(max_geodes, try solution(state.step(), bp, minutes_limit, memo, best_geodes));

    try memo.put(state, max_geodes);

    if (max_geodes > best_geodes.*) {
        best_geodes.* = max_geodes;
    }
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
    const allocator = std.heap.page_allocator;
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

    const starting_state = State{
        .minutes = 0,
        .ore = 0,
        .clay = 0,
        .obsidian = 0,
        .geodes = 0,
        .ore_bots = 1,
        .clay_bots = 0,
        .obsidian_bots = 0,
        .geode_bots = 0,
    };

    var memo = StateHashMap.init(allocator);
    var temp: u32 = 0;

    const best_geodes: *u32 = &temp;
    const result = solution(starting_state, &testBlueprint, 24, &memo, best_geodes);
    // read and print in seconds (it starts as nano seconds)
    const elapsed = timer.read() / 1_000_000;
    std.debug.print("Elapsed: {} ms\n", .{elapsed});
    try std.testing.expectEqual(expected, result);
}
