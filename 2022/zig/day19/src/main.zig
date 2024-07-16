const std = @import("std");
const util = @import("../util.zig");
const Timer = std.time.Timer;

const Resources = packed struct {
    ore: u8,
    clay: u8,
    obsidian: u8,
    geodes: u8,
    ore_bots: u8,
    clay_bots: u8,
    obsidian_bots: u8,
    geode_bots: u8,
};

const State = struct {
    resources: Resources,
    minutes: u8,

    fn can_build_ore_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.ore_bot_ore_cost;
    }
    fn can_build_clay_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.clay_bot_ore_cost;
    }
    fn can_build_obsidian_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.obsidian_bot_ore_cost and
            self.resources.clay >= bp.obsidian_bot_clay_cost;
    }
    fn can_build_geode_bot(self: State, bp: *const Blueprint) bool {
        return self.resources.ore >= bp.geode_bot_ore_cost and
            self.resources.obsidian >= bp.geode_bot_obsidian_cost;
    }

    fn build_ore_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.resources.ore -= bp.ore_bot_ore_cost;
        new_state.resources.ore_bots += 1;
        return new_state;
    }

    fn build_clay_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.resources.ore -= bp.clay_bot_ore_cost;
        new_state.resources.clay_bots += 1;
        return new_state;
    }

    fn build_obsidian_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.resources.ore -= bp.obsidian_bot_ore_cost;
        new_state.resources.clay -= bp.obsidian_bot_clay_cost;
        new_state.resources.obsidian_bots += 1;
        return new_state;
    }

    fn build_geode_bot(self: State, bp: *const Blueprint) State {
        var new_state = self;
        new_state.resources.ore -= bp.geode_bot_ore_cost;
        new_state.resources.obsidian -= bp.geode_bot_obsidian_cost;
        new_state.resources.geode_bots += 1;
        return new_state;
    }

    fn step(self: State) State {
        var new_state = self;
        new_state.minutes += 1;
        new_state.resources.ore += self.resources.ore_bots;
        new_state.resources.clay += self.resources.clay_bots;
        new_state.resources.obsidian += self.resources.obsidian_bots;
        new_state.resources.geodes += self.resources.geode_bots;
        return new_state;
    }

    fn upper_bound_geodes(self: State, minutes_limit: u8) u8 {
        const time_left = minutes_limit - self.minutes;
        // The bound is the current number of geodes,
        // plus the number of geodes that can be made by the existing geode bots,
        // plus the number of geodes that can be made by future geode bots assuming one is made each minute.
        // If we are further than 16 minutes away, we assume at most 255 geodes can be made.
        if (time_left >= 16) {
            return 255;
        }
        return self.resources.geodes + self.resources.geode_bots * time_left + time_left * (time_left - 1) / 2;
    }
};

//const StateContext = struct {
//    pub fn hash(_: StateContext, key: State) u64 {
//        var h = std.hash.Fnv32a.init();
//        h.update(key);
//        return h.final();
//    }
//
//    pub fn eql(_: StateContext, a: State, b: State) bool {
//        // check if they look the same when cast to 64
//        const a64: u64 = @bitCast(a);
//        const b64: u64 = @bitCast(b);
//        return a64 == b64;
//    }
//};

const GemMinutes = struct {
    geodes: u8,
    minutes: u8,
};
const StateHashMap = std.HashMap(Resources, GemMinutes, std.hash_map.AutoContext(Resources), std.hash_map.default_max_load_percentage);

const Blueprint = struct {
    ore_bot_ore_cost: u8,
    clay_bot_ore_cost: u8,
    obsidian_bot_ore_cost: u8,
    obsidian_bot_clay_cost: u8,
    geode_bot_ore_cost: u8,
    geode_bot_obsidian_cost: u8,
};

pub fn solution(state: State, bp: *const Blueprint, minutes_limit: u8, memo: *StateHashMap, best_geodes: *u8) !u8 {
    // If we have reached the time limit, return the number of geodes we have
    if (state.minutes >= minutes_limit) {
        return state.resources.geodes;
    }
    // No matter what, this state cannot exceed the upper bound of geode and can be discarded
    if (state.upper_bound_geodes(minutes_limit) <= best_geodes.*) {
        return 0;
    }

    // If it is in the cache, return the result
    if (memo.contains(state.resources)) {
        const memo_res = memo.get(state.resources).?;
        // If the result from cache reached the same state faster or equally as fast, return the result
        if (memo_res.minutes <= state.minutes) {
            return memo_res.geodes;
        }
    }

    // Find the maximum number of geodes that can be made by searching all options
    var max_geodes = state.resources.geodes;
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

    try memo.put(state.resources, GemMinutes{ .geodes = max_geodes, .minutes = state.minutes });

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

    const starting_resources = Resources{
        .ore = 0,
        .clay = 0,
        .obsidian = 0,
        .geodes = 0,
        .ore_bots = 1,
        .clay_bots = 0,
        .obsidian_bots = 0,
        .geode_bots = 0,
    };

    const starting_state = State{
        .minutes = 0,
        .resources = starting_resources,
    };

    var memo = StateHashMap.init(allocator);
    var temp: u8 = 0;

    const best_geodes: *u8 = &temp;
    const result = solution(starting_state, &testBlueprint, 24, &memo, best_geodes);
    // read and print in seconds (it starts as nano seconds)
    const elapsed = timer.read() / 1_000_000;
    std.debug.print("Elapsed: {} ms\n", .{elapsed});
    try std.testing.expectEqual(expected, result);
}
