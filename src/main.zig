const CPU = @import("cpu.zig");
const Memory = @import("memory.zig");
const Cartridge = @import("cartridge.zig");
const std = @import("std");
const cpu_test = @import("test.zig");
const Gameboy = @import("gameboy.zig").GameBoy;

const sf = struct {
    const sfml = @import("sfml");
    usingnamespace sfml;
    usingnamespace sfml.audio;
    usingnamespace sfml.graphics;
    usingnamespace sfml.window;
    usingnamespace sfml.system;
};

pub fn callback(value: u8) void {
    _ = value;
}

const TestType = enum {
    All,
    Single,
};

pub const Logs = enum {
    On,
    Off,
};

pub var logs = Logs.Off;
const test_type = TestType.Single;
const logLevel = cpu_test.LogLevel.All;
const test_file = "f5.json";
pub var enabled_logging: bool = false;
pub fn main() !void {
    const c = try open_catridge();
    var gameboy = try Gameboy.init(c);
    var running = true;
    while (running) {
        running = gameboy.update();
    }
    // gameboy.destroy();
}

fn open_catridge() Cartridge.CartridgeError!Cartridge.Cartridge {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const path = "rom";
    const allocator = gpa.allocator();
    return Cartridge.Cartridge.init(path, allocator);
}

fn create_cpu(catridge: Cartridge.Catridge) CPU.CPU {
    const memory: Memory.Memory = Memory.Memory.init(catridge);
    const registers = CPU.Registers.init();
    const flags = CPU.Flags.init();
    return CPU.CPU.init(registers, flags, memory);
}
