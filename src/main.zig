const Instruction = @import("instruction.zig");
const CPU = @import("cpu.zig");
const Memory = @import("memory.zig");
const Catridge = @import("catridge.zig");
const std = @import("std");

pub fn main() !void {
    //test_cpu();
    const c = try test_catridge();
    std.debug.print("Catridge {any}", .{c});
    std.debug.print("Title {c}", .{c.title});
    std.debug.print("Compiled ok!", .{});
}

fn test_catridge() Catridge.CatridgeError!Catridge.Catridge {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const path = "C:\\Users\\frank\\source\\repos\\zigboy\\gb-test-roms-master\\cpu_instrs\\cpu_instrs.gb";
    const allocator = gpa.allocator();
    return Catridge.Catridge.init(path, allocator);
}

fn test_cpu() void {
    const memory: Memory.Memory = Memory.Memory.init();
    const registers: CPU.Registers = .{
        .a = 0,
        .b = 0,
        .c = 0,
        .d = 0,
        .e = 0,
        .h = 0,
        .l = 0,
    };
    const flags: CPU.Flags = .{
        .carry = false,
        .half_carry = false,
        .subtract = false,
        .zero = false,
    };
    var cpu: CPU.CPU = CPU.CPU.init(registers, flags, memory);
    _ = cpu.execute(0);
}
