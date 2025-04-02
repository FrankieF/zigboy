const Instruction = @import("instruction.zig");
const CPU = @import("cpu.zig");
const Memory = @import("memory.zig");
const std = @import("std");

pub fn main() !void {
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
    std.debug.print("Compiled ok!", .{});
}
