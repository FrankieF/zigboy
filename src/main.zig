const Instruction = @import("instruction.zig");
const CPU = @import("cpu.zig");
const Memory = @import("memory.zig");
const Catridge = @import("catridge.zig");
const MBC = @import("mbc.zig");
const std = @import("std");

pub fn main() !void {
    //test_cpu();
    test_mbc();
    // const c = try test_catridge();
    // std.debug.print("Catridge {any}", .{c});
    // std.debug.print("Title {c}", .{c.title});
    std.debug.print("Compiled ok!", .{});
}

fn test_mbc() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const data = [_]u8{ 1, 2, 3, 4, 5 };
    var rom = MBC.Rom.init(data[0..]);
    _ = rom.read_byte(1);
    var mbc1 = MBC.MBC1.init(data[0..], 10, allocator);
    _ = mbc1.read_byte(1);
    mbc1.write_byte(1, 1);
    var mbc2 = MBC.MBC2.init(data[0..], 10, allocator);
    _ = mbc2.read_byte(1);
    mbc2.write_byte(1, 1);
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
