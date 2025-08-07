const Instruction = @import("instruction.zig");
const CPU = @import("cpu.zig");
const Memory = @import("memory.zig");
const Catridge = @import("catridge.zig");
const MBC = @import("mbc.zig");
const RTC = @import("real_time_clock.zig");
const GPU = @import("gpu.zig");
const std = @import("std");
const Interrupt = @import("interrupt.zig");

pub fn main() !void {
    test_mbc();
    test_real_time_clock();
    const c = try test_catridge();
    var cpu = test_cpu(c);
    // std.debug.print("Catridge {any}", .{c});
    // std.debug.print("Title {c}", .{c.title});
    const limit = 1258895;
    var file = try std.fs.cwd().createFile("output.txt", .{});
    const gpu = GPU.GPU.init(Interrupt.Interrupt.init());
    _ = gpu;
    defer file.close();
    for (0..limit) |_| {
        //Format: [registers] (mem[pc] mem[pc+1] mem[pc+2] mem[pc+3])
        try print_line(&file, &cpu);
        const cycles = cpu.step();
        cpu.memory.update(cycles);
    }
    std.debug.print("Compiled ok!", .{});
}

fn print_line(file: *std.fs.File, cpu: *CPU.CPU) !void {
    var buffer: [256]u8 = undefined;
    var f: u8 = 0;
    f |= 0x80 & (@as(u8, @intFromBool(cpu.flags.zero)) << 7);
    // std.debug.print("F: {any}, {any}\n", .{ f, @as(u8, @intFromBool(cpu.flags.zero)) });
    f |= 0x40 & (@as(u8, @intFromBool(cpu.flags.subtract)) << 6);
    // std.debug.print("F: {any}, {any}\n", .{ f, @as(u8, @intFromBool(cpu.flags.subtract)) });
    f |= 0x20 & (@as(u8, @intFromBool(cpu.flags.half_carry)) << 5);
    // std.debug.print("F: {any}, {any}\n", .{ f, @as(u8, @intFromBool(cpu.flags.half_carry)) });
    f |= 0x10 & (@as(u8, @intFromBool(cpu.flags.carry)) << 4);
    // std.debug.print("F: {any}, {any}\n", .{ f, @as(u8, @intFromBool(cpu.flags.carry)) });
    //std.debug.print("A is: {X:0>2}\n", .{cpu.registers.a});
    const line = try std.fmt.bufPrint(&buffer, "A:{X:0>2} F:{X:0>2} B:{X:0>2} C:{X:0>2} D:{X:0>2} E:{X:0>2} H:{X:0>2} L:{X:0>2} SP:{X:0>4} PC:{X:0>4} PCMEM:{X:0>2},{X:0>2},{X:0>2},{X:0>2}\n", .{
        cpu.registers.a,
        f,
        cpu.registers.b,
        cpu.registers.c,
        cpu.registers.d,
        cpu.registers.e,
        cpu.registers.h,
        cpu.registers.l,
        cpu.sp,
        cpu.pc,
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 0)[0]),
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 1)[0]),
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 2)[0]),
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 3)[0]),
    });
    //std.debug.print("Wrote line!\nCurrent line: {any}\n\n", .{line});
    try file.writeAll(line);
}

fn test_real_time_clock() void {
    //const rtc = RTC.RealTimeClock.init(null);
    //std.debug.print("Zero {any}", .{rtc.zero});
    // _ = rtc.read_byte(1);
    //rtc.write_byte(1, 1);
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
    var mbc3 = MBC.MBC3.init(data[0..], 10, null, allocator);
    _ = mbc3.read_byte(1);
    mbc3.write_byte(1, 1);
    var mbc5 = MBC.MBC5.init(data[0..], 10, allocator);
    _ = mbc5.read_byte(1);
    mbc5.write_byte(1, 1);
    var mbc = MBC.MBC{ .mbc2 = mbc2 };
    _ = mbc.read_byte(1);
    mbc.write_byte(1, 1);
    _ = mbc.size();
    mbc.save();
}

fn test_catridge() Catridge.CatridgeError!Catridge.Catridge {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const path = "C:\\Users\\frank\\source\\repos\\zigboy\\gb-test-roms-master\\cpu_instrs\\individual\\03-op sp,hl.gb";
    const allocator = gpa.allocator();
    return Catridge.Catridge.init(path, allocator);
}

fn test_cpu(catridge: Catridge.Catridge) CPU.CPU {
    const memory: Memory.Memory = Memory.Memory.init(catridge);
    const registers = CPU.Registers.init();
    const flags = CPU.Flags.init();
    return CPU.CPU.init(registers, flags, memory);
}
