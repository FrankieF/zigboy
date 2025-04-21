const Instruction = @import("instruction.zig");
const CPU = @import("cpu.zig");
const Memory = @import("memory.zig");
const Catridge = @import("catridge.zig");
const MBC = @import("mbc.zig");
const RTC = @import("real_time_clock.zig");
const std = @import("std");

pub fn main() !void {
    test_mbc();
    test_real_time_clock();
    const c = try test_catridge();
    var cpu = test_cpu(c);
    // std.debug.print("Catridge {any}", .{c});
    // std.debug.print("Title {c}", .{c.title});
    const limit = 10; //1258895;
    var file = try std.fs.cwd().createFile("output.txt", .{});
    defer file.close();
    for (0..limit) |_| {
        //Format: [registers] (mem[pc] mem[pc+1] mem[pc+2] mem[pc+3])
        try print_line(&file, &cpu);
        const byte = cpu.next_byte();
        _ = cpu.execute(byte);
    }
    std.debug.print("Compiled ok!", .{});
}

fn print_line(file: *std.fs.File, cpu: *CPU.CPU) !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    var buffer: [256]u8 = undefined;
    const line = try std.fmt.bufPrint(&buffer, "A: {d} F: {d} B: {d} C: {d} D; {d} E: {d} H: {d} L: {d} SP: {d} PC: {d}({d} {d} {d})\n", .{
        cpu.registers.a,
        cpu.registers.a,
        cpu.registers.b,
        cpu.registers.c,
        cpu.registers.d,
        cpu.registers.e,
        cpu.registers.h,
        cpu.registers.l,
        cpu.sp,
        cpu.pc,
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 1)[0]),
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 2)[0]),
        cpu.memory.read_byte(@addWithOverflow(cpu.pc, 3)[0]),
    });
    //const format = "A: {d} F: {d} B: {d} C: {d} D; {d} E: {d} H: {d} L: {d} SP: {d} PC: {d}({d} {d} {d})";
    // const string = try std.fmt.allocPrint(allocator, format, .{
    //     cpu.registers.a,
    //     cpu.registers.a,
    //     cpu.registers.b,
    //     cpu.registers.c,
    //     cpu.registers.d,
    //     cpu.registers.e,
    //     cpu.registers.h,
    //     cpu.registers.l,
    //     cpu.sp,
    //     cpu.pc,
    //     cpu.memory.read_byte(@addWithOverflow(cpu.pc, 1)[0]),
    //     cpu.memory.read_byte(@addWithOverflow(cpu.pc, 2)[0]),
    //     cpu.memory.read_byte(@addWithOverflow(cpu.pc, 3)[0]),
    // });
    //defer allocator.free(string);
    try file.writeAll(line);
    //std.debug.print("{s}\n", .{line});
}

fn test_real_time_clock() void {
    const rtc = RTC.RealTimeClock.init(null);
    std.debug.print("Zero {any}", .{rtc.zero});
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
    const path = "C:\\Users\\frank\\source\\repos\\zigboy\\gb-test-roms-master\\cpu_instrs\\cpu_instrs.gb";
    const allocator = gpa.allocator();
    return Catridge.Catridge.init(path, allocator);
}

fn test_cpu(catridge: Catridge.Catridge) CPU.CPU {
    const memory: Memory.Memory = Memory.Memory.init(catridge);
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
    return CPU.CPU.init(registers, flags, memory);
}
