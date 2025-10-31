const std = @import("std");
const expect = std.testing.expect;
const cpu_file = @import("cpu.zig");
const Registers = cpu_file.Registers;
const Flags = cpu_file.Flags;
const CPU = cpu_file.CPU;
const Memory = @import("memory.zig").Memory;
const bit = @import("bit.zig");

pub const LogLevel = enum {
    All,
    SuccessOnly,
};

const dir_path = "test_roms\\sm83";

pub fn run_all_tests(logLevel: LogLevel) !void {
    var cpu = setup();
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();
    var it = dir.iterate();
    std.debug.print("\nStarting all tests", .{});
    while (try it.next()) |next| {
        std.debug.print("\nname: {s}", .{next.name}); // need to combine this with the full path
        try run_one_test(&cpu, next.name, logLevel);
    }
    std.debug.print("\nFinished all tests", .{});
}

pub fn run_test(test_path: []const u8, logLevel: LogLevel) !void {
    var cpu = setup();
    const data = try open_test(test_path);
    for (data) |d| {
        setup_cpu(&cpu, d.initial);
        _ = cpu.tick();
        try assert_cpu(&cpu, d.final, d.name, logLevel);
    }
}

pub fn run_one_test(cpu: *CPU, test_path: []const u8, logLevel: LogLevel) !void {
    const data = try open_test(test_path);
    const name = data[0].name[0..2];
    std.debug.print("\nStarting test for {s}", .{name});
    for (data) |d| {
        setup_cpu(cpu, d.initial);
        if (logLevel == LogLevel.All) {
            std.debug.print("\nrunning {s}", .{d.name});
        }
        _ = cpu.tick();
        try assert_cpu(cpu, d.final, d.name, logLevel);
    }
    std.debug.print("\nFinished test for {s}", .{name});
}

fn setup() CPU {
    const memory: Memory = Memory.init_for_cpu_test();
    const registers = Registers.init();
    const flags = Flags.init();
    return CPU.init(registers, flags, memory);
}

fn open_test(path: []const u8) ![]Mb32Data {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();
    var file = try dir.openFile(path, .{});
    defer file.close();
    const allocator = gpa.allocator();
    const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

    var scanner = std.json.Scanner.initCompleteInput(allocator, contents);
    defer scanner.deinit();

    var diagnostics = std.json.Diagnostics{};
    scanner.enableDiagnostics(&diagnostics);
    errdefer std.log.err("byte offset {d}\nbytes: {s}|{s}\n", .{
        diagnostics.getByteOffset(),
        contents[diagnostics.getByteOffset() -| 32..][0..32],
        contents[diagnostics.getByteOffset()..][0..32],
    });

    const json = try std.json.parseFromTokenSource([]Mb32Data, allocator, &scanner, .{ .allocate = .alloc_always, .ignore_unknown_fields = true });
    return json.value;
}

pub fn setup_cpu(cpu: *CPU, data: Mb32Initial) void {
    cpu.registers.a = data.a;
    cpu.registers.b = data.b;
    cpu.registers.c = data.c;
    cpu.registers.d = data.d;
    cpu.registers.e = data.e;
    cpu.registers.h = data.h;
    cpu.registers.l = data.l;
    cpu.flags.zero = bit.is_set(data.f, 7);
    cpu.flags.subtract = bit.is_set(data.f, 6);
    cpu.flags.half_carry = bit.is_set(data.f, 5);
    cpu.flags.carry = bit.is_set(data.f, 4);
    cpu.pc = data.pc;
    cpu.sp = data.sp;
    @memset(&cpu.memory.test_ram, 0);
    for (data.ram) |entry| {
        cpu.memory.write_byte(entry.@"0", entry.@"1");
    }
    cpu.interrupt_master_enabled = false;
    cpu.halted = false;
}

pub fn assert_cpu(cpu: *CPU, data: Mb32Initial, name: []const u8, logLevel: LogLevel) !void {
    if (logLevel == LogLevel.All) {
        std.debug.print("\nChecking {s}", .{name});
    }
    try assert_value(cpu.registers.a, data.a, "a");
    try assert_value(cpu.registers.b, data.b, "b");
    try assert_value(cpu.registers.c, data.c, "c");
    try assert_value(cpu.registers.d, data.d, "d");
    try assert_value(cpu.registers.e, data.e, "e");
    try assert_value(cpu.registers.h, data.h, "h");
    try assert_value(cpu.registers.l, data.l, "l");
    const zero = bit.is_set(data.f, 7);
    try assert_bool(cpu.flags.zero, zero, "zero");
    const sub = bit.is_set(data.f, 6);
    try assert_bool(cpu.flags.subtract, sub, "subtract");
    const hf = bit.is_set(data.f, 5);
    try assert_bool(cpu.flags.half_carry, hf, "half carry");
    const carry = bit.is_set(data.f, 4);
    try assert_bool(cpu.flags.carry, carry, "carry");
    try assert_value(cpu.pc, data.pc, "PC");
    try assert_value(cpu.sp, data.sp, "SP");
    for (data.ram) |entry| {
        const e0 = entry.@"0";
        const e1 = entry.@"1";
        const byte = cpu.memory.read_byte(e0);
        if (byte != e1) {
            std.debug.print("\nRam {d} is {d} but should be {d}.", .{ e0, byte, e1 });
            try expect(byte == e1);
        }
    }
    if (logLevel == LogLevel.All) {
        std.debug.print("\n{s} OK!", .{name});
    }
}

fn assert_value(a: isize, b: isize, name: []const u8) !void {
    if (a != b) {
        std.debug.print("\n{s} is {d} but should be {d}.", .{ name, a, b });
        try expect(a == b);
    }
}

fn assert_bool(a: bool, b: bool, name: []const u8) !void {
    if (a != b) {
        std.debug.print("\n{s} is {any} but should be {any}.", .{ name, a, b });
        try expect(a == b);
    }
}

pub const Mb32Data = struct {
    name: []const u8,
    initial: Mb32Initial,
    final: Mb32Initial,
    cycles: []struct { u16, u8, []const u8 },
};

pub const Mb32Initial = struct {
    pc: u16,
    sp: u16,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    ime: u2,
    ie: u2 = 0,
    ram: []struct { u16, u8 },
};
