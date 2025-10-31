const std = @import("std");
const Memory = @import("memory.zig").Memory;
const bits = @import("bit.zig");
const main = @import("main.zig");

pub const Registers = struct {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    pub fn init() Registers {
        return Registers{
            .a = 0x01,
            .b = 0x00,
            .c = 0x13,
            .d = 0x00,
            .e = 0xD8,
            .h = 0x01,
            .l = 0x4D,
        };
    }

    pub fn get_bc(self: *Registers) u16 {
        const _u16: u16 = @as(u16, self.b) << 8 | self.c;
        return _u16;
    }

    pub fn set_bc(self: *Registers, value: u16) void {
        self.b = @truncate(value >> 8);
        self.c = @truncate(value & 0xFF);
    }

    pub fn get_hl(self: *Registers) u16 {
        return (@as(u16, self.h) << 8) | self.l;
    }

    pub fn set_hl(self: *Registers, value: u16) void {
        self.h = @truncate(value >> 8);
        self.l = @truncate(value & 0xFF);
    }

    pub fn get_de(self: *Registers) u16 {
        return (@as(u16, self.d) << 8) | self.e;
    }

    pub fn set_de(self: *Registers, value: u16) void {
        self.d = @truncate((value & 0xFF00) >> 8);
        self.e = @truncate(value & 0xFF);
    }
};

pub const Flags = struct {
    zero: bool, // Z flag
    subtract: bool, // N flag
    half_carry: bool, // H flag
    carry: bool, // C flag

    pub const ZERO = 7;
    pub const SUBTRACT = 6;
    pub const HALF_CARRY = 5;
    pub const CARRY = 4;

    pub fn init() Flags {
        return Flags{
            .carry = true,
            .half_carry = true,
            .subtract = false,
            .zero = true,
        };
    }

    pub fn set(self: *Flags, zero: bool, subtract: bool, half_carry: bool, carry: bool) void {
        self.zero = zero;
        self.subtract = subtract;
        self.half_carry = half_carry;
        self.carry = carry;
    }
};

const STEP_TIME: u32 = 16;
const STEP_CYCLES: u32 = @intFromFloat(@as(f64, @floatFromInt(STEP_TIME)) / (1000.0 / 4194304.0));

pub const CPU = struct {
    registers: Registers,
    flags: Flags,
    sp: u16,
    pc: u16,
    step_cycles: u32,
    halted: bool,
    interrupt_master_enabled: bool,
    disable_interrupt: u2,
    enable_interrupt: u2,
    memory: Memory,
    // step_zero: i64,

    pub fn init(registers: Registers, flags: Flags, memory: Memory) CPU {
        // const now = try std.time.Instant.now();
        return CPU{
            .registers = registers,
            .flags = flags,
            .sp = 0xFFFE,
            .pc = 0x100,
            .step_cycles = 0,
            .halted = false,
            .interrupt_master_enabled = true,
            .disable_interrupt = 0,
            .enable_interrupt = 0,
            .memory = memory,
            // .step_zero = now.timestamp.nsec,
        };
    }

    pub fn pop_stack(self: *CPU) u16 {
        const value = self.memory.read_word(self.sp);
        self.sp = self.sp +% 2;
        return value;
    }

    pub fn push_stack(self: *CPU, value: u16) void {
        self.sp = self.sp -% 2;
        self.memory.write_word(self.sp, value);
    }

    pub fn next_byte(self: *CPU) u8 {
        const byte = self.memory.read_byte(self.pc);
        self.pc = self.pc +% 1;
        return byte;
    }

    pub fn next_word(self: *CPU) u16 {
        const word = self.memory.read_word(self.pc);
        self.pc += 2;
        return word;
    }

    pub fn nop() void {}

    pub fn ld_bc_n16(self: *CPU, value: u16) void {
        self.*.registers.b = value & 0xFF00;
        self.*.registers.c = value & 0xFF;
    }

    fn add8(self: *CPU, value: u8, carry: bool) void {
        const a = self.registers.a;
        const c: u8 = if (self.flags.carry and carry) 1 else 0;
        const ac = a +% c;
        const result = ac +% value;
        self.registers.a = result;
        const zero_flag = result == 0;
        const half_carry_flag = (a & 0xF) + (value & 0xF) + c > 0xF; // carry at 4 bit
        const carry_flag = @as(u16, a) + @as(u16, value) + @as(u16, c) > 0xFF; // carry at 7 bit
        self.flags.set(zero_flag, false, half_carry_flag, carry_flag);
    }

    fn sub8(self: *CPU, value: u8, carry: bool) void {
        const a = self.registers.a;
        const c: u8 = if (self.flags.carry and carry) 1 else 0;
        const ac = a -% c;
        const result = ac -% value;
        self.registers.a = result;
        const zero_flag = result == 0;
        const half_carry_flag = (a & 0xF) < (value & 0xF) + c;
        const carry_flag = @as(u16, a) < @as(u16, value) + @as(u16, c);
        self.flags.set(zero_flag, true, half_carry_flag, carry_flag);
    }

    fn andA(self: *CPU, value: u8) void {
        const result = self.registers.a & value;
        self.registers.a = result;
        const zero_flag = result == 0;
        self.flags.set(zero_flag, false, true, false);
    }

    fn orA(self: *CPU, value: u8) void {
        const result = self.registers.a | value;
        self.registers.a = result;
        const zero_flag = result == 0;
        self.flags.set(zero_flag, false, false, false);
    }

    fn xor(self: *CPU, value: u8) void {
        const result = self.registers.a ^ value;
        self.registers.a = result;
        const zero_flag = result == 0;
        self.flags.set(zero_flag, false, false, false);
    }

    fn cp(self: *CPU, value: u8) void {
        const a = self.registers.a;
        sub8(self, value, false);
        self.registers.a = a;
    }

    fn inc(self: *CPU, value: u8) u8 {
        const result = value +% 1;
        const zero_flag = result == 0;
        const half_carry_flag = (value & 0xF) + 1 > 0xF;
        self.flags.set(zero_flag, false, half_carry_flag, self.flags.carry);
        return result;
    }

    fn dec(self: *CPU, value: u8) u8 {
        const result = value -% 1;
        const zero_flag = result == 0;
        const half_carry_flag = (value & 0xF) == 0;
        self.flags.set(zero_flag, true, half_carry_flag, self.flags.carry);
        return result;
    }

    fn add16(self: *CPU, value: u16) void {
        const hl = self.registers.get_hl();
        const result = hl +% value;
        const half_carry_flag = (hl & 0xFFF) + (value & 0xFFF) > 0xFFF; // carry at 11 bit
        const carry_flag = hl > 0xFFFF - value; // carry at 15 bit
        self.flags.set(self.flags.zero, false, half_carry_flag, carry_flag);
        self.registers.set_hl(result);
    }

    fn addSP(self: *CPU, value: u8) void {
        const sp = self.sp;
        const value_i8: i8 = @bitCast(value);
        const value_i16 = @as(i16, value_i8);
        const value_u16: u16 = @bitCast(value_i16);
        const half_carry_flag = (sp & 0xF) + (value_u16 & 0xF) > 0xF;
        const carry_flag = (sp & 0xFF) + (value_u16 & 0xFF) > 0xFF;
        self.flags.set(false, false, half_carry_flag, carry_flag);
        self.sp = sp +% value_u16;
    }

    fn swap(self: *CPU, value: u8) u8 {
        self.flags.set(value == 0, false, false, false);
        return (value >> 4) | (value << 4);
    }

    fn daa(self: *CPU) struct { u8, bool } {
        var offset: u8 = 0;
        var should_carry = false;
        const a = self.registers.a;
        const flags = &self.flags;
        if ((!flags.subtract and (a & 0xF > 0x09)) or flags.half_carry) {
            offset |= 0x06;
        }
        if ((!flags.subtract and a > 0x99) or self.flags.carry) {
            offset |= 0x60;
            should_carry = true;
        }
        flags.set(a == 0, flags.subtract, false, should_carry);
        return .{ if (flags.subtract) a -% offset else a +% offset, should_carry };
    }

    pub fn rlca(self: *CPU, value: u8) u8 {
        const carry = (value & 0x80) >> 7 == 1;
        const rotated: u8 = (value << 1) + @intFromBool(carry);
        self.flags.set(rotated == 0, false, false, carry);
        return rotated;
    }

    pub fn rla(self: *CPU, value: u8) u8 {
        const carry = (value & 0x80) >> 7 == 1;
        const result = (value << 1) + @intFromBool(self.flags.carry);
        self.flags.set(result == 0, false, false, carry);
        return result;
    }

    pub fn rrca(self: *CPU, value: u8) u8 {
        const carry = value & 1 == 1;
        const rotate: u8 = if (carry) 0x80 else 0;
        const result = (value >> 1) | rotate;
        self.flags.set(result == 0, false, false, carry);
        return result;
    }

    pub fn rra(self: *CPU, value: u8) u8 {
        const carry: u8 = value & 1;
        const carry_value: u8 = if (self.flags.carry) 0x80 else 0;
        const rotated = (value >> 1) | carry_value;
        self.flags.set(rotated == 0, false, false, carry != 0);
        return rotated;
    }

    pub fn sla(self: *CPU, value: u8) u8 {
        const carry = value & 0x80 == 0x80;
        const shift = value << 1;
        self.flags.set(shift == 0, false, false, carry);
        return shift;
    }

    pub fn sra(self: *CPU, value: u8) u8 {
        const carry = value & 1 == 1;
        const shift = (value >> 1) | (value & 0x80);
        self.flags.set(shift == 0, false, false, carry);
        return shift;
    }

    pub fn srl(self: *CPU, value: u8) u8 {
        const carry = value & 1 == 1;
        const shift = value >> 1;
        self.flags.set(shift == 0, false, false, carry);
        return shift;
    }

    pub fn bit(self: *CPU, b: u3, value: u8) void {
        const shift: u8 = @as(u8, 1) << b;
        const result = value & shift == 0;
        self.flags.set(result, false, true, self.flags.carry);
    }

    pub fn jr(self: *CPU, value: u8) void {
        const ipc32: i32 = @intCast(self.pc);
        const ivalue: i8 = @bitCast(value);
        const sum: u32 = @bitCast(ipc32 + ivalue);
        self.pc = @truncate(sum);
    }

    pub fn step(self: *CPU) u32 {
        if (self.step_cycles > STEP_CYCLES) {
            self.step_cycles -= STEP_CYCLES;
            // const now = try std.time.Instant.now();
            // const now_nano = now.timestamp.nsec;
            // const duration: i64 = now_nano - self.step_zero;
            // const sleep_time: u64 = if (STEP_TIME < duration) {
            //     0;
            // } else {
            //     @as(u64, STEP_TIME) - duration;
            // };
            // std.Thread.sleep(sleep_time);
            // const sixteen_millis = STEP_TIME * 1000000;
            // self.step_zero = self.step_zero.timestamp.nsec + sixteen_millis;
            // if (now_nano < self.step_zero) {
            //     self.step_zero = now_nano;
            // }
        }
        const cycles = self.tick();
        self.step_cycles += cycles;
        return cycles;
    }

    var tick_count: u64 = 0;

    pub fn tick(self: *CPU) u32 {
        tick_count += 1;
        if (main.logs == main.Logs.On)
            std.debug.print("\nTick count: {d}", .{tick_count});
        self.update_interrupts();
        const interrupt_cycles = self.check_interrupts();
        if (interrupt_cycles != 0) {
            return interrupt_cycles;
        }

        if (self.halted) {
            return 4;
        }
        const opcode = self.next_byte();
        if (main.logs == main.Logs.On)
            std.debug.print(" opcode is: {X}", .{opcode});
        return self.execute(opcode);
    }

    pub fn check_interrupts(self: *CPU) u32 {
        if (!self.halted and !self.interrupt_master_enabled) {
            return 0;
        }
        const interrupt_f = self.memory.read_byte(0xFFFF);
        const interrupt_e = self.memory.read_byte(0xFF0F);
        const interrupts = interrupt_f & interrupt_e;
        if (interrupts == 0) {
            return 0;
        }
        self.halted = false;
        if (!self.interrupt_master_enabled) {
            return 0;
        }
        self.interrupt_master_enabled = false;
        self.handle_interrupts(interrupts);
        return 16;
    }

    pub fn handle_interrupts(self: *CPU, interrupt: u8) void {
        const trailing_zeros = @ctz(interrupt);
        var itr = interrupt;
        const one: u16 = 1;
        itr &= @intFromBool((one << trailing_zeros) != 1);
        self.memory.write_byte(0xFF0F, itr);
        self.push_stack(self.pc);
        self.pc = 0x0040 | (@as(u16, trailing_zeros) << 3);
    }

    pub fn update_interrupts(self: *CPU) void {
        if (self.disable_interrupt == 2) {
            self.disable_interrupt = 1;
        } else if (self.disable_interrupt == 1) {
            self.interrupt_master_enabled = false;
            self.disable_interrupt = 0;
        } else {
            self.disable_interrupt = 0;
        }
        if (self.enable_interrupt == 2) {
            self.enable_interrupt = 1;
        } else if (self.enable_interrupt == 1) {
            self.interrupt_master_enabled = true;
            self.enable_interrupt = 0;
        } else {
            self.enable_interrupt = 0;
        }
    }

    pub fn execute(self: *CPU, opcode: u8) u32 {
        switch (opcode) {
            0x00 => { // NOP
                return 4;
            },
            0x01 => { // LD BC, n16
                const word = self.next_word();
                self.registers.set_bc(word);
                return 12;
            },
            0x02 => { // LD [BC], A
                self.memory.write_byte(self.registers.get_bc(), self.registers.a);
                return 8;
            },
            0x03 => { // INC BC
                const bc = self.registers.get_bc();
                self.registers.set_bc(bc +% 1);
                return 8;
            },
            0x04 => { // INC B
                self.registers.b = self.inc(self.registers.b);
                return 4;
            },
            0x05 => { // DEC B
                self.registers.b = self.dec(self.registers.b);
                return 4;
            },
            0x06 => { // LD B n8
                const byte = self.next_byte();
                self.registers.b = byte;
                return 8;
            },
            0x07 => { // RLCA
                self.registers.a = self.rlca(self.registers.a);
                self.flags.zero = false;
                return 4;
            },
            0x08 => { // LD [a16], SP
                const word = self.next_word();
                self.memory.write_word(word, self.sp);
                return 20;
            },
            0x09 => { // ADD HL, BC
                self.add16(self.registers.get_bc());
                return 8;
            },
            0x0A => { // LD A, [BC]
                self.registers.a = self.memory.read_byte(self.registers.get_bc());
                return 8;
            },
            0x0B => { // DEC BC
                const bc = self.registers.get_bc();
                self.registers.set_bc(bc -% 1);
                return 8;
            },
            0x0C => { // INC C
                self.registers.c = self.inc(self.registers.c);
                return 4;
            },
            0x0D => { // DEC C
                self.registers.c = self.dec(self.registers.c);
                return 4;
            },
            0x0E => { // LD C, n8
                self.registers.c = self.next_byte();
                return 8;
            },
            0x0F => { // RRCA
                self.registers.a = self.rrca(self.registers.a);
                self.flags.zero = false;
                return 4;
            },
            0x10 => { // STOP
                return 4;
            },
            0x11 => { // LD DE, n16
                const word = self.next_word();
                self.registers.set_de(word);
                return 12;
            },
            0x12 => { // LD [DE], A
                self.memory.write_byte(self.registers.get_de(), self.registers.a);
                return 8;
            },
            0x13 => { // INC DE
                const de = self.registers.get_de();
                self.registers.set_de(de +% 1);
                return 8;
            },
            0x14 => { // INC D
                self.registers.d = self.inc(self.registers.d);
                return 4;
            },
            0x15 => { // DEC D
                self.registers.d = self.dec(self.registers.d);
                return 4;
            },
            0x16 => { // LD D, n8
                self.registers.d = self.next_byte();
                return 8;
            },
            0x17 => { // RLA
                self.registers.a = self.rla(self.registers.a);
                self.flags.zero = false;
                return 8;
            },
            0x18 => { // JR e8
                self.jr(self.next_byte());
                return 12;
            },
            0x19 => { // ADD HL, DE
                self.add16(self.registers.get_de());
                return 8;
            },
            0x1A => { // LD A, [DE]
                self.registers.a = self.memory.read_byte(self.registers.get_de());
                return 8;
            },
            0x1B => { // DEC DE
                const de = self.registers.get_de();
                self.registers.set_de(de -% 1);
                return 8;
            },
            0x1C => { // INC E
                self.registers.e = self.inc(self.registers.e);
                return 4;
            },
            0x1D => { // DEC E
                self.registers.e = self.dec(self.registers.e);
                return 4;
            },
            0x1E => { // LD E, n8
                self.registers.e = self.next_byte();
                return 4;
            },
            0x1F => { // RRA
                self.registers.a = self.rra(self.registers.a);
                self.flags.zero = false;
                return 4;
            },
            0x20 => { // JR NZ, e8
                const byte = self.next_byte();
                if (!self.flags.zero) {
                    self.jr(byte);
                    return 12;
                }
                return 8;
            },
            0x21 => { // LD HL, n16
                const word = self.next_word();
                self.registers.set_hl(word);
                return 12;
            },
            0x22 => { // LD [HL+], A
                const hl = self.registers.get_hl();
                self.memory.write_byte(hl, self.registers.a);
                self.registers.set_hl(hl +% 1);
                return 8;
            },
            0x23 => { // INC HL
                const hl = self.registers.get_hl();
                self.registers.set_hl(hl +% 1);
                return 8;
            },
            0x24 => { // INC H
                self.registers.h = self.inc(self.registers.h);
                return 4;
            },
            0x25 => { // DEC H
                self.registers.h = self.dec(self.registers.h);
                return 4;
            },

            0x26 => { // LD H, n8
                self.registers.h = self.next_byte();
                return 8;
            },
            0x27 => { // DAA
                const tuple = self.daa();
                self.registers.a = tuple[0];
                const flags = &self.flags;
                flags.set(self.registers.a == 0, flags.subtract, false, tuple[1]);
                return 4;
            },
            0x28 => { // JR Z, e8
                const byte = self.next_byte();
                if (self.flags.zero) {
                    self.jr(byte);
                    return 12;
                }
                return 8;
            },
            0x29 => { // ADD HL, HL
                self.add16(self.registers.get_hl());
                return 8;
            },
            0x2A => { // LD A, [HL+]
                const hl = self.registers.get_hl();
                self.registers.a = self.memory.read_byte(hl);
                self.registers.set_hl(hl +% 1);
                return 8;
            },
            0x2B => { // DEC HL
                const hl = self.registers.get_hl();
                self.registers.set_hl(hl -% 1);
                return 8;
            },
            0x2C => { // INC L
                const l = self.registers.l;
                self.registers.l = self.inc(l);
                return 4;
            },
            0x2D => { // DEC L
                self.registers.l = self.dec(self.registers.l);
                return 4;
            },
            0x2E => { // LD L, n8
                self.registers.l = self.next_byte();
                return 4;
            },
            0x2F => { // CPL
                self.registers.a = ~self.registers.a;
                self.flags.subtract = true;
                self.flags.half_carry = true;
                return 4;
            },
            0x30 => { // JR NC, e8
                const byte = self.next_byte();
                if (!self.flags.carry) {
                    self.jr(byte);
                    return 12;
                }
                return 8;
            },
            0x31 => { // LD SP, n16
                self.sp = self.next_word();
                return 12;
            },
            0x32 => { // LD [HL-], A
                const hl = self.registers.get_hl();
                self.memory.write_byte(hl, self.registers.a);
                self.registers.set_hl(hl -% 1);
                return 8;
            },
            0x33 => { // INC SP
                const sp = self.sp;
                self.sp = sp +% 1;
                return 8;
            },
            0x34 => { // INC [HL]
                const hl = self.registers.get_hl();
                const byte = self.memory.read_byte(hl);
                const value = self.inc(byte);
                self.memory.write_byte(hl, value);
                return 12;
            },
            0x35 => { // DEC [HL]
                const hl = self.registers.get_hl();
                const byte = self.memory.read_byte(hl);
                const value = self.dec(byte);
                self.memory.write_byte(hl, value);
                return 12;
            },
            0x36 => { // LD [HL], n8
                const byte = self.next_byte();
                self.memory.write_byte(self.registers.get_hl(), byte);
                return 12;
            },
            0x37 => { // SCF
                self.flags.set(self.flags.zero, false, false, true);
                return 4;
            },
            0x38 => { // JR C, e8
                const byte = self.next_byte();
                if (self.flags.carry) {
                    self.jr(byte);
                    return 12;
                }
                return 8;
            },
            0x39 => { // ADD HL, SP
                self.add16(self.sp);
                return 8;
            },
            0x3A => { // LA A, [HL-]
                const hl = self.registers.get_hl();
                self.registers.a = self.memory.read_byte(hl);
                self.registers.set_hl(hl -% 1);
                return 8;
            },
            0x3B => { // DEC SP
                self.sp = self.sp -% 1;
                return 8;
            },
            0x3C => { // INC A
                self.registers.a = self.inc(self.registers.a);
                return 4;
            },
            0x3D => { // DEC A
                self.registers.a = self.dec(self.registers.a);
                return 4;
            },
            0x3E => { // LD A, n8
                self.registers.a = self.next_byte();
                return 8;
            },
            0x3F => { // CCF
                const flags = &self.flags;
                flags.set(flags.zero, false, false, !flags.carry);
                return 4;
            },
            0x40 => { // LD B, B
                return 4;
            },
            0x41 => { // LD B, C
                self.registers.b = self.registers.c;
                return 4;
            },
            0x42 => { // LD B, D
                self.registers.b = self.registers.d;
                return 4;
            },
            0x43 => { // LD B, E
                self.registers.b = self.registers.e;
                return 4;
            },
            0x44 => { // LD B, H
                self.registers.b = self.registers.h;
                return 4;
            },
            0x45 => { // LD B, L
                self.registers.b = self.registers.l;
                return 4;
            },
            0x46 => { // LD B, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.b = byte;
                return 8;
            },
            0x47 => { // LD B, A
                self.registers.b = self.registers.a;
                return 4;
            },
            0x48 => { // LD C, B
                self.registers.c = self.registers.b;
                return 4;
            },
            0x49 => { // LD C, C
                return 4;
            },
            0x4A => { // LD C, D
                self.registers.c = self.registers.d;
                return 4;
            },
            0x4B => { // LD C, E
                self.registers.c = self.registers.e;
                return 4;
            },
            0x4C => { // LD C, H
                self.registers.c = self.registers.h;
                return 4;
            },
            0x4D => { // LD C, L
                self.registers.c = self.registers.l;
                return 4;
            },
            0x4E => { // LD C, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.c = byte;
                return 8;
            },
            0x4F => { // LD C, A
                self.registers.c = self.registers.a;
                return 4;
            },
            0x50 => { // LD D, B
                self.registers.d = self.registers.b;
                return 4;
            },
            0x51 => { // LD D, C
                self.registers.d = self.registers.c;
                return 4;
            },
            0x52 => { // LD D, D
                return 4;
            },
            0x53 => { // LD D, E
                self.registers.d = self.registers.e;
                return 4;
            },
            0x54 => { // LD D, H
                self.registers.d = self.registers.h;
                return 4;
            },
            0x55 => { // LD D, L
                self.registers.d = self.registers.l;
                return 4;
            },
            0x56 => { // LD D, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.d = byte;
                return 8;
            },
            0x57 => { // LD D, A
                self.registers.d = self.registers.a;
                return 4;
            },
            0x58 => { // LD E, B
                self.registers.e = self.registers.b;
                return 4;
            },
            0x59 => { // LD E, C
                self.registers.e = self.registers.c;
                return 4;
            },
            0x5A => { // LD E, D
                self.registers.e = self.registers.d;
                return 4;
            },
            0x5B => { // LD E, E
                return 4;
            },
            0x5C => { // LD E, H
                self.registers.e = self.registers.h;
                return 4;
            },
            0x5D => { // LD E, L
                self.registers.e = self.registers.l;
                return 4;
            },
            0x5E => { // LD E, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.e = byte;
                return 8;
            },
            0x5F => { // LD E, A
                self.registers.e = self.registers.a;
                return 4;
            },
            0x60 => { // LD H, B
                self.registers.h = self.registers.b;
                return 4;
            },
            0x61 => { // LD H, C
                self.registers.h = self.registers.c;
                return 4;
            },
            0x62 => { // LD H, D
                self.registers.h = self.registers.d;
                return 4;
            },
            0x63 => { // LD H, E
                self.registers.h = self.registers.e;
                return 4;
            },
            0x64 => { // LD H, H
                return 4;
            },
            0x65 => { // LD H, L
                self.registers.h = self.registers.l;
                return 4;
            },
            0x66 => { // LD H, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.h = byte;
                return 8;
            },
            0x67 => { // LD H, A
                self.registers.h = self.registers.a;
                return 4;
            },
            0x68 => { // LD L, B
                self.registers.l = self.registers.b;
                return 4;
            },
            0x69 => { // LD L, C
                self.registers.l = self.registers.c;
                return 4;
            },
            0x6A => { // LD L, D
                self.registers.l = self.registers.d;
                return 4;
            },
            0x6B => { // LD L, E
                self.registers.l = self.registers.e;
                return 4;
            },
            0x6C => { // LD L, H
                self.registers.l = self.registers.h;
                return 4;
            },
            0x6D => { // LD L, L
                return 4;
            },
            0x6E => { // LD L, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.l = byte;
                return 8;
            },
            0x6F => { // LD L, A
                self.registers.l = self.registers.a;
                return 4;
            },
            0x70 => { // LD [HL], B
                self.memory.write_byte(self.registers.get_hl(), self.registers.b);
                return 8;
            },
            0x71 => { // LD [HL], C
                self.memory.write_byte(self.registers.get_hl(), self.registers.c);
                return 8;
            },
            0x72 => { // LD [HL], D
                self.memory.write_byte(self.registers.get_hl(), self.registers.d);
                return 8;
            },
            0x73 => { // LD [HL], E
                self.memory.write_byte(self.registers.get_hl(), self.registers.e);
                return 8;
            },
            0x74 => { // LD [HL], H
                self.memory.write_byte(self.registers.get_hl(), self.registers.h);
                return 8;
            },
            0x75 => { // LD [HL], L
                self.memory.write_byte(self.registers.get_hl(), self.registers.l);
                return 8;
            },
            0x76 => { // HALT
                self.halted = true;
                return 4;
            },
            0x77 => { // LD [HL], A
                self.memory.write_byte(self.registers.get_hl(), self.registers.a);
                return 8;
            },
            0x78 => { // LD A, B
                self.registers.a = self.registers.b;
                return 4;
            },
            0x79 => { // LD A, C
                self.registers.a = self.registers.c;
                return 4;
            },
            0x7A => { // LD A, D
                self.registers.a = self.registers.d;
                return 4;
            },
            0x7B => { // LD A, E
                self.registers.a = self.registers.e;
                return 4;
            },
            0x7C => { // LD A, H
                self.registers.a = self.registers.h;
                return 4;
            },
            0x7D => { // LD A, L
                self.registers.a = self.registers.l;
                return 4;
            },
            0x7E => { // LD A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.a = byte;
                return 8;
            },
            0x7F => { // LD A, A
                return 4;
            },
            0x80 => { // ADD A, B
                self.add8(self.registers.b, false);
                return 4;
            },
            0x81 => { // ADD A, C
                self.add8(self.registers.c, false);
                return 4;
            },
            0x82 => { // ADD A, D
                self.add8(self.registers.d, false);
                return 4;
            },
            0x83 => { // ADD A, E
                self.add8(self.registers.e, false);
                return 4;
            },
            0x84 => { // ADD A, H
                self.add8(self.registers.h, false);
                return 4;
            },
            0x85 => { // ADD A, L
                self.add8(self.registers.l, false);
                return 4;
            },
            0x86 => { // ADD A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.add8(byte, false);
                return 4;
            },
            0x87 => { // ADD A, A
                self.add8(self.registers.a, false);
                return 4;
            },
            0x88 => { // ADC A, B
                self.add8(self.registers.b, true);
                return 4;
            },
            0x89 => { // ADC A, C
                self.add8(self.registers.c, true);
                return 4;
            },
            0x8A => { // ADC A, D
                self.add8(self.registers.d, true);
                return 4;
            },
            0x8B => { // ADC A, E
                self.add8(self.registers.e, true);
                return 4;
            },
            0x8C => { // ADC A, H
                self.add8(self.registers.h, true);
                return 4;
            },
            0x8D => { // ADC A, L
                self.add8(self.registers.l, true);
                return 4;
            },
            0x8E => { // ADC A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.add8(byte, true);
                return 4;
            },
            0x8F => { // ADC A, A
                self.add8(self.registers.a, true);
                return 4;
            },
            0x90 => { // SUB A, B
                self.sub8(self.registers.b, false);
                return 4;
            },
            0x91 => { // SUB A, C
                self.sub8(self.registers.c, false);
                return 4;
            },
            0x92 => { // SUB A, D
                self.sub8(self.registers.d, false);
                return 4;
            },
            0x93 => { // SUB A, E
                self.sub8(self.registers.e, false);
                return 4;
            },
            0x94 => { // SUB A, H
                self.sub8(self.registers.h, false);
                return 4;
            },
            0x95 => { // SUB A, L
                self.sub8(self.registers.l, false);
                return 4;
            },
            0x96 => { // SUB A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.sub8(byte, false);
                return 4;
            },
            0x97 => { // SUB A, A
                self.sub8(self.registers.a, false);
                return 4;
            },
            0x98 => { // SBC A, B
                self.sub8(self.registers.b, true);
                return 4;
            },
            0x99 => { // SBC A, C
                self.sub8(self.registers.c, true);
                return 4;
            },
            0x9A => { // SBC A, D
                self.sub8(self.registers.d, true);
                return 4;
            },
            0x9B => { // SBC A, E
                self.sub8(self.registers.e, true);
                return 4;
            },
            0x9C => { // SBC A, H
                self.sub8(self.registers.h, true);
                return 4;
            },
            0x9D => { // SBC A, L
                self.sub8(self.registers.l, true);
                return 4;
            },
            0x9E => { // SBC A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.sub8(byte, true);
                return 4;
            },
            0x9F => { // SBC A, A
                self.sub8(self.registers.a, true);
                return 4;
            },
            0xA0 => { // AND A, B
                self.andA(self.registers.b);
                return 4;
            },
            0xA1 => { // AND A, C
                self.andA(self.registers.c);
                return 4;
            },
            0xA2 => { // AND A, D
                self.andA(self.registers.d);
                return 4;
            },
            0xA3 => { // AND A, E
                self.andA(self.registers.e);
                return 4;
            },
            0xA4 => { // AND A, H
                self.andA(self.registers.h);
                return 4;
            },
            0xA5 => { // AND A, L
                self.andA(self.registers.l);
                return 4;
            },
            0xA6 => { // AND A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.andA(byte);
                return 4;
            },
            0xA7 => { // AND A, A
                self.andA(self.registers.a);
                return 4;
            },
            0xA8 => { // XOR A, B
                self.xor(self.registers.b);
                return 4;
            },
            0xA9 => { // XOR A, C
                self.xor(self.registers.c);
                return 4;
            },
            0xAA => { // XOR A, D
                self.xor(self.registers.d);
                return 4;
            },
            0xAB => { // XOR A, E
                self.xor(self.registers.e);
                return 4;
            },
            0xAC => { // XOR A, H
                self.xor(self.registers.h);
                return 4;
            },
            0xAD => { // XOR A, L
                self.xor(self.registers.l);
                return 4;
            },
            0xAE => { // XOR A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.xor(byte);
                return 4;
            },
            0xAF => { // XOR A, A
                self.xor(self.registers.a);
                return 4;
            },
            0xB0 => { // OR A, B
                self.orA(self.registers.b);
                return 4;
            },
            0xB1 => { // OR A, C
                self.orA(self.registers.c);
                return 4;
            },
            0xB2 => { // OR A, D
                self.orA(self.registers.d);
                return 4;
            },
            0xB3 => { // OR A, E
                self.orA(self.registers.e);
                return 4;
            },
            0xB4 => { // OR A, H
                self.orA(self.registers.h);
                return 4;
            },
            0xB5 => { // OR A, L
                self.orA(self.registers.l);
                return 4;
            },
            0xB6 => { // OR A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.orA(byte);
                return 4;
            },
            0xB7 => { // OR A, A
                self.orA(self.registers.a);
                return 4;
            },
            0xB8 => { // CP A, B
                self.cp(self.registers.b);
                return 4;
            },
            0xB9 => { // CP A, C
                self.cp(self.registers.c);
                return 4;
            },
            0xBA => { // CP A, D
                self.cp(self.registers.d);
                return 4;
            },
            0xBB => { // CP A, E
                self.cp(self.registers.e);
                return 4;
            },
            0xBC => { // CP A, H
                self.cp(self.registers.h);
                return 4;
            },
            0xBD => { // CP A, L
                self.cp(self.registers.l);
                return 4;
            },
            0xBE => { // CP A, [HL]
                const byte = self.memory.read_byte(self.registers.get_hl());
                self.cp(byte);
                return 4;
            },
            0xBF => { // CP A, A
                self.cp(self.registers.a);
                return 4;
            },
            0xC0 => { // RET NZ
                if (self.flags.zero) {
                    return 8;
                }
                self.pc = self.pop_stack();
                return 20;
            },
            0xC1 => { // POP BC
                self.registers.set_bc(self.pop_stack());
                return 12;
            },
            0xC2 => { // JP NZ, a16
                const word = self.next_word();
                if (self.flags.zero) {
                    return 12;
                }
                self.pc = word;
                return 16;
            },
            0xC3 => { // JP a16
                const word = self.next_word();
                self.pc = word;
                return 16;
            },
            0xC4 => { // CALL NZ, a16
                const word = self.next_word();
                if (self.flags.zero) {
                    return 12;
                }
                self.push_stack(self.pc);
                self.pc = word;
                return 24;
            },
            0xC5 => { // PUSH BC
                self.push_stack(self.registers.get_bc());
                return 16;
            },
            0xC6 => { // ADD A, n8
                self.add8(self.next_byte(), false);
                return 8;
            },
            0xC7 => { // RST $00
                self.push_stack(self.pc);
                self.pc = 0;
                return 32;
            },
            0xC8 => { // RET Z
                if (!self.flags.zero) {
                    return 8;
                }
                self.pc = self.pop_stack();
                return 20;
            },
            0xC9 => { // RET
                const word = self.pop_stack();
                self.pc = word;
                return 16;
            },
            0xCA => { // JP Z, a16
                const word = self.next_word();
                if (!self.flags.zero) {
                    return 12;
                }
                self.pc = word;
                return 16;
            },
            0xCB => { // PREFIX
                return prefix(self, self.next_byte());
            },
            0xCC => { // CALL Z, a16
                const word = self.next_word();
                if (!self.flags.zero) {
                    return 12;
                }
                self.push_stack(self.pc);
                self.pc = word;
                return 24;
            },
            0xCD => { // CALL a16
                const word = self.next_word();
                self.push_stack(self.pc);
                self.pc = word;
                return 24;
            },
            0xCE => { // ADC A, n8
                self.add8(self.next_byte(), true);
                return 8;
            },
            0xCF => { // RST $08
                self.push_stack(self.pc);
                self.pc = 0x08;
                return 32;
            },
            0xD0 => { // RET NC
                if (self.flags.carry) {
                    return 8;
                }
                self.pc = self.pop_stack();
                return 20;
            },
            0xD1 => { // POP DE
                self.registers.set_de(self.pop_stack());
                return 12;
            },
            0xD2 => { // JP NC, a16
                const word = self.next_word();
                if (self.flags.carry) {
                    return 12;
                }
                self.pc = word;
                return 16;
            },
            0xD4 => { // CALL NC, a16
                const word = self.next_word();
                if (self.flags.carry) {
                    return 12;
                }
                self.push_stack(self.pc);
                self.pc = word;
                return 24;
            },
            0xD5 => { // PUSH DE
                self.push_stack(self.registers.get_de());
                return 16;
            },
            0xD6 => { // SUB A, n8
                self.sub8(self.next_byte(), false);
                return 8;
            },
            0xD7 => { // RST $10
                self.push_stack(self.pc);
                self.pc = 0x10;
                return 32;
            },
            0xD8 => { // RET C
                if (!self.flags.carry) {
                    return 8;
                }
                self.pc = self.pop_stack();
                return 20;
            },
            0xD9 => { // RETi
                self.pc = self.pop_stack();
                self.interrupt_master_enabled = true;
                return 16;
            },
            0xDA => { // JP C, a16
                const word = self.next_word();
                if (!self.flags.carry) {
                    return 12;
                }
                self.pc = word;
                return 16;
            },
            0xDC => { // CALL C, a16
                const word = self.next_word();
                if (!self.flags.carry) {
                    return 12;
                }
                self.push_stack(self.pc);
                self.pc = word;
                return 24;
            },
            0xDE => { // SBC A, n8
                self.sub8(self.next_byte(), true);
                return 8;
            },
            0xDF => { // RST $18
                self.push_stack(self.pc);
                self.pc = 0x18;
                return 32;
            },
            0xE0 => { // LDH [a8], A
                const address = 0xFF00 | @as(u16, self.next_byte());
                self.memory.write_byte(address, self.registers.a);
                return 12;
            },
            0xE1 => { // POP HL
                self.registers.set_hl(self.pop_stack());
                return 12;
            },
            0xE2 => { // LDH [C] A
                const address: u16 = 0xFF00 | @as(u16, self.registers.c);
                self.memory.write_byte(address, self.registers.a);
                return 8;
            },
            0xE5 => { // PUSH HL
                self.push_stack(self.registers.get_hl());
                return 16;
            },
            0xE6 => { // AND A, n8
                self.andA(self.next_byte());
                return 8;
            },
            0xE7 => { // RST $20
                self.push_stack(self.pc);
                self.pc = 0x20;
                return 32;
            },
            0xE8 => { // ADD SP, e8
                self.addSP(self.next_byte());
                return 16;
            },
            0xE9 => { // JP HL
                self.pc = self.registers.get_hl();
                return 4;
            },
            0xEA => { // LD [a16], A
                const word = self.next_word();
                self.memory.write_byte(word, self.registers.a);
                return 16;
            },
            0xEE => { // XOR A, n8
                self.xor(self.next_byte());
                return 8;
            },
            0xEF => { // RST $28
                self.push_stack(self.pc);
                self.pc = 0x28;
                return 32;
            },
            0xF0 => { // LDH A, [a8]
                const address = 0xFF00 | @as(u16, self.next_byte());
                self.registers.a = self.memory.read_byte(address);
                return 12;
            },
            0xF1 => { // POP AF
                const value = self.pop_stack();
                self.registers.a = @truncate(value >> 8);
                self.flags.zero = bits.is_set16(value, Flags.ZERO);
                self.flags.subtract = bits.is_set16(value, Flags.SUBTRACT);
                self.flags.half_carry = bits.is_set16(value, Flags.HALF_CARRY);
                self.flags.carry = bits.is_set16(value, Flags.CARRY);
                return 12;
            },
            0xF2 => { // LDH A, [C]
                const address: u16 = 0xFF00 | @as(u16, self.registers.c);
                self.registers.a = self.memory.read_byte(address);
                return 8;
            },
            0xF3 => { // DI
                self.disable_interrupt = 2;
                return 4;
            },
            0xF5 => { // PUSH AF
                var af = @as(u16, self.registers.a) << 8;
                if (self.flags.zero) {
                    af |= 0x80;
                }
                if (self.flags.subtract) {
                    af |= 0x40;
                }
                if (self.flags.half_carry) {
                    af |= 0x20;
                }
                if (self.flags.carry) {
                    af |= 0x10;
                }
                self.push_stack(af);
                return 16;
            },
            0xF6 => { // OR a, n8
                self.orA(self.next_byte());
                return 8;
            },
            0xF7 => { // RST $30
                self.push_stack(self.pc);
                self.pc = 0x30;
                return 32;
            },
            0xF8 => { // LD HL, SP + e8
                const byte = self.next_byte();
                const byte_i8: i8 = @bitCast(byte);
                const byte_i16 = @as(i16, byte_i8);
                const byte_u16: u16 = @bitCast(byte_i16);
                const sp = self.sp;
                const half_carry = (sp & 0xF) + (byte_u16 & 0xF) > 0xF;
                const carry = (sp & 0xFF) + (byte_u16 & 0xFF) > 0xFF;
                self.flags.set(false, false, half_carry, carry);
                const value = sp +% byte_u16;
                self.registers.set_hl(value);
                return 12;
            },
            0xF9 => { // LD SP, HL
                self.sp = self.registers.get_hl();
                return 8;
            },
            0xFA => { // LD A, [a16]
                const address = self.next_word();
                self.registers.a = self.memory.read_byte(address);
                return 16;
            },
            0xFB => { // EI
                self.enable_interrupt = 2;
                return 4;
            },
            0xFE => { // CP A, n8
                self.cp(self.next_byte());
                return 8;
            },
            0xFF => { // RST $38
                self.push_stack(self.pc);
                self.pc = 0x38;
                return 32;
            },
            else => std.debug.print("Opcode [{d}] is not implemented yet.", .{opcode}),
        }
        return 0;
    }

    pub fn prefix(self: *CPU, opcode: u8) u32 {
        switch (opcode) {
            0x00 => { // RLC B
                self.registers.b = self.rlca(self.registers.b);
                return 8;
            },
            0x01 => { // RLC C
                self.registers.c = self.rlca(self.registers.c);
                return 8;
            },
            0x02 => { // RLC D
                self.registers.d = self.rlca(self.registers.d);
                return 8;
            },
            0x03 => { // RLC E
                self.registers.e = self.rlca(self.registers.e);
                return 8;
            },
            0x04 => { // RLC H
                self.registers.h = self.rlca(self.registers.h);
                return 8;
            },
            0x05 => { // RLC L
                self.registers.l = self.rlca(self.registers.l);
                return 8;
            },
            0x06 => { // RLC HL
                const value = self.rlca(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x07 => { // RLC A
                self.registers.a = self.rlca(self.registers.a);
                return 8;
            },
            0x08 => { // RRC B
                self.registers.b = self.rrca(self.registers.b);
                return 8;
            },
            0x09 => { // RRC C
                self.registers.c = self.rrca(self.registers.c);
                return 8;
            },
            0x0A => { // RRC D
                self.registers.d = self.rrca(self.registers.d);
                return 8;
            },
            0x0B => { // RRC E
                self.registers.e = self.rrca(self.registers.e);
                return 8;
            },
            0x0C => { // RRC H
                self.registers.h = self.rrca(self.registers.h);
                return 8;
            },
            0x0D => { // RRC L
                self.registers.l = self.rrca(self.registers.l);
                return 8;
            },
            0x0E => { // RRC HL
                const value = self.rrca(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x0F => { // RRC A
                self.registers.a = self.rrca(self.registers.a);
                return 8;
            },
            0x10 => { // RL B
                self.registers.b = self.rla(self.registers.b);
                return 8;
            },
            0x11 => { // RL C
                self.registers.c = self.rla(self.registers.c);
                return 8;
            },
            0x12 => { // RL D
                self.registers.d = self.rla(self.registers.d);
                return 8;
            },
            0x13 => { // RL E
                self.registers.e = self.rla(self.registers.e);
                return 8;
            },
            0x14 => { // RL H
                self.registers.h = self.rla(self.registers.h);
                return 8;
            },
            0x15 => { // RL L
                self.registers.l = self.rla(self.registers.l);
                return 8;
            },
            0x16 => { // RL HL
                const value = self.rla(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x17 => { // RL A
                self.registers.a = self.rla(self.registers.a);
                return 8;
            },
            0x18 => { // RR B
                self.registers.b = self.rra(self.registers.b);
                return 8;
            },
            0x19 => { // RR C
                self.registers.c = self.rra(self.registers.c);
                return 8;
            },
            0x1A => { // RR D
                self.registers.d = self.rra(self.registers.d);
                return 8;
            },
            0x1B => { // RR E
                self.registers.e = self.rra(self.registers.e);
                return 8;
            },
            0x1C => { // RR H
                self.registers.h = self.rra(self.registers.h);
                return 8;
            },
            0x1D => { // RR L
                self.registers.l = self.rra(self.registers.l);
                return 8;
            },
            0x1E => { // RR HL
                const value = self.rra(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x1F => { // RR A
                self.registers.a = self.rra(self.registers.a);
                return 8;
            },
            0x20 => { // SLA B
                self.registers.b = self.sla(self.registers.b);
                return 8;
            },
            0x21 => { // SLA C
                self.registers.c = self.sla(self.registers.c);
                return 8;
            },
            0x22 => { // SLA D
                self.registers.d = self.sla(self.registers.d);
                return 8;
            },
            0x23 => { // SLA E
                self.registers.e = self.sla(self.registers.e);
                return 8;
            },
            0x24 => { // SLA H
                self.registers.h = self.sla(self.registers.h);
                return 8;
            },
            0x25 => { // SLA L
                self.registers.l = self.sla(self.registers.l);
                return 8;
            },
            0x26 => { // SLA HL
                const value = self.sla(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x27 => { // SLA A
                self.registers.a = self.sla(self.registers.a);
                return 8;
            },
            0x28 => { // SRA B
                self.registers.b = self.sra(self.registers.b);
                return 8;
            },
            0x29 => { // SRA C
                self.registers.c = self.sra(self.registers.c);
                return 8;
            },
            0x2A => { // SRA D
                self.registers.d = self.sra(self.registers.d);
                return 8;
            },
            0x2B => { // SRA E
                self.registers.e = self.sra(self.registers.e);
                return 8;
            },
            0x2C => { // SRA H
                self.registers.h = self.sra(self.registers.h);
                return 8;
            },
            0x2D => { // SRA L
                self.registers.l = self.sra(self.registers.l);
                return 8;
            },
            0x2E => { // SRA HL
                const value = self.sra(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x2F => { // SRA A
                self.registers.a = self.sra(self.registers.a);
                return 8;
            },
            0x30 => { // SWAP B
                self.registers.b = self.swap(self.registers.b);
                return 8;
            },
            0x31 => { // SWAP C
                self.registers.c = self.swap(self.registers.c);
                return 8;
            },
            0x32 => { // SWAP D
                self.registers.d = self.swap(self.registers.d);
                return 8;
            },
            0x33 => { // SWAP E
                self.registers.e = self.swap(self.registers.e);
                return 8;
            },
            0x34 => { // SWAP H
                self.registers.h = self.swap(self.registers.h);
                return 8;
            },
            0x35 => { // SWAP L
                self.registers.l = self.swap(self.registers.l);
                return 8;
            },
            0x36 => { // SWAP HL
                const value = self.swap(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x37 => { // SWAP A
                self.registers.a = self.swap(self.registers.a);
                return 8;
            },
            0x38 => { // SRA B
                self.registers.b = self.srl(self.registers.b);
                return 8;
            },
            0x39 => { // SRA C
                self.registers.c = self.srl(self.registers.c);
                return 8;
            },
            0x3A => { // SRA D
                self.registers.d = self.srl(self.registers.d);
                return 8;
            },
            0x3B => { // SRA E
                self.registers.e = self.srl(self.registers.e);
                return 8;
            },
            0x3C => { // SRA H
                self.registers.h = self.srl(self.registers.h);
                return 8;
            },
            0x3D => { // SRA L
                self.registers.l = self.srl(self.registers.l);
                return 8;
            },
            0x3E => { // SRA HL
                const value = self.srl(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x3F => { // SRA A
                self.registers.a = self.srl(self.registers.a);
                return 8;
            },
            0x40 => { // BIT 0, B
                self.bit(0, self.registers.b);
                return 8;
            },
            0x41 => { // BIT 0, C
                self.bit(0, self.registers.c);
                return 8;
            },
            0x42 => { // BIT 0, D
                self.bit(0, self.registers.d);
                return 8;
            },
            0x43 => { // BIT 0, E
                self.bit(0, self.registers.e);
                return 8;
            },
            0x44 => { // BIT 0, H
                self.bit(0, self.registers.h);
                return 8;
            },
            0x45 => { // BIT 0, L
                self.bit(0, self.registers.l);
                return 8;
            },
            0x46 => { // BIT 0, HL
                self.bit(0, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x47 => { // BIT 0, A
                self.bit(0, self.registers.a);
                return 8;
            },
            0x48 => { // BIT 1, B
                self.bit(1, self.registers.b);
                return 8;
            },
            0x49 => { // BIT 1, C
                self.bit(1, self.registers.c);
                return 8;
            },
            0x4A => { // BIT 1, D
                self.bit(1, self.registers.d);
                return 8;
            },
            0x4B => { // BIT 1, E
                self.bit(1, self.registers.e);
                return 8;
            },
            0x4C => { // BIT 1, H
                self.bit(1, self.registers.h);
                return 8;
            },
            0x4D => { // BIT 1, L
                self.bit(1, self.registers.l);
                return 8;
            },
            0x4E => { // BIT 1, HL
                self.bit(1, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x4F => { // BIT 1, A
                self.bit(1, self.registers.a);
                return 8;
            },
            0x50 => { // BIT 2, B
                self.bit(2, self.registers.b);
                return 8;
            },
            0x51 => { // BIT 2, C
                self.bit(2, self.registers.c);
                return 8;
            },
            0x52 => { // BIT 2, D
                self.bit(2, self.registers.d);
                return 8;
            },
            0x53 => { // BIT 2, E
                self.bit(2, self.registers.e);
                return 8;
            },
            0x54 => { // BIT 2, H
                self.bit(2, self.registers.h);
                return 8;
            },
            0x55 => { // BIT 2, L
                self.bit(2, self.registers.l);
                return 8;
            },
            0x56 => { // BIT 2, HL
                self.bit(2, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x57 => { // BIT 2, A
                self.bit(2, self.registers.a);
                return 8;
            },
            0x58 => { // BIT 3, B
                self.bit(3, self.registers.b);
                return 8;
            },
            0x59 => { // BIT 3, C
                self.bit(3, self.registers.c);
                return 8;
            },
            0x5A => { // BIT 3, D
                self.bit(3, self.registers.d);
                return 8;
            },
            0x5B => { // BIT 3, E
                self.bit(3, self.registers.e);
                return 8;
            },
            0x5C => { // BIT 3, H
                self.bit(3, self.registers.h);
                return 8;
            },
            0x5D => { // BIT 3, L
                self.bit(3, self.registers.l);
                return 8;
            },
            0x5E => { // BIT 3, HL
                self.bit(3, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x5F => { // BIT 3, A
                self.bit(3, self.registers.a);
                return 8;
            },
            0x60 => { // BIT 4, B
                self.bit(4, self.registers.b);
                return 8;
            },
            0x61 => { // BIT 4, C
                self.bit(4, self.registers.c);
                return 8;
            },
            0x62 => { // BIT 4, D
                self.bit(4, self.registers.d);
                return 8;
            },
            0x63 => { // BIT 4, E
                self.bit(4, self.registers.e);
                return 8;
            },
            0x64 => { // BIT 4, H
                self.bit(4, self.registers.h);
                return 8;
            },
            0x65 => { // BIT 4, L
                self.bit(4, self.registers.l);
                return 8;
            },
            0x66 => { // BIT 4, HL
                self.bit(4, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x67 => { // BIT 4, A
                self.bit(4, self.registers.a);
                return 8;
            },
            0x68 => { // BIT 5, B
                self.bit(5, self.registers.b);
                return 8;
            },
            0x69 => { // BIT 5, C
                self.bit(5, self.registers.c);
                return 8;
            },
            0x6A => { // BIT 5, D
                self.bit(5, self.registers.d);
                return 8;
            },
            0x6B => { // BIT 5, E
                self.bit(5, self.registers.e);
                return 8;
            },
            0x6C => { // BIT 5, H
                self.bit(5, self.registers.h);
                return 8;
            },
            0x6D => { // BIT 5, L
                self.bit(5, self.registers.l);
                return 8;
            },
            0x6E => { // BIT 5, HL
                self.bit(5, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x6F => { // BIT 5, A
                self.bit(5, self.registers.a);
                return 8;
            },
            0x70 => { // BIT 6, B
                self.bit(6, self.registers.b);
                return 8;
            },
            0x71 => { // BIT 6, C
                self.bit(6, self.registers.c);
                return 8;
            },
            0x72 => { // BIT 6, D
                self.bit(6, self.registers.d);
                return 8;
            },
            0x73 => { // BIT 6, E
                self.bit(6, self.registers.e);
                return 8;
            },
            0x74 => { // BIT 6, H
                self.bit(6, self.registers.h);
                return 8;
            },
            0x75 => { // BIT 6, L
                self.bit(6, self.registers.l);
                return 8;
            },
            0x76 => { // BIT 6, HL
                self.bit(6, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x77 => { // BIT 6, A
                self.bit(6, self.registers.a);
                return 8;
            },
            0x78 => { // BIT 7, B
                self.bit(7, self.registers.b);
                return 8;
            },
            0x79 => { // BIT 7, C
                self.bit(7, self.registers.c);
                return 8;
            },
            0x7A => { // BIT 7, D
                self.bit(7, self.registers.d);
                return 8;
            },
            0x7B => { // BIT 7, E
                self.bit(7, self.registers.e);
                return 8;
            },
            0x7C => { // BIT 7, H
                self.bit(7, self.registers.h);
                return 8;
            },
            0x7D => { // BIT 7, L
                self.bit(7, self.registers.l);
                return 8;
            },
            0x7E => { // BIT 7, HL
                self.bit(7, self.memory.read_byte(self.registers.get_hl()));
                return 16;
            },
            0x7F => { // BIT 7, A
                self.bit(7, self.registers.a);
                return 8;
            },
            0x80 => { // RES 0, B
                self.registers.b &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x81 => { // RES 0, C
                self.registers.c &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x82 => { // RES 0, D
                self.registers.d &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x83 => { // RES 0, E
                self.registers.e &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x84 => { // RES 0, H
                self.registers.h &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x85 => { // RES 0, L
                self.registers.l &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x86 => { // RES 0, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 0));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x87 => { // RES 0, A
                self.registers.a &= ~(@as(u8, 1 << 0));
                return 8;
            },
            0x88 => { // RES 1, B
                self.registers.b &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x89 => { // RES 1, C
                self.registers.c &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x8A => { // RES 1, D
                self.registers.d &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x8B => { // RES 1, E
                self.registers.e &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x8C => { // RES 1, H
                self.registers.h &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x8D => { // RES 1, L
                self.registers.l &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x8E => { // RES 1, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 1));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x8F => { // RES 1, A
                self.registers.a &= ~(@as(u8, 1 << 1));
                return 8;
            },
            0x90 => { // RES 2, B
                self.registers.b &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x91 => { // RES 2, C
                self.registers.c &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x92 => { // RES 2, D
                self.registers.d &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x93 => { // RES 2, E
                self.registers.e &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x94 => { // RES 2, H
                self.registers.h &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x95 => { // RES 2, L
                self.registers.l &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x96 => { // RES 2, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 2));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x97 => { // RES 2, A
                self.registers.a &= ~(@as(u8, 1 << 2));
                return 8;
            },
            0x98 => { // RES 3, B
                self.registers.b &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0x99 => { // RES 3, C
                self.registers.c &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0x9A => { // RES 3, D
                self.registers.d &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0x9B => { // RES 3, E
                self.registers.e &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0x9C => { // RES 3, H
                self.registers.h &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0x9D => { // RES 3, L
                self.registers.l &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0x9E => { // RES 3, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 3));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0x9F => { // RES 3, A
                self.registers.a &= ~(@as(u8, 1 << 3));
                return 8;
            },
            0xA0 => { // RES 4, B
                self.registers.b &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA1 => { // RES 4, C
                self.registers.c &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA2 => { // RES 4, D
                self.registers.d &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA3 => { // RES 4, E
                self.registers.e &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA4 => { // RES 4, H
                self.registers.h &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA5 => { // RES 4, L
                self.registers.l &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA6 => { // RES 4, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 4));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xA7 => { // RES 4, A
                self.registers.a &= ~(@as(u8, 1 << 4));
                return 8;
            },
            0xA8 => { // RES 5, B
                self.registers.b &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xA9 => { // RES 5, C
                self.registers.c &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xAA => { // RES 5, D
                self.registers.d &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xAB => { // RES 5, E
                self.registers.e &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xAC => { // RES 5, H
                self.registers.h &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xAD => { // RES 5, L
                self.registers.l &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xAE => { // RES 5, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 5));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xAF => { // RES 5, A
                self.registers.a &= ~(@as(u8, 1 << 5));
                return 8;
            },
            0xB0 => { // RES 6, B
                self.registers.b &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB1 => { // RES 6, C
                self.registers.c &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB2 => { // RES 6, D
                self.registers.d &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB3 => { // RES 6, E
                self.registers.e &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB4 => { // RES 6, H
                self.registers.h &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB5 => { // RES 6, L
                self.registers.l &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB6 => { // RES 6, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 6));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xB7 => { // RES 6, A
                self.registers.a &= ~(@as(u8, 1 << 6));
                return 8;
            },
            0xB8 => { // RES 7, B
                self.registers.b &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xB9 => { // RES 7, C
                self.registers.c &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xBA => { // RES 7, D
                self.registers.d &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xBB => { // RES 7, E
                self.registers.e &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xBC => { // RES 7, H
                self.registers.h &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xBD => { // RES 7, L
                self.registers.l &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xBE => { // RES 7, HL
                const value = self.memory.read_byte(self.registers.get_hl()) & ~(@as(u8, 1 << 7));
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xBF => { // RES 7, A
                self.registers.a &= ~(@as(u8, 1 << 7));
                return 8;
            },
            0xC0 => { // SET 0, B
                self.registers.b |= @as(u8, 1 << 0);
                return 8;
            },
            0xC1 => { // SET 0, C
                self.registers.c |= @as(u8, 1 << 0);
                return 8;
            },
            0xC2 => { // SET 0, D
                self.registers.d |= @as(u8, 1 << 0);
                return 8;
            },
            0xC3 => { // SET 0, E
                self.registers.e |= @as(u8, 1 << 0);
                return 8;
            },
            0xC4 => { // SET 0, H
                self.registers.h |= @as(u8, 1 << 0);
                return 8;
            },
            0xC5 => { // SET 0, L
                self.registers.l |= @as(u8, 1 << 0);
                return 8;
            },
            0xC6 => { // SET 0, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 0);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xC7 => { // SET 0, A
                self.registers.a |= @as(u8, 1 << 0);
                return 8;
            },
            0xC8 => { // SET 1, B
                self.registers.b |= @as(u8, 1 << 1);
                return 8;
            },
            0xC9 => { // SET 1, C
                self.registers.c |= @as(u8, 1 << 1);
                return 8;
            },
            0xCA => { // SET 1, D
                self.registers.d |= @as(u8, 1 << 1);
                return 8;
            },
            0xCB => { // SET 1, E
                self.registers.e |= @as(u8, 1 << 1);
                return 8;
            },
            0xCC => { // SET 1, H
                self.registers.h |= @as(u8, 1 << 1);
                return 8;
            },
            0xCD => { // SET 1, L
                self.registers.l |= @as(u8, 1 << 1);
                return 8;
            },
            0xCE => { // SET 1, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 1);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xCF => { // SET 1, A
                self.registers.a |= @as(u8, 1 << 1);
                return 8;
            },
            0xD0 => { // SET 2, B
                self.registers.b |= @as(u8, 1 << 2);
                return 8;
            },
            0xD1 => { // SET 2, C
                self.registers.c |= @as(u8, 1 << 2);
                return 8;
            },
            0xD2 => { // SET 2, D
                self.registers.d |= @as(u8, 1 << 2);
                return 8;
            },
            0xD3 => { // SET 2, E
                self.registers.e |= @as(u8, 1 << 2);
                return 8;
            },
            0xD4 => { // SET 2, H
                self.registers.h |= @as(u8, 1 << 2);
                return 8;
            },
            0xD5 => { // SET 2, L
                self.registers.l |= @as(u8, 1 << 2);
                return 8;
            },
            0xD6 => { // SET 2, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 2);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xD7 => { // SET 2, A
                self.registers.a |= @as(u8, 1 << 2);
                return 8;
            },
            0xD8 => { // SET 3, B
                self.registers.b |= @as(u8, 1 << 3);
                return 8;
            },
            0xD9 => { // SET 3, C
                self.registers.c |= @as(u8, 1 << 3);
                return 8;
            },
            0xDA => { // SET 3, D
                self.registers.d |= @as(u8, 1 << 3);
                return 8;
            },
            0xDB => { // SET 3, E
                self.registers.e |= @as(u8, 1 << 3);
                return 8;
            },
            0xDC => { // SET 3, H
                self.registers.h |= @as(u8, 1 << 3);
                return 8;
            },
            0xDD => { // SET 3, L
                self.registers.l |= @as(u8, 1 << 3);
                return 8;
            },
            0xDE => { // SET 3, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 3);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xDF => { // SET 3, A
                self.registers.a |= @as(u8, 1 << 3);
                return 8;
            },
            0xE0 => { // SET 4, B
                self.registers.b |= @as(u8, 1 << 4);
                return 8;
            },
            0xE1 => { // SET 4, C
                self.registers.c |= @as(u8, 1 << 4);
                return 8;
            },
            0xE2 => { // SET 4, D
                self.registers.d |= @as(u8, 1 << 4);
                return 8;
            },
            0xE3 => { // SET 4, E
                self.registers.e |= @as(u8, 1 << 4);
                return 8;
            },
            0xE4 => { // SET 4, H
                self.registers.h |= @as(u8, 1 << 4);
                return 8;
            },
            0xE5 => { // SET 4, L
                self.registers.l |= @as(u8, 1 << 4);
                return 8;
            },
            0xE6 => { // SET 4, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 4);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xE7 => { // SET 4, A
                self.registers.a |= @as(u8, 1 << 4);
                return 8;
            },
            0xE8 => { // SET 5, B
                self.registers.b |= @as(u8, 1 << 5);
                return 8;
            },
            0xE9 => { // SET 5, C
                self.registers.c |= @as(u8, 1 << 5);
                return 8;
            },
            0xEA => { // SET 5, D
                self.registers.d |= @as(u8, 1 << 5);
                return 8;
            },
            0xEB => { // SET 5, E
                self.registers.e |= @as(u8, 1 << 5);
                return 8;
            },
            0xEC => { // SET 5, H
                self.registers.h |= @as(u8, 1 << 5);
                return 8;
            },
            0xED => { // SET 5, L
                self.registers.l |= @as(u8, 1 << 5);
                return 8;
            },
            0xEE => { // SET 5, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 5);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xEF => { // SET 5, A
                self.registers.a |= @as(u8, 1 << 5);
                return 8;
            },
            0xF0 => { // SET 6, B
                self.registers.b |= @as(u8, 1 << 6);
                return 8;
            },
            0xF1 => { // SET 6, C
                self.registers.c |= @as(u8, 1 << 6);
                return 8;
            },
            0xF2 => { // SET 6, D
                self.registers.d |= @as(u8, 1 << 6);
                return 8;
            },
            0xF3 => { // SET 6, E
                self.registers.e |= @as(u8, 1 << 6);
                return 8;
            },
            0xF4 => { // SET 6, H
                self.registers.h |= @as(u8, 1 << 6);
                return 8;
            },
            0xF5 => { // SET 6, L
                self.registers.l |= @as(u8, 1 << 6);
                return 8;
            },
            0xF6 => { // SET 6, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 6);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xF7 => { // SET 6, A
                self.registers.a |= @as(u8, 1 << 6);
                return 8;
            },
            0xF8 => { // SET 7, B
                self.registers.b |= @as(u8, 1 << 7);
                return 8;
            },
            0xF9 => { // SET 7, C
                self.registers.c |= @as(u8, 1 << 7);
                return 8;
            },
            0xFA => { // SET 7, D
                self.registers.d |= @as(u8, 1 << 7);
                return 8;
            },
            0xFB => { // SET 7, E
                self.registers.e |= @as(u8, 1 << 7);
                return 8;
            },
            0xFC => { // SET 7, H
                self.registers.h |= @as(u8, 1 << 7);
                return 8;
            },
            0xFD => { // SET 7, L
                self.registers.l |= @as(u8, 1 << 7);
                return 8;
            },
            0xFE => { // SET 7, HL
                const value = self.memory.read_byte(self.registers.get_hl()) | @as(u8, 1 << 7);
                self.memory.write_byte(self.registers.get_hl(), value);
                return 16;
            },
            0xFF => { // SET 7, A
                self.registers.a |= @as(u8, 1 << 7);
                return 8;
            },
        }
        return 0;
    }
};

const cpu_test = @import("test.zig");
const log_level = cpu_test.LogLevel.SuccessOnly;

test "0x00" {
    try cpu_test.run_test("00.json", log_level);
}

test "0x01" {
    try cpu_test.run_test("01.json", log_level);
}

test "0x02" {
    try cpu_test.run_test("02.json", log_level);
}

test "0x03" {
    try cpu_test.run_test("03.json", log_level);
}

test "0x04" {
    try cpu_test.run_test("04.json", log_level);
}

test "0x05" {
    try cpu_test.run_test("05.json", log_level);
}

test "0x06" {
    try cpu_test.run_test("06.json", log_level);
}

test "0x07" {
    try cpu_test.run_test("07.json", log_level);
}

test "0x08" {
    try cpu_test.run_test("08.json", log_level);
}

test "0x09" {
    try cpu_test.run_test("09.json", log_level);
}

test "0x0A" {
    try cpu_test.run_test("0A.json", log_level);
}

test "0x0B" {
    try cpu_test.run_test("0B.json", log_level);
}

test "0x0C" {
    try cpu_test.run_test("0C.json", log_level);
}

test "0x0D" {
    try cpu_test.run_test("0D.json", log_level);
}

test "0x0E" {
    try cpu_test.run_test("0E.json", log_level);
}

test "0x0F" {
    try cpu_test.run_test("0F.json", log_level);
}

test "0x10" {
    try cpu_test.run_test("10.json", log_level);
}

test "0x11" {
    try cpu_test.run_test("11.json", log_level);
}

test "0x12" {
    try cpu_test.run_test("12.json", log_level);
}

test "0x13" {
    try cpu_test.run_test("13.json", log_level);
}

test "0x14" {
    try cpu_test.run_test("14.json", log_level);
}

test "0x15" {
    try cpu_test.run_test("15.json", log_level);
}

test "0x16" {
    try cpu_test.run_test("16.json", log_level);
}

test "0x17" {
    try cpu_test.run_test("17.json", log_level);
}

test "0x18" {
    try cpu_test.run_test("18.json", log_level);
}

test "0x19" {
    try cpu_test.run_test("19.json", log_level);
}

test "0x1A" {
    try cpu_test.run_test("1A.json", log_level);
}

test "0x1B" {
    try cpu_test.run_test("1B.json", log_level);
}

test "0x1C" {
    try cpu_test.run_test("1C.json", log_level);
}

test "0x1D" {
    try cpu_test.run_test("1D.json", log_level);
}

test "0x1E" {
    try cpu_test.run_test("1E.json", log_level);
}

test "0x1F" {
    try cpu_test.run_test("1F.json", log_level);
}

test "0x20" {
    try cpu_test.run_test("20.json", log_level);
}

test "0x21" {
    try cpu_test.run_test("21.json", log_level);
}

test "0x22" {
    try cpu_test.run_test("22.json", log_level);
}

test "0x23" {
    try cpu_test.run_test("23.json", log_level);
}

test "0x24" {
    try cpu_test.run_test("24.json", log_level);
}

test "0x25" {
    try cpu_test.run_test("25.json", log_level);
}

test "0x26" {
    try cpu_test.run_test("26.json", log_level);
}

test "0x27" {
    try cpu_test.run_test("27.json", log_level);
}

test "0x28" {
    try cpu_test.run_test("28.json", log_level);
}

test "0x29" {
    try cpu_test.run_test("29.json", log_level);
}

test "0x2A" {
    try cpu_test.run_test("2A.json", log_level);
}

test "0x2B" {
    try cpu_test.run_test("2B.json", log_level);
}

test "0x2C" {
    try cpu_test.run_test("2C.json", log_level);
}

test "0x2D" {
    try cpu_test.run_test("2D.json", log_level);
}

test "0x2E" {
    try cpu_test.run_test("2E.json", log_level);
}

test "0x2F" {
    try cpu_test.run_test("2F.json", log_level);
}

test "0x30" {
    try cpu_test.run_test("30.json", log_level);
}

test "0x31" {
    try cpu_test.run_test("31.json", log_level);
}

test "0x32" {
    try cpu_test.run_test("32.json", log_level);
}

test "0x33" {
    try cpu_test.run_test("33.json", log_level);
}

test "0x34" {
    try cpu_test.run_test("34.json", log_level);
}

test "0x35" {
    try cpu_test.run_test("35.json", log_level);
}

test "0x36" {
    try cpu_test.run_test("36.json", log_level);
}

test "0x37" {
    try cpu_test.run_test("37.json", log_level);
}

test "0x38" {
    try cpu_test.run_test("38.json", log_level);
}

test "0x39" {
    try cpu_test.run_test("39.json", log_level);
}

test "0x3A" {
    try cpu_test.run_test("3A.json", log_level);
}

test "0x3B" {
    try cpu_test.run_test("3B.json", log_level);
}

test "0x3C" {
    try cpu_test.run_test("3C.json", log_level);
}

test "0x3D" {
    try cpu_test.run_test("3D.json", log_level);
}

test "0x3E" {
    try cpu_test.run_test("3E.json", log_level);
}

test "0x3F" {
    try cpu_test.run_test("3F.json", log_level);
}

test "0x40" {
    try cpu_test.run_test("40.json", log_level);
}

test "0x41" {
    try cpu_test.run_test("41.json", log_level);
}

test "0x42" {
    try cpu_test.run_test("42.json", log_level);
}

test "0x43" {
    try cpu_test.run_test("43.json", log_level);
}

test "0x44" {
    try cpu_test.run_test("44.json", log_level);
}

test "0x45" {
    try cpu_test.run_test("45.json", log_level);
}

test "0x46" {
    try cpu_test.run_test("46.json", log_level);
}

test "0x47" {
    try cpu_test.run_test("47.json", log_level);
}

test "0x48" {
    try cpu_test.run_test("48.json", log_level);
}

test "0x49" {
    try cpu_test.run_test("49.json", log_level);
}

test "0x4A" {
    try cpu_test.run_test("4A.json", log_level);
}

test "0x4B" {
    try cpu_test.run_test("4B.json", log_level);
}

test "0x4C" {
    try cpu_test.run_test("4C.json", log_level);
}

test "0x4D" {
    try cpu_test.run_test("4D.json", log_level);
}

test "0x4E" {
    try cpu_test.run_test("4E.json", log_level);
}

test "0x4F" {
    try cpu_test.run_test("4F.json", log_level);
}

test "0x50" {
    try cpu_test.run_test("50.json", log_level);
}

test "0x51" {
    try cpu_test.run_test("51.json", log_level);
}

test "0x52" {
    try cpu_test.run_test("52.json", log_level);
}

test "0x53" {
    try cpu_test.run_test("53.json", log_level);
}

test "0x54" {
    try cpu_test.run_test("54.json", log_level);
}

test "0x55" {
    try cpu_test.run_test("55.json", log_level);
}

test "0x56" {
    try cpu_test.run_test("56.json", log_level);
}

test "0x57" {
    try cpu_test.run_test("57.json", log_level);
}

test "0x58" {
    try cpu_test.run_test("58.json", log_level);
}

test "0x59" {
    try cpu_test.run_test("59.json", log_level);
}

test "0x5A" {
    try cpu_test.run_test("5A.json", log_level);
}

test "0x5B" {
    try cpu_test.run_test("5B.json", log_level);
}

test "0x5C" {
    try cpu_test.run_test("5C.json", log_level);
}

test "0x5D" {
    try cpu_test.run_test("5D.json", log_level);
}

test "0x5E" {
    try cpu_test.run_test("5E.json", log_level);
}

test "0x5F" {
    try cpu_test.run_test("5F.json", log_level);
}

test "0x60" {
    try cpu_test.run_test("60.json", log_level);
}

test "0x61" {
    try cpu_test.run_test("61.json", log_level);
}

test "0x62" {
    try cpu_test.run_test("62.json", log_level);
}

test "0x63" {
    try cpu_test.run_test("63.json", log_level);
}

test "0x64" {
    try cpu_test.run_test("64.json", log_level);
}

test "0x65" {
    try cpu_test.run_test("65.json", log_level);
}

test "0x66" {
    try cpu_test.run_test("66.json", log_level);
}

test "0x67" {
    try cpu_test.run_test("67.json", log_level);
}

test "0x68" {
    try cpu_test.run_test("68.json", log_level);
}

test "0x69" {
    try cpu_test.run_test("69.json", log_level);
}

test "0x6A" {
    try cpu_test.run_test("6A.json", log_level);
}

test "0x6B" {
    try cpu_test.run_test("6B.json", log_level);
}

test "0x6C" {
    try cpu_test.run_test("6C.json", log_level);
}

test "0x6D" {
    try cpu_test.run_test("6D.json", log_level);
}

test "0x6E" {
    try cpu_test.run_test("6E.json", log_level);
}

test "0x6F" {
    try cpu_test.run_test("6F.json", log_level);
}

test "0x70" {
    try cpu_test.run_test("70.json", log_level);
}

test "0x71" {
    try cpu_test.run_test("71.json", log_level);
}

test "0x72" {
    try cpu_test.run_test("72.json", log_level);
}

test "0x73" {
    try cpu_test.run_test("73.json", log_level);
}

test "0x74" {
    try cpu_test.run_test("74.json", log_level);
}

test "0x75" {
    try cpu_test.run_test("75.json", log_level);
}

test "0x76" {
    try cpu_test.run_test("76.json", log_level);
}

test "0x77" {
    try cpu_test.run_test("77.json", log_level);
}

test "0x78" {
    try cpu_test.run_test("78.json", log_level);
}

test "0x79" {
    try cpu_test.run_test("79.json", log_level);
}

test "0x7A" {
    try cpu_test.run_test("7A.json", log_level);
}

test "0x7B" {
    try cpu_test.run_test("7B.json", log_level);
}

test "0x7C" {
    try cpu_test.run_test("7C.json", log_level);
}

test "0x7D" {
    try cpu_test.run_test("7D.json", log_level);
}

test "0x7E" {
    try cpu_test.run_test("7E.json", log_level);
}

test "0x7F" {
    try cpu_test.run_test("7F.json", log_level);
}

test "0x80" {
    try cpu_test.run_test("80.json", log_level);
}

test "0x81" {
    try cpu_test.run_test("81.json", log_level);
}

test "0x82" {
    try cpu_test.run_test("82.json", log_level);
}

test "0x83" {
    try cpu_test.run_test("83.json", log_level);
}

test "0x84" {
    try cpu_test.run_test("84.json", log_level);
}

test "0x85" {
    try cpu_test.run_test("85.json", log_level);
}

test "0x86" {
    try cpu_test.run_test("86.json", log_level);
}

test "0x87" {
    try cpu_test.run_test("87.json", log_level);
}

test "0x88" {
    try cpu_test.run_test("88.json", log_level);
}

test "0x89" {
    try cpu_test.run_test("89.json", log_level);
}

test "0x8A" {
    try cpu_test.run_test("8A.json", log_level);
}

test "0x8B" {
    try cpu_test.run_test("8B.json", log_level);
}

test "0x8C" {
    try cpu_test.run_test("8C.json", log_level);
}

test "0x8D" {
    try cpu_test.run_test("8D.json", log_level);
}

test "0x8E" {
    try cpu_test.run_test("8E.json", log_level);
}

test "0x8F" {
    try cpu_test.run_test("8F.json", log_level);
}

test "0x90" {
    try cpu_test.run_test("90.json", log_level);
}

test "0x91" {
    try cpu_test.run_test("91.json", log_level);
}

test "0x92" {
    try cpu_test.run_test("92.json", log_level);
}

test "0x93" {
    try cpu_test.run_test("93.json", log_level);
}

test "0x94" {
    try cpu_test.run_test("94.json", log_level);
}

test "0x95" {
    try cpu_test.run_test("95.json", log_level);
}

test "0x96" {
    try cpu_test.run_test("96.json", log_level);
}

test "0x97" {
    try cpu_test.run_test("97.json", log_level);
}

test "0x98" {
    try cpu_test.run_test("98.json", log_level);
}

test "0x99" {
    try cpu_test.run_test("99.json", log_level);
}

test "0x9A" {
    try cpu_test.run_test("9A.json", log_level);
}

test "0x9B" {
    try cpu_test.run_test("9B.json", log_level);
}

test "0x9C" {
    try cpu_test.run_test("9C.json", log_level);
}

test "0x9D" {
    try cpu_test.run_test("9D.json", log_level);
}

test "0x9E" {
    try cpu_test.run_test("9E.json", log_level);
}

test "0x9F" {
    try cpu_test.run_test("9F.json", log_level);
}

test "0xA0" {
    try cpu_test.run_test("A0.json", log_level);
}

test "0xA1" {
    try cpu_test.run_test("A1.json", log_level);
}

test "0xA2" {
    try cpu_test.run_test("A2.json", log_level);
}

test "0xA3" {
    try cpu_test.run_test("A3.json", log_level);
}

test "0xA4" {
    try cpu_test.run_test("A4.json", log_level);
}

test "0xA5" {
    try cpu_test.run_test("A5.json", log_level);
}

test "0xA6" {
    try cpu_test.run_test("A6.json", log_level);
}

test "0xA7" {
    try cpu_test.run_test("A7.json", log_level);
}

test "0xA8" {
    try cpu_test.run_test("A8.json", log_level);
}

test "0xA9" {
    try cpu_test.run_test("A9.json", log_level);
}

test "0xAA" {
    try cpu_test.run_test("AA.json", log_level);
}

test "0xAB" {
    try cpu_test.run_test("AB.json", log_level);
}

test "0xAC" {
    try cpu_test.run_test("AC.json", log_level);
}

test "0xAD" {
    try cpu_test.run_test("AD.json", log_level);
}

test "0xAE" {
    try cpu_test.run_test("AE.json", log_level);
}

test "0xAF" {
    try cpu_test.run_test("AF.json", log_level);
}

test "0xB0" {
    try cpu_test.run_test("B0.json", log_level);
}

test "0xB1" {
    try cpu_test.run_test("B1.json", log_level);
}

test "0xB2" {
    try cpu_test.run_test("B2.json", log_level);
}

test "0xB3" {
    try cpu_test.run_test("B3.json", log_level);
}

test "0xB4" {
    try cpu_test.run_test("B4.json", log_level);
}

test "0xB5" {
    try cpu_test.run_test("B5.json", log_level);
}

test "0xB6" {
    try cpu_test.run_test("B6.json", log_level);
}

test "0xB7" {
    try cpu_test.run_test("B7.json", log_level);
}

test "0xB8" {
    try cpu_test.run_test("B8.json", log_level);
}

test "0xB9" {
    try cpu_test.run_test("B9.json", log_level);
}

test "0xBA" {
    try cpu_test.run_test("BA.json", log_level);
}

test "0xBB" {
    try cpu_test.run_test("BB.json", log_level);
}

test "0xBC" {
    try cpu_test.run_test("BC.json", log_level);
}

test "0xBD" {
    try cpu_test.run_test("BD.json", log_level);
}

test "0xBE" {
    try cpu_test.run_test("BE.json", log_level);
}

test "0xBF" {
    try cpu_test.run_test("BF.json", log_level);
}

test "0xC0" {
    try cpu_test.run_test("C0.json", log_level);
}

test "0xC1" {
    try cpu_test.run_test("C1.json", log_level);
}

test "0xC2" {
    try cpu_test.run_test("C2.json", log_level);
}

test "0xC3" {
    try cpu_test.run_test("C3.json", log_level);
}

test "0xC4" {
    try cpu_test.run_test("C4.json", log_level);
}

test "0xC5" {
    try cpu_test.run_test("C5.json", log_level);
}

test "0xC6" {
    try cpu_test.run_test("C6.json", log_level);
}

test "0xC7" {
    try cpu_test.run_test("C7.json", log_level);
}

test "0xC8" {
    try cpu_test.run_test("C8.json", log_level);
}

test "0xC9" {
    try cpu_test.run_test("C9.json", log_level);
}

test "0xCA" {
    try cpu_test.run_test("CA.json", log_level);
}

test "0xCB 00" {
    try cpu_test.run_test("CB 00.json", log_level);
}
test "0xCB 01" {
    try cpu_test.run_test("CB 01.json", log_level);
}
test "0xCB 02" {
    try cpu_test.run_test("CB 02.json", log_level);
}
test "0xCB 03" {
    try cpu_test.run_test("CB 03.json", log_level);
}
test "0xCB 04" {
    try cpu_test.run_test("CB 04.json", log_level);
}
test "0xCB 05" {
    try cpu_test.run_test("CB 05.json", log_level);
}
test "0xCB 06" {
    try cpu_test.run_test("CB 06.json", log_level);
}
test "0xCB 07" {
    try cpu_test.run_test("CB 07.json", log_level);
}
test "0xCB 08" {
    try cpu_test.run_test("CB 08.json", log_level);
}
test "0xCB 09" {
    try cpu_test.run_test("CB 09.json", log_level);
}
test "0xCB 0A" {
    try cpu_test.run_test("CB 0A.json", log_level);
}
test "0xCB 0B" {
    try cpu_test.run_test("CB 0B.json", log_level);
}
test "0xCB 0C" {
    try cpu_test.run_test("CB 0C.json", log_level);
}
test "0xCB 0D" {
    try cpu_test.run_test("CB 0D.json", log_level);
}
test "0xCB 0E" {
    try cpu_test.run_test("CB 0E.json", log_level);
}
test "0xCB 0F" {
    try cpu_test.run_test("CB 0F.json", log_level);
}
test "0xCB 10" {
    try cpu_test.run_test("CB 10.json", log_level);
}
test "0xCB 11" {
    try cpu_test.run_test("CB 11.json", log_level);
}
test "0xCB 12" {
    try cpu_test.run_test("CB 12.json", log_level);
}
test "0xCB 13" {
    try cpu_test.run_test("CB 13.json", log_level);
}
test "0xCB 14" {
    try cpu_test.run_test("CB 14.json", log_level);
}
test "0xCB 15" {
    try cpu_test.run_test("CB 15.json", log_level);
}
test "0xCB 16" {
    try cpu_test.run_test("CB 16.json", log_level);
}
test "0xCB 17" {
    try cpu_test.run_test("CB 17.json", log_level);
}
test "0xCB 18" {
    try cpu_test.run_test("CB 18.json", log_level);
}
test "0xCB 19" {
    try cpu_test.run_test("CB 19.json", log_level);
}
test "0xCB 1A" {
    try cpu_test.run_test("CB 1A.json", log_level);
}
test "0xCB 1B" {
    try cpu_test.run_test("CB 1B.json", log_level);
}
test "0xCB 1C" {
    try cpu_test.run_test("CB 1C.json", log_level);
}
test "0xCB 1D" {
    try cpu_test.run_test("CB 1D.json", log_level);
}
test "0xCB 1E" {
    try cpu_test.run_test("CB 1E.json", log_level);
}
test "0xCB 1F" {
    try cpu_test.run_test("CB 1F.json", log_level);
}
test "0xCB 20" {
    try cpu_test.run_test("CB 20.json", log_level);
}
test "0xCB 21" {
    try cpu_test.run_test("CB 21.json", log_level);
}
test "0xCB 22" {
    try cpu_test.run_test("CB 22.json", log_level);
}
test "0xCB 23" {
    try cpu_test.run_test("CB 23.json", log_level);
}
test "0xCB 24" {
    try cpu_test.run_test("CB 24.json", log_level);
}
test "0xCB 25" {
    try cpu_test.run_test("CB 25.json", log_level);
}
test "0xCB 26" {
    try cpu_test.run_test("CB 26.json", log_level);
}
test "0xCB 27" {
    try cpu_test.run_test("CB 27.json", log_level);
}
test "0xCB 28" {
    try cpu_test.run_test("CB 28.json", log_level);
}
test "0xCB 29" {
    try cpu_test.run_test("CB 29.json", log_level);
}
test "0xCB 2A" {
    try cpu_test.run_test("CB 2A.json", log_level);
}
test "0xCB 2B" {
    try cpu_test.run_test("CB 2B.json", log_level);
}
test "0xCB 2C" {
    try cpu_test.run_test("CB 2C.json", log_level);
}
test "0xCB 2D" {
    try cpu_test.run_test("CB 2D.json", log_level);
}
test "0xCB 2E" {
    try cpu_test.run_test("CB 2E.json", log_level);
}
test "0xCB 2F" {
    try cpu_test.run_test("CB 2F.json", log_level);
}
test "0xCB 30" {
    try cpu_test.run_test("CB 30.json", log_level);
}
test "0xCB 31" {
    try cpu_test.run_test("CB 31.json", log_level);
}
test "0xCB 32" {
    try cpu_test.run_test("CB 32.json", log_level);
}
test "0xCB 33" {
    try cpu_test.run_test("CB 33.json", log_level);
}
test "0xCB 34" {
    try cpu_test.run_test("CB 34.json", log_level);
}
test "0xCB 35" {
    try cpu_test.run_test("CB 35.json", log_level);
}
test "0xCB 36" {
    try cpu_test.run_test("CB 36.json", log_level);
}
test "0xCB 37" {
    try cpu_test.run_test("CB 37.json", log_level);
}
test "0xCB 38" {
    try cpu_test.run_test("CB 38.json", log_level);
}
test "0xCB 39" {
    try cpu_test.run_test("CB 39.json", log_level);
}
test "0xCB 3A" {
    try cpu_test.run_test("CB 3A.json", log_level);
}
test "0xCB 3B" {
    try cpu_test.run_test("CB 3B.json", log_level);
}
test "0xCB 3C" {
    try cpu_test.run_test("CB 3C.json", log_level);
}
test "0xCB 3D" {
    try cpu_test.run_test("CB 3D.json", log_level);
}
test "0xCB 3E" {
    try cpu_test.run_test("CB 3E.json", log_level);
}
test "0xCB 3F" {
    try cpu_test.run_test("CB 3F.json", log_level);
}
test "0xCB 40" {
    try cpu_test.run_test("CB 40.json", log_level);
}
test "0xCB 41" {
    try cpu_test.run_test("CB 41.json", log_level);
}
test "0xCB 42" {
    try cpu_test.run_test("CB 42.json", log_level);
}
test "0xCB 43" {
    try cpu_test.run_test("CB 43.json", log_level);
}
test "0xCB 44" {
    try cpu_test.run_test("CB 44.json", log_level);
}
test "0xCB 45" {
    try cpu_test.run_test("CB 45.json", log_level);
}
test "0xCB 46" {
    try cpu_test.run_test("CB 46.json", log_level);
}
test "0xCB 47" {
    try cpu_test.run_test("CB 47.json", log_level);
}
test "0xCB 48" {
    try cpu_test.run_test("CB 48.json", log_level);
}
test "0xCB 49" {
    try cpu_test.run_test("CB 49.json", log_level);
}
test "0xCB 4A" {
    try cpu_test.run_test("CB 4A.json", log_level);
}
test "0xCB 4B" {
    try cpu_test.run_test("CB 4B.json", log_level);
}
test "0xCB 4C" {
    try cpu_test.run_test("CB 4C.json", log_level);
}
test "0xCB 4D" {
    try cpu_test.run_test("CB 4D.json", log_level);
}
test "0xCB 4E" {
    try cpu_test.run_test("CB 4E.json", log_level);
}
test "0xCB 4F" {
    try cpu_test.run_test("CB 4F.json", log_level);
}
test "0xCB 50" {
    try cpu_test.run_test("CB 50.json", log_level);
}
test "0xCB 51" {
    try cpu_test.run_test("CB 51.json", log_level);
}
test "0xCB 52" {
    try cpu_test.run_test("CB 52.json", log_level);
}
test "0xCB 53" {
    try cpu_test.run_test("CB 53.json", log_level);
}
test "0xCB 54" {
    try cpu_test.run_test("CB 54.json", log_level);
}
test "0xCB 55" {
    try cpu_test.run_test("CB 55.json", log_level);
}
test "0xCB 56" {
    try cpu_test.run_test("CB 56.json", log_level);
}
test "0xCB 57" {
    try cpu_test.run_test("CB 57.json", log_level);
}
test "0xCB 58" {
    try cpu_test.run_test("CB 58.json", log_level);
}
test "0xCB 59" {
    try cpu_test.run_test("CB 59.json", log_level);
}
test "0xCB 5A" {
    try cpu_test.run_test("CB 5A.json", log_level);
}
test "0xCB 5B" {
    try cpu_test.run_test("CB 5B.json", log_level);
}
test "0xCB 5C" {
    try cpu_test.run_test("CB 5C.json", log_level);
}
test "0xCB 5D" {
    try cpu_test.run_test("CB 5D.json", log_level);
}
test "0xCB 5E" {
    try cpu_test.run_test("CB 5E.json", log_level);
}
test "0xCB 5F" {
    try cpu_test.run_test("CB 5F.json", log_level);
}
test "0xCB 60" {
    try cpu_test.run_test("CB 60.json", log_level);
}
test "0xCB 61" {
    try cpu_test.run_test("CB 61.json", log_level);
}
test "0xCB 62" {
    try cpu_test.run_test("CB 62.json", log_level);
}
test "0xCB 63" {
    try cpu_test.run_test("CB 63.json", log_level);
}
test "0xCB 64" {
    try cpu_test.run_test("CB 64.json", log_level);
}
test "0xCB 65" {
    try cpu_test.run_test("CB 65.json", log_level);
}
test "0xCB 66" {
    try cpu_test.run_test("CB 66.json", log_level);
}
test "0xCB 67" {
    try cpu_test.run_test("CB 67.json", log_level);
}
test "0xCB 68" {
    try cpu_test.run_test("CB 68.json", log_level);
}
test "0xCB 69" {
    try cpu_test.run_test("CB 69.json", log_level);
}
test "0xCB 6A" {
    try cpu_test.run_test("CB 6A.json", log_level);
}
test "0xCB 6B" {
    try cpu_test.run_test("CB 6B.json", log_level);
}
test "0xCB 6C" {
    try cpu_test.run_test("CB 6C.json", log_level);
}
test "0xCB 6D" {
    try cpu_test.run_test("CB 6D.json", log_level);
}
test "0xCB 6E" {
    try cpu_test.run_test("CB 6E.json", log_level);
}
test "0xCB 6F" {
    try cpu_test.run_test("CB 6F.json", log_level);
}
test "0xCB 70" {
    try cpu_test.run_test("CB 70.json", log_level);
}
test "0xCB 71" {
    try cpu_test.run_test("CB 71.json", log_level);
}
test "0xCB 72" {
    try cpu_test.run_test("CB 72.json", log_level);
}
test "0xCB 73" {
    try cpu_test.run_test("CB 73.json", log_level);
}
test "0xCB 74" {
    try cpu_test.run_test("CB 74.json", log_level);
}
test "0xCB 75" {
    try cpu_test.run_test("CB 75.json", log_level);
}
test "0xCB 76" {
    try cpu_test.run_test("CB 76.json", log_level);
}
test "0xCB 77" {
    try cpu_test.run_test("CB 77.json", log_level);
}
test "0xCB 78" {
    try cpu_test.run_test("CB 78.json", log_level);
}
test "0xCB 79" {
    try cpu_test.run_test("CB 79.json", log_level);
}
test "0xCB 7A" {
    try cpu_test.run_test("CB 7A.json", log_level);
}
test "0xCB 7B" {
    try cpu_test.run_test("CB 7B.json", log_level);
}
test "0xCB 7C" {
    try cpu_test.run_test("CB 7C.json", log_level);
}
test "0xCB 7D" {
    try cpu_test.run_test("CB 7D.json", log_level);
}
test "0xCB 7E" {
    try cpu_test.run_test("CB 7E.json", log_level);
}
test "0xCB 7F" {
    try cpu_test.run_test("CB 7F.json", log_level);
}
test "0xCB 80" {
    try cpu_test.run_test("CB 80.json", log_level);
}
test "0xCB 81" {
    try cpu_test.run_test("CB 81.json", log_level);
}
test "0xCB 82" {
    try cpu_test.run_test("CB 82.json", log_level);
}
test "0xCB 83" {
    try cpu_test.run_test("CB 83.json", log_level);
}
test "0xCB 84" {
    try cpu_test.run_test("CB 84.json", log_level);
}
test "0xCB 85" {
    try cpu_test.run_test("CB 85.json", log_level);
}
test "0xCB 86" {
    try cpu_test.run_test("CB 86.json", log_level);
}
test "0xCB 87" {
    try cpu_test.run_test("CB 87.json", log_level);
}
test "0xCB 88" {
    try cpu_test.run_test("CB 88.json", log_level);
}
test "0xCB 89" {
    try cpu_test.run_test("CB 89.json", log_level);
}
test "0xCB 8A" {
    try cpu_test.run_test("CB 8A.json", log_level);
}
test "0xCB 8B" {
    try cpu_test.run_test("CB 8B.json", log_level);
}
test "0xCB 8C" {
    try cpu_test.run_test("CB 8C.json", log_level);
}
test "0xCB 8D" {
    try cpu_test.run_test("CB 8D.json", log_level);
}
test "0xCB 8E" {
    try cpu_test.run_test("CB 8E.json", log_level);
}
test "0xCB 8F" {
    try cpu_test.run_test("CB 8F.json", log_level);
}
test "0xCB 90" {
    try cpu_test.run_test("CB 90.json", log_level);
}
test "0xCB 91" {
    try cpu_test.run_test("CB 91.json", log_level);
}
test "0xCB 92" {
    try cpu_test.run_test("CB 92.json", log_level);
}
test "0xCB 93" {
    try cpu_test.run_test("CB 93.json", log_level);
}
test "0xCB 94" {
    try cpu_test.run_test("CB 94.json", log_level);
}
test "0xCB 95" {
    try cpu_test.run_test("CB 95.json", log_level);
}
test "0xCB 96" {
    try cpu_test.run_test("CB 96.json", log_level);
}
test "0xCB 97" {
    try cpu_test.run_test("CB 97.json", log_level);
}
test "0xCB 98" {
    try cpu_test.run_test("CB 98.json", log_level);
}
test "0xCB 99" {
    try cpu_test.run_test("CB 99.json", log_level);
}
test "0xCB 9A" {
    try cpu_test.run_test("CB 9A.json", log_level);
}
test "0xCB 9B" {
    try cpu_test.run_test("CB 9B.json", log_level);
}
test "0xCB 9C" {
    try cpu_test.run_test("CB 9C.json", log_level);
}
test "0xCB 9D" {
    try cpu_test.run_test("CB 9D.json", log_level);
}
test "0xCB 9E" {
    try cpu_test.run_test("CB 9E.json", log_level);
}
test "0xCB 9F" {
    try cpu_test.run_test("CB 9F.json", log_level);
}
test "0xCB A0" {
    try cpu_test.run_test("CB A0.json", log_level);
}
test "0xCB A1" {
    try cpu_test.run_test("CB A1.json", log_level);
}
test "0xCB A2" {
    try cpu_test.run_test("CB A2.json", log_level);
}
test "0xCB A3" {
    try cpu_test.run_test("CB A3.json", log_level);
}
test "0xCB A4" {
    try cpu_test.run_test("CB A4.json", log_level);
}
test "0xCB A5" {
    try cpu_test.run_test("CB A5.json", log_level);
}
test "0xCB A6" {
    try cpu_test.run_test("CB A6.json", log_level);
}
test "0xCB A7" {
    try cpu_test.run_test("CB A7.json", log_level);
}
test "0xCB A8" {
    try cpu_test.run_test("CB A8.json", log_level);
}
test "0xCB A9" {
    try cpu_test.run_test("CB A9.json", log_level);
}
test "0xCB AA" {
    try cpu_test.run_test("CB AA.json", log_level);
}
test "0xCB AB" {
    try cpu_test.run_test("CB AB.json", log_level);
}
test "0xCB AC" {
    try cpu_test.run_test("CB AC.json", log_level);
}
test "0xCB AD" {
    try cpu_test.run_test("CB AD.json", log_level);
}
test "0xCB AE" {
    try cpu_test.run_test("CB AE.json", log_level);
}
test "0xCB AF" {
    try cpu_test.run_test("CB AF.json", log_level);
}
test "0xCB B0" {
    try cpu_test.run_test("CB B0.json", log_level);
}
test "0xCB B1" {
    try cpu_test.run_test("CB B1.json", log_level);
}
test "0xCB B2" {
    try cpu_test.run_test("CB B2.json", log_level);
}
test "0xCB B3" {
    try cpu_test.run_test("CB B3.json", log_level);
}
test "0xCB B4" {
    try cpu_test.run_test("CB B4.json", log_level);
}
test "0xCB B5" {
    try cpu_test.run_test("CB B5.json", log_level);
}
test "0xCB B6" {
    try cpu_test.run_test("CB B6.json", log_level);
}
test "0xCB B7" {
    try cpu_test.run_test("CB B7.json", log_level);
}
test "0xCB B8" {
    try cpu_test.run_test("CB B8.json", log_level);
}
test "0xCB B9" {
    try cpu_test.run_test("CB B9.json", log_level);
}
test "0xCB BA" {
    try cpu_test.run_test("CB BA.json", log_level);
}
test "0xCB BB" {
    try cpu_test.run_test("CB BB.json", log_level);
}
test "0xCB BC" {
    try cpu_test.run_test("CB BC.json", log_level);
}
test "0xCB BD" {
    try cpu_test.run_test("CB BD.json", log_level);
}
test "0xCB BE" {
    try cpu_test.run_test("CB BE.json", log_level);
}
test "0xCB BF" {
    try cpu_test.run_test("CB BF.json", log_level);
}
test "0xCB C0" {
    try cpu_test.run_test("CB C0.json", log_level);
}
test "0xCB C1" {
    try cpu_test.run_test("CB C1.json", log_level);
}
test "0xCB C2" {
    try cpu_test.run_test("CB C2.json", log_level);
}
test "0xCB C3" {
    try cpu_test.run_test("CB C3.json", log_level);
}
test "0xCB C4" {
    try cpu_test.run_test("CB C4.json", log_level);
}
test "0xCB C5" {
    try cpu_test.run_test("CB C5.json", log_level);
}
test "0xCB C6" {
    try cpu_test.run_test("CB C6.json", log_level);
}
test "0xCB C7" {
    try cpu_test.run_test("CB C7.json", log_level);
}
test "0xCB C8" {
    try cpu_test.run_test("CB C8.json", log_level);
}
test "0xCB C9" {
    try cpu_test.run_test("CB C9.json", log_level);
}
test "0xCB CA" {
    try cpu_test.run_test("CB CA.json", log_level);
}
test "0xCB CB" {
    try cpu_test.run_test("CB CB.json", log_level);
}
test "0xCB CC" {
    try cpu_test.run_test("CB CC.json", log_level);
}
test "0xCB CD" {
    try cpu_test.run_test("CB CD.json", log_level);
}
test "0xCB CE" {
    try cpu_test.run_test("CB CE.json", log_level);
}
test "0xCB CF" {
    try cpu_test.run_test("CB CF.json", log_level);
}
test "0xCB D0" {
    try cpu_test.run_test("CB D0.json", log_level);
}
test "0xCB D1" {
    try cpu_test.run_test("CB D1.json", log_level);
}
test "0xCB D2" {
    try cpu_test.run_test("CB D2.json", log_level);
}
test "0xCB D3" {
    try cpu_test.run_test("CB D3.json", log_level);
}
test "0xCB D4" {
    try cpu_test.run_test("CB D4.json", log_level);
}
test "0xCB D5" {
    try cpu_test.run_test("CB D5.json", log_level);
}
test "0xCB D6" {
    try cpu_test.run_test("CB D6.json", log_level);
}
test "0xCB D7" {
    try cpu_test.run_test("CB D7.json", log_level);
}
test "0xCB D8" {
    try cpu_test.run_test("CB D8.json", log_level);
}
test "0xCB D9" {
    try cpu_test.run_test("CB D9.json", log_level);
}
test "0xCB DA" {
    try cpu_test.run_test("CB DA.json", log_level);
}
test "0xCB DB" {
    try cpu_test.run_test("CB DB.json", log_level);
}
test "0xCB DC" {
    try cpu_test.run_test("CB DC.json", log_level);
}
test "0xCB DD" {
    try cpu_test.run_test("CB DD.json", log_level);
}
test "0xCB DE" {
    try cpu_test.run_test("CB DE.json", log_level);
}
test "0xCB DF" {
    try cpu_test.run_test("CB DF.json", log_level);
}
test "0xCB E0" {
    try cpu_test.run_test("CB E0.json", log_level);
}
test "0xCB E1" {
    try cpu_test.run_test("CB E1.json", log_level);
}
test "0xCB E2" {
    try cpu_test.run_test("CB E2.json", log_level);
}
test "0xCB E3" {
    try cpu_test.run_test("CB E3.json", log_level);
}
test "0xCB E4" {
    try cpu_test.run_test("CB E4.json", log_level);
}
test "0xCB E5" {
    try cpu_test.run_test("CB E5.json", log_level);
}
test "0xCB E6" {
    try cpu_test.run_test("CB E6.json", log_level);
}
test "0xCB E7" {
    try cpu_test.run_test("CB E7.json", log_level);
}
test "0xCB E8" {
    try cpu_test.run_test("CB E8.json", log_level);
}
test "0xCB E9" {
    try cpu_test.run_test("CB E9.json", log_level);
}
test "0xCB EA" {
    try cpu_test.run_test("CB EA.json", log_level);
}
test "0xCB EB" {
    try cpu_test.run_test("CB EB.json", log_level);
}
test "0xCB EC" {
    try cpu_test.run_test("CB EC.json", log_level);
}
test "0xCB ED" {
    try cpu_test.run_test("CB ED.json", log_level);
}
test "0xCB EE" {
    try cpu_test.run_test("CB EE.json", log_level);
}
test "0xCB EF" {
    try cpu_test.run_test("CB EF.json", log_level);
}
test "0xCB F0" {
    try cpu_test.run_test("CB F0.json", log_level);
}
test "0xCB F1" {
    try cpu_test.run_test("CB F1.json", log_level);
}
test "0xCB F2" {
    try cpu_test.run_test("CB F2.json", log_level);
}
test "0xCB F3" {
    try cpu_test.run_test("CB F3.json", log_level);
}
test "0xCB F4" {
    try cpu_test.run_test("CB F4.json", log_level);
}
test "0xCB F5" {
    try cpu_test.run_test("CB F5.json", log_level);
}
test "0xCB F6" {
    try cpu_test.run_test("CB F6.json", log_level);
}
test "0xCB F7" {
    try cpu_test.run_test("CB F7.json", log_level);
}
test "0xCB F8" {
    try cpu_test.run_test("CB F8.json", log_level);
}
test "0xCB F9" {
    try cpu_test.run_test("CB F9.json", log_level);
}
test "0xCB FA" {
    try cpu_test.run_test("CB FA.json", log_level);
}
test "0xCB FB" {
    try cpu_test.run_test("CB FB.json", log_level);
}
test "0xCB FC" {
    try cpu_test.run_test("CB FC.json", log_level);
}
test "0xCB FD" {
    try cpu_test.run_test("CB FD.json", log_level);
}
test "0xCB FE" {
    try cpu_test.run_test("CB FE.json", log_level);
}
test "0xCB FF" {
    try cpu_test.run_test("CB FF.json", log_level);
}

test "0xCC" {
    try cpu_test.run_test("CC.json", log_level);
}

test "0xCD" {
    try cpu_test.run_test("CD.json", log_level);
}

test "0xCE" {
    try cpu_test.run_test("CE.json", log_level);
}

test "0xCF" {
    try cpu_test.run_test("CF.json", log_level);
}

test "0xD0" {
    try cpu_test.run_test("D0.json", log_level);
}

test "0xD1" {
    try cpu_test.run_test("D1.json", log_level);
}

test "0xD2" {
    try cpu_test.run_test("D2.json", log_level);
}

test "0xD4" {
    try cpu_test.run_test("D4.json", log_level);
}

test "0xD5" {
    try cpu_test.run_test("D5.json", log_level);
}

test "0xD6" {
    try cpu_test.run_test("D6.json", log_level);
}

test "0xD7" {
    try cpu_test.run_test("D7.json", log_level);
}

test "0xD8" {
    try cpu_test.run_test("D8.json", log_level);
}

test "0xD9" {
    try cpu_test.run_test("D9.json", log_level);
}

test "0xDA" {
    try cpu_test.run_test("DA.json", log_level);
}

test "0xDC" {
    try cpu_test.run_test("DC.json", log_level);
}

test "0xDE" {
    try cpu_test.run_test("DE.json", log_level);
}

test "0xDF" {
    try cpu_test.run_test("DF.json", log_level);
}

test "0xE0" {
    try cpu_test.run_test("E0.json", log_level);
}

test "0xE1" {
    try cpu_test.run_test("E1.json", log_level);
}

test "0xE2" {
    try cpu_test.run_test("E2.json", log_level);
}

test "0xE5" {
    try cpu_test.run_test("E5.json", log_level);
}

test "0xE6" {
    try cpu_test.run_test("E6.json", log_level);
}

test "0xE7" {
    try cpu_test.run_test("E7.json", log_level);
}

test "0xE8" {
    try cpu_test.run_test("E8.json", log_level);
}

test "0xE9" {
    try cpu_test.run_test("E9.json", log_level);
}

test "0xEA" {
    try cpu_test.run_test("EA.json", log_level);
}

test "0xEE" {
    try cpu_test.run_test("EE.json", log_level);
}

test "0xEF" {
    try cpu_test.run_test("EF.json", log_level);
}

test "0xF0" {
    try cpu_test.run_test("F0.json", log_level);
}

test "0xF1" {
    try cpu_test.run_test("F1.json", log_level);
}

test "0xF2" {
    try cpu_test.run_test("F2.json", log_level);
}

test "0xF3" {
    try cpu_test.run_test("F3.json", log_level);
}

test "0xF5" {
    try cpu_test.run_test("F5.json", log_level);
}

test "0xF6" {
    try cpu_test.run_test("F6.json", log_level);
}

test "0xF7" {
    try cpu_test.run_test("F7.json", log_level);
}

test "0xF8" {
    try cpu_test.run_test("F8.json", log_level);
}

test "0xF9" {
    try cpu_test.run_test("F9.json", log_level);
}

test "0xFA" {
    try cpu_test.run_test("FA.json", log_level);
}

test "0xFB" {
    try cpu_test.run_test("FB.json", log_level);
}

test "0xFE" {
    try cpu_test.run_test("FE.json", log_level);
}

test "0xFF" {
    try cpu_test.run_test("FF.json", log_level);
}
