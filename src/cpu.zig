const Memory = @import("memory.zig").Memory;
pub const Registers = struct {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    pub fn get_bc(self: *Registers) u16 {
        const _u16: u16 = @as(u16, self.b) << 8 | self.c;
        return _u16;
    }

    pub fn set_bc(self: *Registers, value: u16) void {
        self.b = @truncate(value & 0xFF00 >> 8);
        self.c = @truncate(value & 0xFF);
    }

    pub fn get_hl(self: *Registers) u16 {
        return (@as(u16, self.h) << 8) | self.l;
    }

    pub fn set_hl(self: *Registers, value: u16) void {
        self.h = @truncate(value & 0xFF00 >> 8);
        self.l = @truncate(value & 0xFF);
    }

    pub fn get_de(self: *Registers) u16 {
        return (@as(u16, self.d) << 8) | self.e;
    }

    pub fn set_de(self: *Registers, value: u16) void {
        self.d = @truncate(value & 0xFF00 >> 8);
        self.e = @truncate(value & 0xFF);
    }
};

pub const Flags = struct {
    zero: bool, // Z flag
    subtract: bool, // N flag
    half_carry: bool, // H flag
    carry: bool, // C flag

    pub fn set(self: *Flags, zero: bool, subtract: bool, half_carry: bool, carry: bool) void {
        self.zero = zero;
        self.subtract = subtract;
        self.half_carry = half_carry;
        self.carry = carry;
    }
};

pub const CPU = struct {
    registers: Registers,
    flags: Flags,
    sp: u16,
    pc: u16,
    halted: bool,
    disable_interrupt: u2,
    enable_interrupt: u2,
    memory: Memory,

    pub fn init(registers: Registers, flags: Flags, memory: Memory) CPU {
        return CPU{
            .registers = registers,
            .flags = flags,
            .sp = 0,
            .pc = 0,
            .halted = false,
            .disable_interrupt = 0,
            .enable_interrupt = 0,
            .memory = memory,
        };
    }

    pub fn pop_stack(self: *CPU) u16 {
        const value = self.memory.read_word(self.pc);
        self.pc += 2;
        return value;
    }

    pub fn push_stack(self: *CPU, value: u16) void {
        self.pc -= 2;
        self.memory.write_word(self.pc, value);
    }

    pub fn next_byte(self: *CPU) u8 {
        const byte = self.memory.read_byte(self.pc);
        self.pc += 1;
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
        const result = @addWithOverflow(@addWithOverflow(a, c)[0], value)[0];
        self.registers.a = result;
        const zero_flag = result == 0;
        const half_carry_flag = (a & 0xF) + (value & 0xF) + c > 0xF; // carry at 4 bit
        const carry_flag = @as(u16, a) + @as(u16, value) + @as(u16, c) > 0xFF; // carry at 7 bit
        self.flags.set(zero_flag, false, half_carry_flag, carry_flag);
    }

    fn sub8(self: *CPU, value: u8, carry: bool) void {
        const a = self.registers.a;
        const c: u8 = if (self.flags.carry and carry) 1 else 0;
        const result = @subWithOverflow(@subWithOverflow(a, c)[0], value)[0];
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
        const result = @addWithOverflow(value, 1)[0];
        const zero_flag = result == 0;
        const half_carry_flag = (value & 0xF) + 1 > 0xF;
        self.flags.set(zero_flag, false, half_carry_flag, self.flags.carry);
        return result;
    }

    fn dec(self: *CPU, value: u8) u8 {
        const result = @subWithOverflow(value, 1)[0];
        const zero_flag = result == 0;
        const half_carry_flag = (value & 0xF) == 0;
        self.flags.set(zero_flag, true, half_carry_flag, self.flags.carry);
        return result;
    }

    fn add16(self: *CPU, value: u16) void {
        const hl = self.registers.get_hl();
        const result = @addWithOverflow(hl, value)[0];
        self.registers.set_hl(result);
        const half_carry_flag = (hl & 0xFFF) + (value & 0xFFF) > 0xFFF; // carry at 11 bit
        const carry_flag = hl > 0xFFF - value; // carry at 15 bit
        self.flags.set(false, false, half_carry_flag, carry_flag);
    }

    fn addSP(self: *CPU, value: u8) void {
        const sp = self.sp;
        const value16 = @as(u16, value);
        self.sp = @addWithOverflow(sp, value16)[0];
        const half_carry_flag = (sp & 0xF) + (value16 & 0xF) > (value & 0xF);
        const carry_flag = (sp & 0xFF) + (value16 & 0xFF) > (value & 0xFF);
        self.flags.set(false, false, half_carry_flag, carry_flag);
    }

    fn swap(self: *CPU, value: u8) u8 {
        self.flags.set(value == 0, true, false, false);
        return (value >> 4) | (value << 4);
    }

    fn daa(self: *CPU) struct { u8, bool } {
        var offset: u8 = 0;
        var should_carry = false;
        const a = self.registers.a;
        const flags = &self.flags;
        if ((flags.subtract and a & 0xF > 0x09) or flags.half_carry) {
            offset |= 0x06;
        }
        if ((flags.subtract and a > 0x99) or self.flags.carry) {
            offset |= 0x60;
            should_carry = true;
        }
        flags.set(a == 0, flags.subtract, false, should_carry);
        return .{ if (flags.subtract) @subWithOverflow(a, offset)[0] else @addWithOverflow(a, offset)[0], should_carry };
    }

    pub fn rlca(self: *CPU, value: u8) u8 {
        const carry = (value & 0x80) >> 7 == 1;
        const result = (value << 1) | @intFromBool(carry);
        self.flags.set(result == 0, false, false, carry);
        return result;
    }

    pub fn rla(self: *CPU, value: u8) u8 {
        const carry: u8 = value & 0x80;
        const rotated = (value << 1) | carry;
        self.flags.set(rotated == 0, false, false, carry != 0);
        return rotated;
    }

    pub fn rrca(self: *CPU, value: u8) u8 {
        const carry = value & 1 == 1;
        const result = (value >> 1) | @intFromBool(carry);
        self.flags.set(result == 0, false, false, carry);
        return value;
    }

    pub fn rra(self: *CPU, value: u8) u8 {
        const carry: u8 = value & 1;
        const rotated = (value >> 1) | carry;
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

    pub fn bit(self: *CPU, value: u8, register: u8) void {
        const shift: u8 = switch (register) {
            0 => 0,
            1 => 2,
            2 => 4,
            3 => 8,
            4 => 16,
            5 => 32,
            6 => 64,
            else => 128,
        };
        const result = value & shift == 0;
        self.flags.set(result, false, true, self.flags.carry);
    }

    pub fn jr(self: *CPU, value: u8) void {
        const value16 = @as(u16, value);
        self.pc += value16;
    }

    const std = @import("std");
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
                self.registers.set_bc(@addWithOverflow(bc, 1)[0]);
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
                self.registers.set_bc(@subWithOverflow(bc, 1)[0]);
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
                self.registers.set_de(@addWithOverflow(self.registers.get_de(), 1)[0]);
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
                self.registers.a = self.rlca(self.registers.a);
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
                self.registers.set_de(@subWithOverflow(de, 1)[0]);
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
                const byte = self.memory.read_byte(hl);
                self.registers.a = byte;
                self.registers.set_hl(@addWithOverflow(hl, 1)[0]);
                return 8;
            },
            0x23 => { // INC HL
                const hl = self.registers.get_hl();
                self.registers.set_hl(@addWithOverflow(hl, 1)[0]);
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
            0x2A => { // LA A, [HL+]
                const hl = self.registers.get_hl();
                self.registers.a = self.memory.read_byte(hl);
                self.registers.set_hl(@addWithOverflow(hl, 1)[0]);
                return 8;
            },
            0x2B => { // DEC HL
                const hl = self.registers.get_hl();
                self.registers.set_hl(@subWithOverflow(hl, 1)[0]);
                return 8;
            },
            0x2C => { // INC L
                self.registers.l = @addWithOverflow(self.registers.l, 1)[0];
                return 4;
            },
            0x2D => { // DEC L
                self.registers.l = @subWithOverflow(self.registers.l, 1)[0];
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
                const byte = self.memory.read_byte(hl);
                self.registers.a = byte;
                self.registers.set_hl(@addWithOverflow(hl, 1)[0]);
                return 8;
            },
            0x33 => { // INC SP
                const sp = self.sp;
                self.sp = @subWithOverflow(sp, 1)[0];
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
                self.registers.set_hl(@subWithOverflow(hl, 1)[0]);
                return 8;
            },
            0x3B => { // DEC SP
                self.sp = @subWithOverflow(self.sp, 1)[0];
                return 8;
            },
            0x3C => { // INC A
                self.registers.a = @addWithOverflow(self.registers.l, 1)[0];
                return 4;
            },
            0x3D => { // DEC A
                self.registers.a = @subWithOverflow(self.registers.l, 1)[0];
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
                self.registers.b = self.registers.c;
                return 4;
            },
            0x48 => { // LD C, B
                self.registers.c = self.registers.c;
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
                self.memory.write_byte(self.registers.get_hl(), self.registers.b);
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
                self.add8(self.registers.b, false);
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
                self.push_stack(word);
                self.pc = word;
                return 24;
            },
            0xCD => { // CALL a16
                const word = self.next_word();
                self.push_stack(word);
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
                self.registers.set_bc(self.pop_stack());
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
                const word = self.pop_stack();
                self.pc = word;
                self.enable_interrupt = 1;
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
                self.push_stack(word);
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
                self.pc = self.next_word();
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
                self.flags.zero = value & 0x80 != 0;
                self.flags.subtract = value & 0x40 != 0;
                self.flags.half_carry = value & 0x20 != 0;
                self.flags.carry = value & 0x10 != 0;
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
                var af = @as(u16, self.registers.a);
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
                const sp = self.sp;
                const byte = self.next_byte();
                const half_carry = (sp & 0xF) + (byte & 0xF) > 0xF;
                const carry = (sp & 0xFF) + (byte & 0xFF) > 0xFF;
                self.flags.set(false, false, half_carry, carry);
                const value = @addWithOverflow(sp, byte)[0];
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
            else => std.debug.print("Opcode [{d}] is not implemented yet.", .{opcode}),
        }
        return 0;
    }
};
