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
        return (@as(u16, self.b) << 8) | self.c;
    }

    pub fn set_bc(self: *Registers, value: u16) void {
        self.b = @as(u8, (value & 0xFF00) >> 8);
        self.c = @as(u8, (value & 0xFF));
    }

    pub fn get_hl(self: *Registers) u16 {
        return (@as(u16, self.h) << 8) | self.l;
    }

    pub fn set_hl(self: *Registers, value: u16) void {
        self.h = @as(u8, (value & 0xFF00) >> 8);
        self.l = @as(u8, (value & 0xFF));
    }

    pub fn get_de(self: *Registers) u16 {
        return (@as(u16, self.d) << 8) | self.e;
    }

    pub fn set_de(self: *Registers, value: u16) void {
        self.d = @as(u8, (value & 0xFF00) >> 8);
        self.e = @as(u8, (value & 0xFF));
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

    pub fn next_byte(self: *CPU) u8 {
        const byte = self.memory.read_next_byte(self.pc);
        self.pc += 1;
        return byte;
    }

    pub fn nop() void {}

    pub fn ld_bc_n16(self: *CPU, value: u16) void {
        self.*.registers.b = value & 0xFF00;
        self.*.registers.c = value & 0xFF;
    }

    fn add8(self: *CPU, value: u8, carry: bool) void {
        const a = self.registers.a;
        const c: u8 = if (self.flags.carry & carry) 1 else 0;
        const result = @addWithOverflow(@addWithOverflow(a, c), value);
        self.a = result;
        const zero_flag = result == 0;
        const half_carry_flag = (a & 0xF) + (value & 0xF) + c > 0xF; // carry at 4 bit
        const carry_flag = @as(u16, a) + @as(u16, value) + @as(u16, c) > 0xFF; // carry at 7 bit
        self.flags.set(zero_flag, false, half_carry_flag, carry_flag);
    }

    fn sub8(self: *CPU, value: u8, carry: bool) void {
        const a = self.registers.a;
        const c: u8 = if (self.flags.carry & carry) 1 else 0;
        const result = @subWithOverflow(@subWithOverflow(a, c), value);
        self.a = result;
        const zero_flag = result == 0;
        const half_carry_flag = (a & 0xF) < (value & 0xF) + c;
        const carry_flag = @as(u16, a) < @as(u16, value) + @as(u16, c);
        self.flags.set(zero_flag, true, half_carry_flag, carry_flag);
    }

    fn andA(self: *CPU, value: u8) void {
        const result = self.a & value;
        self.a = result;
        const zero_flag = result == 0;
        self.flags.set(zero_flag, false, true, false);
    }

    fn orA(self: *CPU, value: u8) void {
        const result = self.a | value;
        self.a = result;
        const zero_flag = result == 0;
        self.flags.set(zero_flag, false, false, false);
    }

    fn xor(self: *CPU, value: u8) void {
        const result = self.a ^ value;
        self.a = result;
        const zero_flag = result == 0;
        self.flags.set(zero_flag, false, false, false);
    }

    fn cp(self: *CPU, value: u8) void {
        const a = self.a;
        sub8(self, value, false);
        self.a = a;
    }

    fn inc(self: *CPU, value: u8) u8 {
        const result = @addWithOverflow(value, 1);
        const zero_flag = result == 0;
        const half_carry_flag = (value & 0xF) + 1 > 0xF;
        self.flags.set(zero_flag, false, half_carry_flag, self.flags.carry);
        return result;
    }

    fn dec(self: *CPU, value: u8) u8 {
        const result = @subWithOverflow(value, 1);
        const zero_flag = result == 0;
        const half_carry_flag = (value & 0xF) == 0;
        self.flags.set(zero_flag, true, half_carry_flag, self.flags.carry);
        return result;
    }

    fn add16(self: *CPU, value: u16) void {
        const hl = self.registers.get_hl();
        const result = @addWithOverflow(hl, value);
        self.a = result;
        const half_carry_flag = (hl & 0xFFF) + (value & 0xFFF) > 0xFFF; // carry at 11 bit
        const carry_flag = hl > 0xFFF - value; // carry at 15 bit
        self.flags.set(false, false, half_carry_flag, carry_flag);
    }

    fn addSP(self: *CPU, value: u8) void {
        const sp = self.registers.sp;
        const x = @as(u16, value);
        self.sp = @addWithOverflow(sp, x);
        const half_carry_flag = (sp & 0xF) + (x & 0xF) > (value & 0xF);
        const carry_flag = (sp & 0xFF) + (x & 0xFF) > (value & 0xFF);
        self.flags.set(false, true, half_carry_flag, carry_flag);
    }

    fn swap(self: *CPU, value: u8) u8 {
        self.flags.set(value == 0, true, false, false);
        return (value >> 4) | (value << 4);
    }

    fn daa(self: *CPU) struct { u8, bool } {
        var offset: u8 = 0;
        var should_carry = false;
        const a = self.registers.a;
        const flags = self.flags;
        if ((flags.subtract and a & 0xF > 0x09) || flags.half_carry) {
            offset |= 0x06;
        }
        if ((flags.subtract and a > 0x99) || self.flags.carry) {
            offset |= 0x60;
            should_carry = true;
        }
        flags.set(a == 0, flags.subtract, false);
        return .{ if (flags.subtract) @subWithOverflow(a, offset) else @addWithOverflow(a, offset), should_carry };
        // set value into a after returning, set zero flag true if a is zero, reset half carry, set carry to carry
    }

    pub fn rlca(self: *CPU, value: u8) u8 {
        const carry = (value & 0x80) >> 7 == 1;
        const result = (value << 1) | @as(u8, carry);
        self.flags.set(result == 0, false, false, carry);
        return result;
    }

    pub fn rla(self: *CPU, value: u8) u8 {
        const carry: u8 = value & 0x80;
        const rotated = (value << 1) | carry;
        self.flags.set(rotated == 0, false, false, carry != 0);
    }

    pub fn rrca(self: *CPU, value: u8) u8 {
        const carry = value & 1 == 1;
        const result = (value >> 1) | @as(u8, carry);
        self.flags.set(result == 0, false, false, carry);
        return value;
    }

    pub fn rra(self: *CPU, value: u8) u8 {
        const carry: u8 = value & 1;
        const rotated = (value >> 1) | carry;
        self.flags.set(rotated == 0, false, false, carry != 0);
    }

    pub fn jr(self: *CPU, value: u8) void {
        const signed = @as(i16, value);
        self.pc = @as(i16, self.pc) + signed;
    }

    const std = @import("std");
    pub fn execute(self: *CPU, opcode: u8) u32 {
        switch (opcode) {
            0x00 => { // NOP
                return 4;
            },
            0x03 => { // INC BC
                const bc = self.registers.get_bc();
                self.registers.set_bc(@addWithOverflow(bc, 1));
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
                const word = self.memory.read_word();
                self.memory.write_word(word, self.registers.sp);
                return 20;
            },
            0x09 => { // ADD HL, BC
                self.add16(self.registers.get_bc());
                return 8;
            },
            0x0A => { // LD A, [BC]
                self.a = self.memory.read_byte(self.registers.get_bc());
                return 8;
            },
            0x0B => { // DEC BC
                const bc = self.registers.get_bc();
                self.registers.set_bc(@subWithOverflow(bc, 1));
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
                self.registers.a = self.rrca(self.a);
                self.flags.zero = false;
                return 4;
            },
            0x10 => { // STOP
                return 4;
            },
            0x11 => { // LD DE, n16
                const word = self.memory.read_word();
                self.registers.set_de(word);
                return 12;
            },
            0x12 => { // LD DE, A
                self.memory.write_byte(self.registers.get_de(), self.registers.a);
                return 8;
            },
            0x13 => { // INC DE
                self.registers.set_de(@addWithOverflow(self.registers.get_de(), 1));
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
                jr(self.next_byte());
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
                self.registers.set_de(@subWithOverflow(de, 1));
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
            0x27 => { // DAA
                const tuple = self.daa();
                self.a = tuple[0];
                const flags = self.flags;
                flags.set(self.a == 0, flags.subtract, false, tuple[1]);
            },
            0x2F => { // CPL
                self.a = ~self.a;
                self.flags.subtract = true;
                self.flags.half_carry = true;
                return 4;
            },
            0x37 => { // SCF
                self.flags.set(self.flags.zero, false, false, true);
                return 4;
            },
            0x3F => { // CCF
                const flags = self.flags;
                flags.set(flags.zero, false, false, !flags.carry);
                return 4;
            },
            0x76 => { // HALT
                self.halted = true;
                return 4;
            },
            0xF3 => { // DI
                self.disable_interrupt = 2;
                return 4;
            },
            0xFB => { // EI
                self.enable_interrupt = 2;
                return 4;
            },
            else => std.debug.print("Opcode [{d}] is not implemented yet.", .{opcode}),
        }
        return 0;
    }
};
