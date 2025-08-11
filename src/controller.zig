const bit = @import("bit.zig");
const Interrupt = @import("interrupt.zig");

pub const Key = enum {
    Right,
    Left,
    Up,
    Down,
    A,
    B,
    Select,
    Start,
};

pub const KeyPad = struct {
    register: [2]u8,
    select: u8,
    interrupt: *Interrupt.Interrupt,

    pub fn init(interrupt: *Interrupt.Interrupt) KeyPad {
        return KeyPad{
            .register = .{ 0xF, 0xF },
            .select = 0,
            .interrupt = interrupt,
        };
    }

    pub fn press(self: *KeyPad, key: Key) void {
        switch (key) {
            Key.Right => self.register[1] &= 0b1110,
            Key.Left => self.register[1] &= 0b1101,
            Key.Up => self.register[1] &= 0b1011,
            Key.Down => self.register[1] &= 0b0111,
            Key.A => self.register[1] &= 0b1110,
            Key.B => self.register[1] &= 0b1101,
            Key.Select => self.register[1] &= 0b1011,
            Key.Start => self.register[1] &= 0b0111,
        }
        self.interrupt.set_interrupt(Interrupt.InterruptSource.Keypad);
    }

    pub fn read_byte(self: *KeyPad, address: u16) u8 {
        if (address != 0xFF00) {
            unreachable;
        }
        if (bit.is_set(self.select, 4)) {
            return self.register[0];
        } else if (bit.is_set(self.select, 5)) {
            return self.register[1];
        }
        if (self.select != 0) {
            unreachable;
        }
        return 0;
    }

    pub fn write_byte(self: *KeyPad, address: u16, value: u8) void {
        if (address != 0xFF00) {
            unreachable;
        }
        // The only write will switch which keys are read
        self.select = value & 0b0011_0000;
    }
};
