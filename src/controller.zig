const bit = @import("bit.zig");
const Interrupt = @import("interrupt.zig");
const sf = struct {
    const sfml = @import("sfml");
    usingnamespace sfml;
    usingnamespace sfml.window;
};

pub const KeyMapping = struct {
    sfKey: sf.keyboard.KeyCode,
    myKey: Key,
};

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
            .register = .{ 0xFF, 0xFF },
            .select = 0,
            .interrupt = interrupt,
        };
    }

    pub fn press(self: *KeyPad, key: Key) void {
        switch (key) {
            Key.Right => self.register[1] = bit.turn_off(self.register[1], 0),
            Key.Left => self.register[1] = bit.turn_off(self.register[1], 1),
            Key.Up => self.register[1] = bit.turn_off(self.register[1], 2),
            Key.Down => self.register[1] = bit.turn_off(self.register[1], 3),
            Key.A => self.register[0] = bit.turn_off(self.register[0], 0),
            Key.B => self.register[0] = bit.turn_off(self.register[0], 1),
            Key.Select => self.register[0] = bit.turn_off(self.register[0], 2),
            Key.Start => self.register[0] = bit.turn_off(self.register[0], 3),
        }
        self.interrupt.set_interrupt(Interrupt.InterruptSource.Keypad);
    }

    pub fn release(self: *KeyPad, key: Key) void {
        switch (key) {
            Key.Right => self.register[1] = bit.set(self.register[1], 0),
            Key.Left => self.register[1] = bit.set(self.register[1], 1),
            Key.Up => self.register[1] = bit.set(self.register[1], 2),
            Key.Down => self.register[1] = bit.set(self.register[1], 3),
            Key.A => self.register[0] = bit.set(self.register[0], 0),
            Key.B => self.register[0] = bit.set(self.register[0], 1),
            Key.Select => self.register[0] = bit.set(self.register[0], 2),
            Key.Start => self.register[0] = bit.set(self.register[0], 3),
        }
    }

    pub fn read_byte(self: *KeyPad, address: u16) u8 {
        if (address != 0xFF00) {
            unreachable;
        }
        var result: u8 = 0b11000000 | self.select;
        var low_key_bits: u8 = 0x0F;
        if (!bit.is_set(self.select, 4)) {
            low_key_bits &= self.register[1];
        }
        if (!bit.is_set(self.select, 5)) {
            low_key_bits &= self.register[0];
        }
        result |= low_key_bits;
        return result;
    }

    pub fn write_byte(self: *KeyPad, address: u16, value: u8) void {
        if (address != 0xFF00) {
            unreachable;
        }
        // The only write will switch which keys are read
        self.select = value & 0b0011_0000;
    }
};

const std = @import("std");

test "write byte low" {
    var interrupt = Interrupt.Interrupt.init();
    var keypad = KeyPad.init(&interrupt);
    const address: u16 = 0xFF00;
    keypad.write_byte(address, 0b00010000);
    try std.testing.expectEqual(0b00010000, keypad.select);
}

test "write byte high" {
    var interrupt = Interrupt.Interrupt.init();
    var keypad = KeyPad.init(&interrupt);
    const address: u16 = 0xFF00;
    keypad.write_byte(address, 0b00100000);
    try std.testing.expectEqual(0b00100000, keypad.select);
}

test "press start and read byte" {
    var interrupt = Interrupt.Interrupt.init();
    var keypad = KeyPad.init(&interrupt);
    const address: u16 = 0xFF00;
    keypad.press(Key.Start);
    // CPU writes 0x10 to $FF00 to select Direction keys (P4=0, P5=1)
    keypad.write_byte(address, 0x10);
    const value = keypad.read_byte(address);
    const expected: u8 = 0b11010111;
    try std.testing.expectEqual(expected, value);
}

test "press up and read byte" {
    var interrupt = Interrupt.Interrupt.init();
    var keypad = KeyPad.init(&interrupt);
    const address: u16 = 0xFF00;
    keypad.press(Key.Up);
    // CPU writes 0x20 to $FF00 to select Action keys (P5=0, P4=1)
    keypad.write_byte(address, 0x20);
    const value = keypad.read_byte(address);
    const expected: u8 = 0b11101011;
    try std.testing.expectEqual(expected, value);
}

test "press a" {
    try press_and_assert(Key.A, 0, 0b11111110);
}

test "press b" {
    try press_and_assert(Key.B, 0, 0b11111101);
}

test "press select" {
    try press_and_assert(Key.Select, 0, 0b11111011);
}

test "press start" {
    try press_and_assert(Key.Start, 0, 0b11110111);
}

test "press right" {
    try press_and_assert(Key.Right, 1, 0b11111110);
}

test "press left" {
    try press_and_assert(Key.Left, 1, 0b11111101);
}

test "press up" {
    try press_and_assert(Key.Up, 1, 0b11111011);
}

test "press down" {
    try press_and_assert(Key.Down, 1, 0b11110111);
}

test "release a" {
    try release_and_assert(Key.A, 0, 0b11110001);
}

test "release b" {
    try release_and_assert(Key.B, 0, 0b11110010);
}

test "release select" {
    try release_and_assert(Key.Select, 0, 0b11110100);
}

test "release start" {
    try release_and_assert(Key.Start, 0, 0b11111000);
}

test "release right" {
    try release_and_assert(Key.Right, 1, 0b11110001);
}

test "release left" {
    try release_and_assert(Key.Left, 1, 0b11110010);
}

test "release up" {
    try release_and_assert(Key.Up, 1, 0b11110100);
}

test "release down" {
    try release_and_assert(Key.Down, 1, 0b11111000);
}

fn press_and_assert(key: Key, register: usize, expected: u8) !void {
    var interrupt = Interrupt.Interrupt.init();
    var keypad = KeyPad.init(&interrupt);
    keypad.press(key);
    try std.testing.expect(keypad.register[register] == expected);
}

fn release_and_assert(key: Key, register: usize, expected: u8) !void {
    var interrupt = Interrupt.Interrupt.init();
    var keypad = KeyPad.init(&interrupt);
    for (std.enums.values(Key)) |k| {
        keypad.press(k);
    }
    keypad.release(key);
    try std.testing.expect(keypad.register[register] == expected);
}
