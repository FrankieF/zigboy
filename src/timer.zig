const Clock = @import("clock.zig").Clock;
const Interrupt = @import("interrupt.zig").Interrupt;
const InterruptSource = @import("interrupt.zig").InterruptSource;
const bits = @import("bit.zig");

pub const Timer = struct {
    divider: u8,
    counter: u8,
    modulo: u8,
    enable: bool,
    divide_clock: Clock,
    modulo_clock: Clock,
    interrupt: *Interrupt,

    pub fn init(interrupt: *Interrupt) Timer {
        return Timer{
            .divide_clock = Clock.init(256),
            .modulo_clock = Clock.init(1024),
            .divider = 0,
            .counter = 0,
            .modulo = 0,
            .enable = false,
            .interrupt = interrupt,
        };
    }

    pub fn update(self: *Timer, cycles: u32) void {
        const ticks: u8 = @truncate(self.divide_clock.tick(cycles));
        self.divider = self.divider +% ticks;
        if (!self.enable) {
            return;
        }
        for (0..self.modulo_clock.tick(cycles)) |_| {
            self.counter = self.counter +% 1;
            if (self.counter == 0) {
                self.counter = self.modulo;
                self.interrupt.set_interrupt(InterruptSource.Timer);
            }
        }
    }

    pub fn read_byte(self: *Timer, address: u16) u8 {
        return switch (address) {
            0xFF04 => self.divider,
            0xFF05 => self.counter,
            0xFF06 => self.modulo,
            0xFF07 => {
                var byte: u8 = 0;
                if (self.enable) {
                    byte = bits.set(byte, 2);
                }
                switch (self.modulo_clock.period) {
                    1024 => byte = bits.set(byte, 0),
                    16 => byte = bits.set(byte, 1),
                    64 => byte = bits.set(byte, 2),
                    256 => byte = bits.set(byte, 3),
                    else => undefined,
                }
                return byte;
            },
            else => undefined,
        };
    }

    pub fn write_byte(self: *Timer, address: u16, value: u8) void {
        switch (address) {
            0xFF04 => self.divider = value,
            0xFF05 => self.counter = value,
            0xFF06 => self.modulo = value,
            0xFF07 => {
                self.enable = bits.is_set(value, 2);
                self.modulo_clock.period = switch (value & 0b11) {
                    0 => 1024,
                    1 => 16,
                    2 => 64,
                    3 => 256,
                    else => undefined,
                };
            },
            else => undefined,
        }
    }
};
