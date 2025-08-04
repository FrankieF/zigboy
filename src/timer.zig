const Clock = @import("clock.zig").Clock;
const Interrupt = @import("interrupt.zig").Interrupt;

pub const Timer = struct {
    divider: u8,
    counter: u8,
    modulo: u8,
    enable: bool,
    divide_clock: Clock,
    modulo_clock: Clock,
    interrupt: Interrupt,

    pub fn init(interrupt: Interrupt) Timer {
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
        self.divider = @truncate(self.divider +% self.divide_clock.tick(cycles));
        if (!self.enable) {
            return;
        }
        for (0..self.modulo_clock.tick(cycles)) |_| {
            self.counter = self.counter +% 1;
            if (self.counter == 0) {
                self.counter = self.modulo;
                // update interrupt here
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
                    byte &= 0x2;
                }
                switch (self.modulo_clock.period) {
                    1024 => byte &= 0x1,
                    16 => byte &= 0x2,
                    64 => byte &= 0x4,
                    256 => byte &= 0x8,
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
                self.enable = (0x4 & value) != 0;
                var byte: u8 = 0;
                if (self.enable) {
                    byte &= 0x2;
                }
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
