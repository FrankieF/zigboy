const bit = @import("bit.zig");

pub const Mode = enum {
    HBlank,
    VBlank,
    OAMRead,
    VRAMRead,
};

pub const Stat = struct {
    lyc_interrupt: bool,
    oam_interrupt: bool,
    vblank_interrupt: bool,
    hblank_interrupt: bool,
    mode: Mode,

    pub fn init() Stat {
        return Stat{
            .lyc_interrupt = false,
            .oam_interrupt = false,
            .vblank_interrupt = false,
            .hblank_interrupt = false,
            .mode = Mode.HBlank,
        };
    }

    pub fn read_byte(self: *Stat, address: u16) u8 {
        if (address != 0xFF41) {
            unreachable;
        }
        var value: u8 = 0;
        if (self.lyc_interrupt) value = bit.set(value, 6);
        if (self.oam_interrupt) value = bit.set(value, 5);
        if (self.vblank_interrupt) value = bit.set(value, 4);
        if (self.hblank_interrupt) value = bit.set(value, 3);
        switch (self.mode) {
            Mode.VBlank => value = bit.set(value, 0),
            Mode.OAMRead => value = bit.set(value, 1),
            Mode.VRAMRead => value = bit.set(value, 2),
            else => {},
        }
        return value;
    }

    pub fn write_byte(self: *Stat, address: u16, value: u8) void {
        if (address != 0xFF41) {
            unreachable;
        }
        self.lyc_interrupt = bit.is_set(value, 6);
        self.oam_interrupt = bit.is_set(value, 5);
        self.vblank_interrupt = bit.is_set(value, 4);
        self.hblank_interrupt = bit.is_set(value, 3);
    }
};
