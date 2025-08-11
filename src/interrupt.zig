pub const InterruptSource = enum(u8) {
    VBLank = 0b00000001,
    STAT = 0b00000010,
    Timer = 0b00000100,
    Serial = 0b00001000,
    Keypad = 0b00010000,
};

pub const Interrupt = struct {
    value: u8,

    pub fn init() Interrupt {
        return Interrupt{
            .value = 0,
        };
    }

    pub fn set_interrupt(self: *Interrupt, source: InterruptSource) void {
        self.value |= @intFromEnum(source);
    }

    pub fn read_byte(self: *Interrupt, address: u16) u8 {
        if (address != 0xFF0F) {
            unreachable;
        }
        return self.value;
    }

    pub fn write_byte(self: *Interrupt, address: u16, value: u8) void {
        if (address != 0xFF0F) {
            unreachable;
        }
        self.value = value;
    }
};
