const Interrupt = @import("interrupt.zig");

pub const Serial = struct {
    data: u8,
    control: u8,
    callback: ?*const fn (u8) void,
    interrupt: *Interrupt.Interrupt,

    pub fn init(interrupt: *Interrupt.Interrupt, callback: ?*const fn (u8) void) Serial {
        return Serial{
            .data = 0,
            .control = 0,
            .callback = callback,
            .interrupt = interrupt,
        };
    }

    pub fn read_byte(self: *Serial, address: u16) u8 {
        return switch (address) {
            0xFF01 => self.data,
            0xFF02 => self.control,
            else => unreachable,
        };
    }

    pub fn write_byte(self: *Serial, address: u16, value: u8) void {
        switch (address) {
            0xFF01 => self.data = value,
            0xFF02 => {
                self.control = value;
                if (self.control == 0x81) {
                    if (self.callback) |callback| {
                        callback(self.data);
                        self.data = value;
                        self.interrupt.set_interrupt(Interrupt.InterruptSource.Serial);
                    }
                }
            },
            else => unreachable,
        }
    }
};
