const HIGH_RAM_LENGTH = 127;
const WORK_RAM_LENGTH = 8192;

pub const Memory = struct {
    high_ram: [HIGH_RAM_LENGTH]u8,
    work_ram: [WORK_RAM_LENGTH]u8, // 2 4kb work ram

    pub fn init() Memory {
        const high_ram = [_]u8{0} ** HIGH_RAM_LENGTH;
        const work_ram = [_]u8{0} ** WORK_RAM_LENGTH;
        return Memory{
            .high_ram = high_ram,
            .work_ram = work_ram,
        };
    }

    pub fn read_byte(self: *Memory, address: u16) u8 {
        switch (address) {
            0xC000...0xDFFF => { // work ram 8 kb
                return self.work_ram[@as(u8, address - 0xC000)];
            },
        }
        std.debug.print("Memory address[{d}] is not implemented yet.", .{address});
        return 0;
    }

    pub fn read_word(self: *Memory, address: u16) u16 {
        return @as(u16, self.read_byte(address)) | (@as(u16, self.read_byte(address + 1)) << 8);
    }

    pub fn write_byte(self: *Memory, address: u16, value: u8) void {
        switch (address) {
            0xC000...0xDFFF => { // work ram 8 kb
                self.work_ram[@as(u8, address - 0xC000)] = value;
            },
        }
        std.debug.print("Memory address[{d}] is not implemented yet.", .{address});
    }

    pub fn write_word(self: *Memory, address: u16, value: u16) void {
        self.write_byte(address, @as(u8, value & 0xFF));
        self.write_byte(address, @as(u8, value >> 8));
    }
};

const std = @import("std");
