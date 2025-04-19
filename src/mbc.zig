const std = @import("std");
const RTC = @import("real_time_clock.zig");

pub const MBC = union(enum) {
    rom: Rom,
    mbc1: MBC1,
    mbc2: MBC2,
    mbc3: MBC3,
    mbc5: MBC5,

    const Self = @This();

    pub fn read_byte(self: *const Self, address: u16) u8 {
        switch (self.*) {
            inline else => |*mbc| return mbc.read_byte(address),
        }
    }

    pub fn write_byte(self: *Self, address: u16, value: u8) void {
        switch (self.*) {
            inline else => |*mbc| mbc.write_byte(address, value),
        }
    }

    pub fn size(self: *const Self) usize {
        switch (self.*) {
            inline else => |mbc| return mbc.size(),
        }
    }

    pub fn save(self: *const MBC) void {
        switch (self.*) {
            inline else => |mbc| mbc.save(),
        }
    }
};

pub const Rom = struct {
    data: []const u8,

    pub fn init(data: []const u8) Rom {
        return Rom{ .data = data };
    }

    pub fn read_byte(self: Rom, address: u16) u8 {
        return self.data[address];
    }

    pub fn write_byte(_: *const Rom, _: u16, _: u8) void {}

    pub fn size(self: Rom) usize {
        return self.data.len;
    }

    pub fn save(_: Rom) void {}
};

pub const MBC1 = struct {
    rom: []const u8,
    rom_bank: u8 = 1,
    ram_enabled: bool = false,
    ram: []u8,
    default_mode: bool = true,

    pub fn init(rom: []const u8, ram_size: usize, allocator: std.mem.Allocator) MBC1 {
        return MBC1{
            .rom = rom,
            .ram = allocator.alloc(u8, ram_size) catch undefined,
        };
    }

    pub fn read_byte(self: MBC1, address: u16) u8 {
        switch (address) {
            0x0000...0x3FFF => return self.rom[address],
            0x4000...0x7FFF => {
                const offset = 0x4000 * @as(u16, self.rom_bank) * 8192;
                return self.rom[offset];
            },
            0xA000...0xBFFF => {
                if (!self.ram_enabled) {
                    return 0;
                }
                const offset = @as(u16, self.rom_bank) * 8192;
                const index = offset + address - 0xA000;
                return self.rom[index];
            },
            else => return 0,
        }
    }

    pub fn write_byte(self: *MBC1, address: u16, value: u8) void {
        switch (address) {
            0x000...0x1FFF => self.ram_enabled = value & 0x0F == 0x0A,
            0x2000...0x3FFF => {
                const n = if (value == 0) 1 else value;
                self.rom_bank = self.rom_bank & 0b01100000 | n & 0b00011111;
            },
            0x4000...0x5FFF => {
                if (self.default_mode) {
                    self.rom_bank = value & 0b11;
                } else {
                    self.rom_bank = self.rom_bank & 0b00011111 | (value & 0b11) << 5;
                }
            },
            else => {},
        }
    }

    pub fn size(self: MBC1) usize {
        return self.rom.len;
    }

    pub fn save(_: MBC1) void {}
};

pub const MBC2 = struct {
    rom: []const u8,
    rom_bank: usize = 1,
    ram_enabled: bool = false,
    ram: []u8,

    pub fn init(rom: []const u8, ram_size: usize, allocator: std.mem.Allocator) MBC2 {
        return MBC2{
            .rom = rom,
            .ram = allocator.alloc(u8, ram_size) catch undefined,
        };
    }

    pub fn read_byte(self: MBC2, address: u16) u8 {
        switch (address) {
            0x0000...0x3FFF => return self.rom[address],
            0x4000...0x7FFF => {
                const start = 0x4000 * self.rom_bank;
                const offset = start + address - 0x4000;
                return self.rom[offset];
            },
            0xA000...0xA1FF => {
                if (!self.ram_enabled) {
                    return 0;
                }
                const offset = address - 0xA000;
                return self.rom[offset];
            },
            else => return 0,
        }
    }

    pub fn write_byte(self: *MBC2, address: u16, value: u8) void {
        const byte = value & 0xF;
        switch (address) {
            0x000...0x1FFF => {
                if (address & 0x0100 == 0) {
                    self.ram_enabled = byte == 0x0A;
                }
            },
            0x2000...0x3FFF => {
                if (address & 0x0100 != 0) {
                    self.rom_bank = byte;
                }
            },
            0xA000...0xA1FF => {
                if (self.ram_enabled) {
                    self.ram[address - 0xA000] = byte;
                }
            },
            else => {},
        }
    }

    pub fn size(self: MBC2) usize {
        return self.rom.len;
    }

    pub fn save(_: MBC2) void {}
};

pub const MBC3 = struct {
    rom: []const u8,
    rom_bank: usize = 0,
    ram_enabled: bool = false,
    ram: []u8,
    ram_bank: usize = 1,
    rtc: ?RTC.RealTimeClock,

    pub fn init(rom: []const u8, ram_size: usize, rtc_path: ?[]const u8, allocator: std.mem.Allocator) MBC3 {
        return MBC3{
            .rom = rom,
            .ram = allocator.alloc(u8, ram_size) catch undefined,
            .rtc = RTC.RealTimeClock.init(rtc_path),
        };
    }

    pub fn read_byte(self: MBC3, address: u16) u8 {
        switch (address) {
            0x0000...0x3FFF => return self.rom[address],
            0x4000...0x7FFF => {
                const start = 0x4000 * self.rom_bank;
                const offset = start + address - 0x4000;
                return self.rom[offset];
            },
            0xA000...0xA1FF => {
                if (!self.ram_enabled) {
                    return 0;
                }
                if (self.ram_bank <= 3) {
                    const start = self.ram_bank * 0x2000;
                    const offset = start + address - 0xA000;
                    return self.ram[offset];
                }
                if (self.rtc) |*rtc| {
                    return rtc.read_byte(@intCast(self.ram_bank));
                }
                return 0;
            },
            else => return 0,
        }
    }

    pub fn write_byte(self: *MBC3, address: u16, value: u8) void {
        switch (address) {
            0x000...0x1FFF => {
                self.ram_enabled = value & 0x0F == 0x0A;
            },
            0x2000...0x3FFF => {
                var val = value & 0b01111111;
                val = if (val == 0) 1 else val;
                self.rom_bank = val;
            },
            0x4000...0x5FFF => {
                self.ram_bank = value & 0x0F;
            },
            0x6000...0x7FFF => {
                if (value & 1 != 0) {
                    if (self.rtc) |*rtc| {
                        rtc.step();
                    }
                }
            },
            0xA000...0xBFFF => {
                if (!self.ram_enabled) {
                    return;
                }
                if (self.ram_bank <= 3) {
                    const start = 0x2000 * self.ram_bank;
                    const offset = start + address - 0xA000;
                    self.ram[offset] = value;
                } else if (self.rtc) |*rtc| {
                    rtc.write_byte(address, value);
                }
            },
            else => {},
        }
    }

    pub fn size(self: MBC3) usize {
        return self.rom.len;
    }

    pub fn save(_: MBC3) void {}
};

pub const MBC5 = struct {
    rom: []const u8,
    rom_bank: usize = 0,
    ram_enabled: bool = false,
    ram: []u8,
    ram_bank: usize = 0,

    pub fn init(rom: []const u8, ram_size: usize, allocator: std.mem.Allocator) MBC5 {
        return MBC5{
            .rom = rom,
            .ram = allocator.alloc(u8, ram_size) catch undefined,
        };
    }

    pub fn read_byte(self: MBC5, address: u16) u8 {
        switch (address) {
            0x0000...0x3FFF => return self.rom[address],
            0x4000...0x7FFF => {
                const start = 0x4000 * self.rom_bank;
                const offset = start + address - 0x4000;
                return self.rom[offset];
            },
            0xA000...0xBFFF => {
                if (!self.ram_enabled) {
                    return 0;
                }
                const start = 0x2000 * self.ram_bank;
                const offset = start + address - 0xA000;
                return self.rom[offset];
            },
            else => return 0,
        }
    }

    pub fn write_byte(self: *MBC5, address: u16, value: u8) void {
        switch (address) {
            0x000...0x1FFF => self.ram_enabled = value * 0x0F == 0x0A,
            0x2000...0x2FFF => self.rom_bank = (self.rom_bank & 0x100) | value,
            0x3000...0x3FFF => self.rom_bank = (self.rom_bank & 0xFF) | (@as(u16, value) << 8),
            0x4000...0x5FFF => self.rom_bank = value & 0xF,
            0xA000...0xBFFF => {
                if (self.ram_enabled) {
                    const start = 0x2000 * self.ram_bank;
                    const offset = start + address - 0xA000;
                    self.ram[offset] = value;
                }
            },
            else => {},
        }
    }

    pub fn size(self: MBC5) usize {
        return self.rom.len;
    }

    pub fn save(_: MBC5) void {}
};
