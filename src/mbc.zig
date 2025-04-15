const std = @import("std");
pub const Rom = struct {
    data: []const u8,

    pub fn init(data: []const u8) Rom {
        return Rom{ .data = data };
    }

    pub fn read_byte(self: *Rom, address: u16) u8 {
        return self.data[address];
    }

    pub fn write_byte(_: *Rom, _: u16, _: u8) void {}

    pub fn size(self: *Rom) usize {
        return self.data.len;
    }

    pub fn save(_: *Rom) void {}
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

    pub fn read_byte(self: *MBC1, address: u16) u8 {
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

    pub fn size(self: *MBC1) usize {
        return self.rom.len;
    }

    pub fn save(_: *MBC1) void {}
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

    pub fn read_byte(self: *MBC2, address: u16) u8 {
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

    pub fn size(self: *MBC2) usize {
        return self.rom.len;
    }

    pub fn save(_: *MBC2) void {}
};
