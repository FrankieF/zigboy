const std = @import("std");
const MBC = @import("mbc.zig");

pub const CartridgeError = error{
    FileNotFound,
    IncorrectSize,
    IncorrectChecksum,
    IncorrectLogo,
};

const CartridgeType = enum {
    CartridgeNoMBC,
    CartridgeMBC1,
    CartridgeMBC2,
    CartridgeMBC3,
    CartridgeMBC5,
    CartridgeMBC1Multi,
    CartridgeNotSupported,
};

const KILOBYTE: u32 = 1024;
const CHECKSUM_START = 0x0134;
const CHECKSUM_END = 0x014D;
const NINTENDO_LOGO_START: u16 = 0x0104;
const NINTENDO_LOGO = [_]u8{
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11,
    0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E,
    0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
};

pub const Cartridge = struct {
    title: [16]u8,
    cbg_flag: bool,
    sbg_flag: bool,
    cartridge_type: CartridgeType,
    size: i32,
    mbc: MBC.MBC,

    pub fn init_for_testing() Cartridge {
        const kb_64 = 64 * KILOBYTE;
        const rom: [kb_64]u8 = [_]u8{0} ** kb_64;
        const title = "test cartidgerom";
        return Cartridge{
            .title = title.*,
            .cbg_flag = false,
            .sbg_flag = false,
            .cartridge_type = CartridgeType.CartridgeNoMBC,
            .size = 100,
            .mbc = .{ .rom = MBC.Rom.init(&rom) },
        };
    }

    pub fn init(path: []const u8, allocator: std.mem.Allocator) CartridgeError!Cartridge {
        const cartridge = try create(path, allocator);
        try cartridge.verify_nintendo_logo();
        try cartridge.verify_checksum();
        return cartridge;
    }

    pub fn read_byte(self: Cartridge, address: u16) u8 {
        return self.mbc.read_byte(address);
    }

    pub fn write_byte(self: *Cartridge, address: u16, value: u8) void {
        self.mbc.write_byte(address, value);
    }

    fn create(path: []const u8, allocator: std.mem.Allocator) CartridgeError!Cartridge {
        std.fs.accessAbsolute(path, .{}) catch {
            return CartridgeError.FileNotFound;
        };
        const file = std.fs.openFileAbsolute(path, .{}) catch unreachable;
        defer file.close();
        const file_size: u64 = file.getEndPos() catch unreachable;
        if (file_size < 0x0150) {
            return CartridgeError.IncorrectSize;
        }
        const buffer: []u8 = allocator.alloc(u8, file_size) catch unreachable;
        _ = file.readAll(buffer) catch unreachable;
        const cgb_flag = buffer[0x0143];
        const rom_size = get_rom_size(buffer[0x0148]);
        const ram_size = get_ram_size(buffer[0x0149]);
        const cartridge_type = get_cartridge_type(buffer[0x0147]);
        const mbc = create_mbc(buffer, ram_size, cartridge_type, allocator);
        return Cartridge{
            .cartridge_type = cartridge_type,
            .title = buffer[0x0134..0x0144].*,
            .cbg_flag = cgb_flag == 0x80 or cgb_flag == 0xC0,
            .sbg_flag = buffer[0x0146] == 0x03,
            .size = @intCast(rom_size),
            .mbc = mbc,
        };
    }

    fn get_rom_size(value: u8) usize {
        return switch (value) {
            0 => 32 * KILOBYTE,
            1 => 64 * KILOBYTE,
            2 => 128 * KILOBYTE,
            3 => 256 * KILOBYTE,
            4 => 512 * KILOBYTE,
            5 => 1024 * KILOBYTE,
            6 => 2048 * KILOBYTE,
            7 => 4096 * KILOBYTE,
            8 => 8092 * KILOBYTE,
            0x52 => 1152 * KILOBYTE,
            0x53 => 1280 * KILOBYTE,
            0x54 => 1536 * KILOBYTE,
            else => 0,
        };
    }

    fn get_ram_size(value: u8) usize {
        return switch (value) {
            0 => 0,
            1 => 2 * KILOBYTE,
            2 => 8 * KILOBYTE,
            3 => 32 * KILOBYTE,
            4 => 128 * KILOBYTE,
            5 => 64 * KILOBYTE,
            else => 0,
        };
    }

    fn get_cartridge_type(value: u8) CartridgeType {
        switch (value) {
            0x00, 0x08, 0x09 => return CartridgeType.CartridgeNoMBC,
            0x01, 0x02, 0x03, 0xEA, 0xFF => return CartridgeType.CartridgeMBC1,
            0x05, 0x06 => return CartridgeType.CartridgeMBC2,
            0x0F, 0x10, 0x11, 0x12, 0x13, 0xFC => return CartridgeType.CartridgeMBC3,
            0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E => return CartridgeType.CartridgeMBC5,
            else => {
                std.debug.print("Unsupported cartridge type.", .{});
                return CartridgeType.CartridgeNotSupported;
            },
        }
    }

    fn create_mbc(rom: []const u8, ram_size: usize, cartridge_type: CartridgeType, allocator: std.mem.Allocator) MBC.MBC {
        switch (cartridge_type) {
            CartridgeType.CartridgeNoMBC => {
                return .{ .rom = MBC.Rom.init(rom) };
            },
            CartridgeType.CartridgeMBC1, CartridgeType.CartridgeMBC1Multi => {
                return .{ .mbc1 = MBC.MBC1.init(rom, ram_size, allocator) };
            },
            CartridgeType.CartridgeMBC2 => {
                return .{ .mbc2 = MBC.MBC2.init(rom, ram_size, allocator) };
            },
            CartridgeType.CartridgeMBC3 => {
                return .{ .mbc3 = MBC.MBC3.init(rom, ram_size, null, allocator) };
            },
            CartridgeType.CartridgeMBC5 => {
                return .{ .mbc5 = MBC.MBC5.init(rom, ram_size, allocator) };
            },
            else => {
                std.debug.print("Unsupported cartridge type.", .{});
                return undefined;
            },
        }
    }

    fn verify_nintendo_logo(self: Cartridge) CartridgeError!void {
        for (0..48) |i| {
            const index: u16 = @intCast(i);
            const byte = self.mbc.read_byte(index + NINTENDO_LOGO_START);
            if (byte != NINTENDO_LOGO[i]) {
                return CartridgeError.IncorrectLogo;
            }
        }
    }

    fn verify_checksum(self: Cartridge) CartridgeError!void {
        var checksum: u8 = 0;
        for (CHECKSUM_START..CHECKSUM_END) |i| {
            const index: u16 = @intCast(i);
            checksum = @subWithOverflow(checksum, self.mbc.read_byte(index))[0];
            checksum = @subWithOverflow(checksum, 1)[0];
        }
        if (checksum != self.mbc.read_byte(CHECKSUM_END)) {
            return CartridgeError.IncorrectChecksum;
        }
    }
};
