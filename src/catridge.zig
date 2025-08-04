const std = @import("std");
const MBC = @import("mbc.zig");

pub const CatridgeError = error{
    FileNotFound,
    IncorrectSize,
    IncorrectChecksum,
    IncorrectLogo,
};

const CatridgeType = enum {
    CartridgeNoMBC,
    CartridgeMBC1,
    CartridgeMBC2,
    CartridgeMBC3,
    CartridgeMBC5,
    CartridgeMBC1Multi,
    CartridgeNotSupported,
};

const CHECKSUM_START = 0x0134;
const CHECKSUM_END = 0x014D;
const NINTENDO_LOGO_START: u16 = 0x0104;
const NINTENDO_LOGO = [_]u8{
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11,
    0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E,
    0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
};

pub const Catridge = struct {
    title: [16]u8,
    cbg_flag: bool,
    sbg_flag: bool,
    catridge_type: CatridgeType,
    size: i8,
    mbc: MBC.MBC,

    pub fn init(path: []const u8, allocator: std.mem.Allocator) CatridgeError!Catridge {
        const catridge = try create(path, allocator);
        try catridge.verify_nintendo_logo();
        try catridge.verify_checksum();
        return catridge;
    }

    pub fn read_byte(self: Catridge, address: u16) u8 {
        return self.mbc.read_byte(address);
    }

    pub fn write_byte(self: Catridge, address: u16, value: u8) void {
        _ = self;
        _ = address;
        _ = value;
    }

    fn create(path: []const u8, allocator: std.mem.Allocator) CatridgeError!Catridge {
        std.fs.accessAbsolute(path, .{}) catch {
            return CatridgeError.FileNotFound;
        };
        const file = std.fs.openFileAbsolute(path, .{}) catch unreachable;
        defer file.close();
        const file_size: u64 = file.getEndPos() catch unreachable;
        if (file_size < 0x0150) {
            return CatridgeError.IncorrectSize;
        }
        const buffer: []u8 = allocator.alloc(u8, file_size) catch unreachable;
        //defer allocator.free(buffer);
        _ = file.readAll(buffer) catch unreachable;
        const cgb_flag = buffer[0x0143];
        const size = buffer[0x0149];
        const catridge_type = get_catridge_type(buffer[0x147]);
        const mbc = create_mbc(buffer, size, catridge_type, allocator);
        return Catridge{
            .catridge_type = catridge_type,
            .title = buffer[0x0134..0x0144].*,
            .cbg_flag = cgb_flag == 0x80 or cgb_flag == 0xC0,
            .sbg_flag = buffer[0x0146] == 0x03,
            .size = @truncate(@as(i16, size)),
            .mbc = mbc,
        };
    }

    fn get_catridge_type(value: u8) CatridgeType {
        switch (value) {
            0x00, 0x08, 0x09 => return CatridgeType.CartridgeNoMBC,
            0x01, 0x02, 0x03, 0xEA, 0xFF => return CatridgeType.CartridgeMBC1,
            0x05, 0x06 => return CatridgeType.CartridgeMBC2,
            0x0F, 0x10, 0x11, 0x12, 0x13, 0xFC => return CatridgeType.CartridgeMBC3,
            0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E => return CatridgeType.CartridgeMBC5,
            else => {
                std.debug.print("Unsupported catridge type.", .{});
                return CatridgeType.CartridgeNotSupported;
            },
        }
    }

    fn create_mbc(rom: []const u8, ram_size: usize, catridge_type: CatridgeType, allocator: std.mem.Allocator) MBC.MBC {
        switch (catridge_type) {
            CatridgeType.CartridgeNoMBC => {
                return .{ .rom = MBC.Rom.init(rom) };
            },
            CatridgeType.CartridgeMBC1, CatridgeType.CartridgeMBC1Multi => {
                return .{ .mbc1 = MBC.MBC1.init(rom, ram_size, allocator) };
            },
            CatridgeType.CartridgeMBC2 => {
                return .{ .mbc2 = MBC.MBC2.init(rom, ram_size, allocator) };
            },
            CatridgeType.CartridgeMBC3 => {
                return .{ .mbc3 = MBC.MBC3.init(rom, ram_size, null, allocator) };
            },
            CatridgeType.CartridgeMBC5 => {
                return .{ .mbc5 = MBC.MBC5.init(rom, ram_size, allocator) };
            },
            else => {
                std.debug.print("Unsupported catridge type.", .{});
                return undefined;
            },
        }
    }

    fn verify_nintendo_logo(self: Catridge) CatridgeError!void {
        for (0..48) |i| {
            const index: u16 = @intCast(i);
            const byte = self.mbc.read_byte(index + NINTENDO_LOGO_START);
            if (byte != NINTENDO_LOGO[i]) {
                return CatridgeError.IncorrectLogo;
            }
        }
    }

    fn verify_checksum(self: Catridge) CatridgeError!void {
        var checksum: u8 = 0;
        for (CHECKSUM_START..CHECKSUM_END) |i| {
            const index: u16 = @intCast(i);
            checksum = @subWithOverflow(checksum, self.mbc.read_byte(index))[0];
            checksum = @subWithOverflow(checksum, 1)[0];
        }
        if (checksum != self.mbc.read_byte(CHECKSUM_END)) {
            return CatridgeError.IncorrectChecksum;
        }
    }
};
