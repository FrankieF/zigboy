const std = @import("std");

pub const CatridgeError = error{
    FileNotFound,
    IncorrectSize,
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

pub const Catridge = struct {
    title: [16]u8,
    cbg_flag: bool,
    sbg_flag: bool,
    catridge_type: CatridgeType,
    size: i8,

    pub fn init(path: []const u8, allocator: std.mem.Allocator) CatridgeError!Catridge {
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
        defer allocator.free(buffer);
        _ = file.readAll(buffer) catch unreachable;
        const cgb_flag = buffer[0x0143];
        const size = buffer[0x0148];
        return Catridge{
            .catridge_type = get_catridge_type(buffer[0x147]),
            .title = buffer[0x0134..0x0144].*,
            .cbg_flag = cgb_flag == 0x80 or cgb_flag == 0xC0,
            .sbg_flag = buffer[0x0146] == 0x03,
            .size = @truncate(@as(i16, size)),
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
};
