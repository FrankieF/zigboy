const bit = @import("bit.zig");

pub const LCD = struct {
    lcd_enable: bool,
    window_tilemap: u16,
    window_enable: bool,
    background_window_tilemap: u16,
    background_window_enable: bool,
    background_tilemap: u16,
    sprite_size: u8,
    sprite_enable: bool,

    pub fn init() LCD {
        return LCD{
            .lcd_enable = false,
            .window_tilemap = 0x9C00,
            .window_enable = false,
            .background_window_tilemap = 0x8800,
            .background_tilemap = 0x9C00,
            .sprite_size = 8,
            .sprite_enable = false,
            .background_window_enable = false,
        };
    }

    pub fn read_byte(self: *LCD, address: u16) u8 {
        if (address != 0xFF40) {
            unreachable;
        }
        var value: u8 = 0;
        if (self.lcd_enable) value = bit.set(value, 7);
        if (self.window_tilemap == 0x9C00) value = bit.set(value, 6);
        if (self.window_enable) value = bit.set(value, 5);
        if (self.background_window_tilemap == 0x8000) value = bit.set(value, 4);
        if (self.background_tilemap == 0x9C00) value = bit.set(value, 3);
        if (self.sprite_size == 16) value = bit.set(value, 2);
        if (self.sprite_enable) value = bit.set(value, 1);
        if (self.background_window_enable) value = bit.set(value, 0);
        return value;
    }

    pub fn write_bye(self: *LCD, address: u16, value: u8) void {
        if (address != 0xFF40) {
            unreachable;
        }
        self.lcd_enable = bit.is_set(value, 7);
        self.window_tilemap = if (bit.is_set(value, 6)) 0x9C00 else 0x9800;
        self.window_enable = bit.is_set(value, 5);
        self.background_window_tilemap = if (bit.is_set(value, 4)) 0x8000 else 0x8800;
        self.background_tilemap = if (bit.is_set(value, 3)) 0x9C00 else 0x9800;
        self.sprite_size = if (bit.is_set(value, 2)) 16 else 8;
        self.sprite_enable = bit.is_set(value, 1);
        self.background_window_enable = bit.is_set(value, 0);
    }
};
