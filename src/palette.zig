pub const Palette = struct {
    data: u8,
    shades: [4]u32,
    colors: [4]u32,

    pub fn init() Palette {
        return Palette{
            .data = 0,
            .shades = [_]u32{0} ** 4,
            .colors = .{ 0xE0F8D0, 0x88C070, 0x346856, 0x81820 },
        };
    }

    pub fn get_shade(self: *Palette, index: u3) u32 {
        const value = self.data >> (2 * index) & 3;
        return switch (value) {
            0b00 => self.colors[0],
            0b01 => self.colors[1],
            0b10 => self.colors[2],
            0b11 => self.colors[3],
            else => unreachable,
        };
    }

    pub fn set_colors(self: *Palette, colors: [4]u32) void {
        self.colors = colors;
    }

    pub fn update(self: *Palette) void {
        for (0..4) |i| {
            const index: u3 = @truncate(i);
            self.shades[i] = self.get_shade(index);
        }
    }

    pub fn read_byte(self: *Palette) u8 {
        return self.data;
    }

    pub fn write_byte(self: *Palette, value: u8) void {
        self.data = value;
        self.update();
    }
};
