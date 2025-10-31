pub const Palette = struct {
    data: u8,
    shades: [4]u32,
    colors: [4]u32,

    pub fn init() Palette {
        return Palette{
            .data = 0,
            .shades = [_]u32{0} ** 4,
            .colors = .{ 255, 170, 85, 0 },
        };
    }

    pub fn get_shade(self: *Palette, color_id: u3) u32 {
        return self.colors[color_id];
    }

    pub fn set_colors(self: *Palette, colors: [4]u32) void {
        self.colors = colors;
        self.update();
    }

    pub fn update(self: *Palette) void {
        for (0..4) |i| {
            const index: u3 = @intCast(i);
            const shift = self.data >> (2 * index);
            const color_index = shift & 0b11;
            self.shades[i] = self.get_shade(@intCast(color_index));
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

const expect = @import("std").testing.expect;

test "palette" {
    var palette = Palette.init();
    palette.write_byte(0b1011_0010);
    palette.update();
    try expect(palette.shades[0] == 0x346856);
    try expect(palette.shades[1] == 0xE0F8D0);
    try expect(palette.shades[2] == 0x081820);
    try expect(palette.shades[3] == 0x346856);
}
