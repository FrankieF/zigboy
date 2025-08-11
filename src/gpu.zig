const interrupt_file = @import("interrupt.zig");
const Interrupt = interrupt_file.Interrupt;
const InterruptSource = interrupt_file.InterruptSource;
const LCD = @import("lcd_control.zig").LCD;
const stat_file = @import("stat.zig");
const Stat = stat_file.Stat;
const StatMode = stat_file.Mode;
const Palette = @import("palette.zig").Palette;
const bit = @import("bit.zig");
const std = @import("std");

pub const Sprite = struct {
    y: i16,
    x: i16,
    tile_number: u8,
    below_background: bool,
    y_flip: bool,
    x_flip: bool,
    is_palette_1: bool,
};

const VRAM_SIZE = 16384;
const OAM_SIZE = 160;
const SCREEN_HEIGHT: u32 = 144;
const SCREEN_WIDTH: u32 = 160;
const LINE_WIDTH: u16 = 456;
const FRAME_LINES: u8 = 154;

pub const Prioriy = enum { Color0, None };

pub const GPU = struct {
    vram: [VRAM_SIZE]u8,
    oam: [OAM_SIZE]u8,
    pixels: [SCREEN_HEIGHT * SCREEN_WIDTH]u32,
    updated: bool,
    lcd: LCD,
    stat: Stat,
    h_blank: bool,
    scroll_y: u8,
    scroll_x: u8,
    line_y: u8,
    line_y_compare: u8,
    window_y: u8,
    window_x: u8,
    background_palette: Palette,
    sprite_patlette_0: Palette,
    sprite_patlette_1: Palette,
    background_priority: [SCREEN_WIDTH]Prioriy,
    dots: u32,
    interrupt: Interrupt,

    pub fn init(interrupt: Interrupt) GPU {
        return GPU{
            .vram = [_]u8{0} ** VRAM_SIZE,
            .oam = [_]u8{0} ** OAM_SIZE,
            .lcd = LCD.init(),
            .stat = Stat.init(),
            .h_blank = false,
            .scroll_y = 0,
            .scroll_x = 0,
            .line_y = 0,
            .line_y_compare = 0,
            .window_y = 0,
            .window_x = 0,
            .background_palette = Palette.init(),
            .sprite_patlette_0 = Palette.init(),
            .sprite_patlette_1 = Palette.init(),
            .background_priority = [_]Prioriy{Prioriy.None} ** SCREEN_WIDTH,
            .dots = 0,
            .interrupt = interrupt,
            .pixels = [_]u32{1000000} ** (SCREEN_HEIGHT * SCREEN_WIDTH),
            .updated = false,
        };
    }

    pub fn read_byte(self: *GPU, address: u16) u8 {
        switch (address) {
            0x8000...0x9FFF => return self.vram[address - 0x8000],
            0xFE00...0xFE9F => return self.oam[address - 0xFE00],
            0xFF40 => return self.lcd.read_byte(address),
            0xFF41 => {
                var value = self.stat.read_byte(address);
                if (self.line_y == self.line_y_compare) value = bit.set(value, 2);
                return value;
            },
            0xFF42 => return self.scroll_y,
            0xFF43 => return self.scroll_x,
            0xFF44 => return self.line_y,
            0xFF45 => return self.line_y_compare,
            0xFF47 => return self.background_palette.read_byte(address),
            0xFF48 => return self.sprite_patlette_0.read_byte(address),
            0xFF49 => return self.sprite_patlette_1.read_byte(address),
            0xFF4A => return self.window_y,
            0xFF4B => return self.window_x,
            _ => {
                std.debug.print("Address: {X} is not readable for GPU.", .{address});
                unreachable;
            },
        }
    }

    pub fn write_bye(self: *GPU, address: u16, value: u8) void {
        switch (address) {
            0x8000...0x9FFF => self.vram[address - 0x8000] = value,
            0xFE00...0xFE9F => self.oam[address - 0xFE00] = value,
            0xFF40 => {
                const was_enabled = self.lcd.lcd_enable;
                self.lcd.write_bye(address, value);
                if (was_enabled and !self.lcd.lcd_enable) {
                    self.dots = 0;
                    self.line_y = 0;
                    self.stat.mode = StatMode.HBlank;
                    self.clear_screen();
                }
                if (!was_enabled and self.lcd.lcd_enable) {
                    self.switch_mode(StatMode.OAMRead);
                    self.dots = 4;
                }
            },
            0xFF41 => self.stat.write_byte(address, value),
            0xFF42 => self.scroll_y = value,
            0xFF43 => self.scroll_x = value,
            0xFF44 => {}, // line_y is read only
            0xFF45 => self.line_y_compare = value,
            0xFF47 => self.background_palette.write_byte(address, value),
            0xFF48 => self.sprite_patlette_0.write_byte(address, value),
            0xFF49 => self.sprite_patlette_1.write_byte(address, value),
            0xFF4A => self.window_y = value,
            0xFF4B => self.window_x = value,
            _ => {
                std.debug.print("Address: {X} is not writeable for GPU.", .{address});
                unreachable;
            },
        }
    }

    pub fn update(self: *GPU, cycles: u32) void {
        if (!self.lcd.lcd_enable) {
            return;
        }
        self.h_blank = false;
        var _cycles = cycles;
        while (_cycles > 0) {
            const current_cycles = if (cycles >= 80) 80 else _cycles;
            self.dots += current_cycles;
            _cycles -= current_cycles;
            // One full line
            if (self.dots >= LINE_WIDTH) {
                self.dots -= LINE_WIDTH;
                self.line_y = (self.line_y + 1) % FRAME_LINES;
                if (self.stat.lyc_interrupt and (self.line_y == self.line_y_compare)) {
                    self.interrupt.set_interrupt(InterruptSource.STAT);
                }

                // Mode 1: Vblank
                // Happens after last visible row (143), handles the last 10 rows for 4,560 cycles
                // Then goes back to the first row
                if (self.line_y >= SCREEN_HEIGHT and self.stat.mode != StatMode.VBlank) {
                    self.switch_mode(StatMode.VBlank);
                }
            }

            // Normal line
            if (self.line_y < SCREEN_HEIGHT) {
                if (self.dots <= 80) {
                    if (self.stat.mode != StatMode.OAMRead) {
                        self.switch_mode(StatMode.OAMRead);
                    }
                    // Tiles and sprites rednered
                    else if (self.dots <= (80 + 172)) {
                        if (self.stat.mode != StatMode.VRAMRead) {
                            self.switch_mode(StatMode.VRAMRead);
                        }
                    } else if (self.stat.mode != StatMode.HBlank) {
                        self.switch_mode(StatMode.HBlank);
                    }
                }
            }
        }
    }

    fn switch_mode(self: *GPU, mode: StatMode) void {
        self.stat.mode = mode;
        const interrupt = switch (self.stat.mode) {
            StatMode.HBlank => {
                self.render_scanline();
                self.h_blank = true;
                return self.stat.hblank_interrupt;
            },
            StatMode.VBlank => {
                self.interrupt.set_interrupt(InterruptSource.VBLank);
                self.updated = true;
                return self.stat.vblank_interrupt;
            },
            StatMode.OAMRead => return self.stat.oam_interrupt,
            StatMode.VRAMRead => return false,
        };
        if (interrupt) {
            self.interrupt.set_interrupt(InterruptSource.STAT);
        }
    }

    fn render_scanline(self: *GPU) void {
        if (self.lcd.background_window_enable) self.render_background();
        if (self.lcd.sprite_enable) self.render_sprites();
    }

    fn render_background(self: *GPU) void {
        const is_window_y = self.lcd.window_enable and self.line_y >= self.window_y;
        const background_y = self.line_y +% self.scroll_y;
        for (0..SCREEN_WIDTH) |x| {
            const is_window_x = self.lcd.window_enable and x >= self.window_x -% 7;
            const is_window = is_window_x and is_window_y;
            const background_x = x +% self.scroll_x;
            const tile_address = undefined;
            if (is_window) {
                const offset_y = self.line_y +% self.window_y;
                const offset_x = x -% (self.window_x -% 7);
                tile_address = get_address(self.lcd.window_tilemap, offset_x, offset_y);
            } else {
                tile_address = get_address(self.lcd.background_tilemap, background_x, background_y);
            }
            const tile = self.read_byte(tile_address);
            const tile_base_address = self.get_tile_address(tile);
            const tile_offset: u16 = if (is_window) ((self.line_y - self.window_y) % 8) * 2 else (background_y % 8) * 2;
            const tile_data_address = tile_base_address + tile_offset;
            const tile_data: [2]u8 = [_]u8{ self.read_bye(tile_data_address), self.read_bye(tile_data_address + 1) };
            const x_bit: u8 = if (is_window) (self.window_x -% x) % 8 else 7 - (background_x % 8);
            const color_number_left = tile_data[0] and (@as(u8, 1) << x_bit) > 0;
            const color_number_right = if (tile_data[1] & (@as(u8, 1) << x_bit) > 0) 2 else 0;
            const color_number = color_number_left | color_number_right;
            self.background_priority[@as(usize, x)] = if (color_number == 0) Prioriy.Color0 else Prioriy.None;
        }
    }

    fn render_sprites(self: *GPU) void {
        const line = @as(i16, self.line_y);
        const size = @as(i16, self.lcd.sprite_size);
        // sprites with a lower index have a lower priority
        var index = 39;
        while (index >= 0) : (index -= 1) {
            const sprite = self.fetch_sprite(index);
            if (line < sprite.y or line >= sprite.y + size) {
                continue;
            }
            const tile_base_address = 0x8000 + (@as(u16, sprite.tile_number) * 16);
            const tile_offset = if (sprite.y_flip) size - 1 - (line - sprite.y) else line - sprite.y;
            const tile_data_address = tile_base_address + (tile_offset * 2);
            const tile_data: [2]u8 = [_]u8{
                self.read_byte(tile_data_address),
                self.read_byte(tile_data_address + 1),
            };
            // set each pixel
            for (0..8) |x| {
                const pixel_x = sprite.x + x;
                if (pixel_x < 0 or pixel_x >= 160) continue;
                const x_bit = if (sprite.x_flip) x else 7 - x;
                const color_index_left = tile_data[0] and (@as(u8, 1) << x_bit) > 0;
                const color_index_right = if (tile_data[1] & (@as(u8, 1) << x_bit) > 0) 2 else 0;
                const color_index = color_index_left | color_index_right;
                // Skip transparent pixels
                if (color_index == 0) {
                    continue;
                }
                const color = if (sprite.is_palette_1) self.sprite_patlette_1.get_shade(color_index) else self.sprite_patlette_0.get_shade(color_index);
                // Skip if background has prioirty
                if (sprite.below_background and self.background_priority[sprite.x + x] != Prioriy.Color0) {
                    continue;
                }
                self.set_pixel(pixel_x, color);
            }
        }
    }

    fn fetch_sprite(self: *GPU, index: u16) Sprite {
        const address = 0xFE00 + index * 4;
        const y = @as(i16, self.read_byte(address)) - 16;
        const x = @as(i16, self.read_byte(address + 1)) - 8;
        const bits = self.read_byte(address + 3);
        const tile_number_byte = self.read_byte(address + 2);
        const tile_number_sprite_size = if (self.lcd.sprite_size == 16) 0xFE else 0xFF;
        return Sprite{
            .y = y,
            .x = x,
            .tile_number = tile_number_byte & tile_number_sprite_size,
            .below_background = bit.is_set(bits, 7),
            .y_flip = bit.is_set(bits, 6),
            .x_flip = bit.is_set(bits, 5),
            .is_palette_1 = bit.is_set(bits, 4),
        };
    }

    fn clear_screen(self: *GPU) void {
        for (&self.pixels) |*pixel| {
            pixel.* = std.math.maxInt(u32);
        }
        for (&self.background_priority) |*priority| {
            priority.* = Prioriy.None;
        }
        self.updated = true;
    }

    fn set_pixel(self: *GPU, x: u16, color: u32) void {
        const start: u16 = (self.line_y * SCREEN_WIDTH + x) * 4;
        var rgba: [3]u8 = undefined;
        const shift: u4 = 8;
        const color_value = color << shift;
        std.mem.writeInt(u32, rgba[0..], color_value, .big);
        self.pixels[start] = rgba[0];
        self.pixels[start + 1] = rgba[1];
        self.pixels[start + 2] = rgba[2];
        self.pixels[start + 3] = rgba[3];
    }

    pub fn set_colors(self: *GPU, colors: [4]u32) void {
        const previous_colors = self.background_palette.colors;
        const previous_line_y = self.line_y;
        for (0..SCREEN_HEIGHT) |y| {
            self.line_y = @as(u8, y);
            for (0..SCREEN_WIDTH) |x| {
                const index = y * 166 + x;
                switch (self.pixels[index]) {
                    previous_colors[0] => self.set_pixel(x, colors[0]),
                    previous_colors[1] => self.set_pixel(x, colors[1]),
                    previous_colors[2] => self.set_pixel(x, colors[2]),
                    previous_colors[3] => self.set_pixel(x, colors[3]),
                    _ => unreachable,
                }
            }
        }
        self.line_y = previous_line_y;
        self.background_palette.set_colors(colors);
        self.sprite_patlette_0.set_colors(colors);
        self.sprite_patlette_1.set_colors(colors);
    }

    pub fn take_updated(self: *GPU) bool {
        const updated = self.updated;
        self.updated = false;
        return updated;
    }

    fn get_address(base: u16, x: u8, y: u8) u16 {
        const y_16 = @as(u16, y) / 8 * 32;
        const x_16 = @as(u16, x) / 8;
        return base + y_16 + x_16;
    }

    fn get_tile_address(self: *GPU, tile_numer: u8) u16 {
        const offset: u16 = 0x9000;
        switch (self.lcd.background_tilemap) {
            0x8000 => return 0x8000 + tile_numer,
            _ => return (offset +% tile_numer) *% 16,
        }
    }
};
