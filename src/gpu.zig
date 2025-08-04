const Interrupt = @import("interrupt.zig").Interrupt;

pub const Sprite = struct {
    y: i16,
    x: i16,
    tile_number: u8,
    below_bg: bool,
    y_flip: bool,
    x_flip: bool,
    is_palette_1: bool,
};

const VRAM_SIZE = 16384;
const OAM_SIZE = 160;
const SCREEN_HEIGHT: u32 = 144;
const SCREEN_WIDTH: u32 = 160;

pub const Prioriy = enum { Color0, None };

pub const GPU = struct {
    vram: [VRAM_SIZE]u8,
    oam: [OAM_SIZE]u8,
    pixels: [SCREEN_HEIGHT * SCREEN_WIDTH]u32,
    updated: bool,
    //lcdc: LCDC,
    //stat: STAT,
    h_blank: bool,
    scroll_y: u8,
    scroll_x: u8,
    line_y: u8,
    line_y_compare: u8,
    window_y: u8,
    window_x: u8,
    //background_palette: Palette,
    //sprite_patlette_0: Palette,
    //sprite_patlette_1: Palette,
    background_priority: [SCREEN_WIDTH]Prioriy,
    dots: u32,
    interrupt: Interrupt,

    pub fn init(interrupt: Interrupt) GPU {
        return GPU{
            .vram = [_]u8{0} ** VRAM_SIZE,
            .oam = [_]u8{0} ** OAM_SIZE,
            // lcdc: LCDC.new
            // stat = stat.new
            .h_blank = false,
            .scroll_y = 0,
            .scroll_x = 0,
            .line_y = 0,
            .line_y_compare = 0,
            .window_y = 0,
            .window_x = 0,
            // bg pallete = new
            // sprite palettes = new
            .background_priority = [_]Prioriy{Prioriy.None} ** SCREEN_WIDTH,
            .dots = 0,
            .interrupt = interrupt,
            .pixels = [_]u32{1000000} ** (SCREEN_HEIGHT * SCREEN_WIDTH),
            .updated = false,
        };
    }
};
