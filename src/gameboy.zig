const std = @import("std");
const CPU = @import("cpu.zig");
const Key = @import("controller.zig").Key;
const KeyMapping = @import("controller.zig").KeyMapping;
const Memory = @import("memory.zig").Memory;
const Cartridge = @import("cartridge.zig").Cartridge;
const Main = @import("main.zig");

const sf = struct {
    const sfml = @import("sfml");
    usingnamespace sfml;
    usingnamespace sfml.audio;
    usingnamespace sfml.graphics;
    usingnamespace sfml.window;
    usingnamespace sfml.system;
};

const STEP_TIME: u32 = 16;
const STEP_CYCLES: u32 = 70224;
const FRAME_DURATION_NANO_SECONDS: u66 = 16742674; // Approx. 10^9 / 59.7275 FPS

const keys: [8]KeyMapping = .{
    .{ .sfKey = sf.keyboard.KeyCode.Right, .myKey = Key.Right },
    .{ .sfKey = sf.keyboard.KeyCode.Left, .myKey = Key.Left },
    .{ .sfKey = sf.keyboard.KeyCode.Up, .myKey = Key.Up },
    .{ .sfKey = sf.keyboard.KeyCode.Down, .myKey = Key.Down },
    .{ .sfKey = sf.keyboard.KeyCode.A, .myKey = Key.A },
    .{ .sfKey = sf.keyboard.KeyCode.S, .myKey = Key.B },
    .{ .sfKey = sf.keyboard.KeyCode.Enter, .myKey = Key.Start },
    .{ .sfKey = sf.keyboard.KeyCode.Space, .myKey = Key.Select },
};

pub const GameBoy = struct {
    step_cycles: u32,
    cpu: CPU.CPU,
    window: sf.RenderWindow,
    texture: sf.Texture,
    tick: u64,

    pub fn init(cartridge: Cartridge) !GameBoy {
        const cpu = create_cpu(cartridge);
        const window = try sf.RenderWindow.create(.{ .x = 160, .y = 144 }, 32, "Zigboy", sf.Style.defaultStyle, null);
        const texture = try sf.Texture.create(.{ .x = 160, .y = 144 });
        return GameBoy{
            .step_cycles = 0,
            .cpu = cpu,
            .window = window,
            .texture = texture,
            .tick = 0,
        };
    }

    pub fn update(self: *GameBoy) bool {
        const frame_start = std.time.nanoTimestamp();
        while (self.window.pollEvent()) |event| {
            if (event == .closed) {
                self.window.close();
                return false;
            }
        }
        self.update_keys();
        var cycle_budget: u32 = 0;
        while (cycle_budget < STEP_CYCLES) {
            self.tick +%= 1;
            const cycles = self.cpu.tick();
            cycle_budget += cycles;
            self.cpu.memory.update(cycles);
            print_line(&self.cpu, self.tick) catch {};
        }
        if (self.cpu.memory.gpu.take_updated()) {
            self.texture.updateFromPixels(&self.cpu.memory.gpu.pixels, null) catch {
                return false;
            };
            const sprite = sf.Sprite.createFromTexture(self.texture) catch {
                return false;
            };
            self.window.draw(sprite, null);
            self.window.display();
        }
        const frame_end = std.time.nanoTimestamp();
        const elapsed = frame_end - frame_start;
        const remaining = FRAME_DURATION_NANO_SECONDS - elapsed;
        if (remaining > 0) {
            const duration: u64 = @as(u64, @intCast(remaining));
            std.time.sleep(duration);
        }
        return true;
    }

    fn update_keys(self: *GameBoy) void {
        for (keys) |key| {
            if (sf.keyboard.isKeyPressed(key.sfKey)) {
                self.cpu.memory.keypad.press(key.myKey);
            } else {
                self.cpu.memory.keypad.release(key.myKey);
            }
        }
    }

    fn create_cpu(cartridge: Cartridge) CPU.CPU {
        const memory: Memory = Memory.init(cartridge);
        const registers = CPU.Registers.init();
        const flags = CPU.Flags.init();
        return CPU.CPU.init(registers, flags, memory);
    }

    pub fn destroy(self: *GameBoy) void {
        @constCast(self.window).destroy();
        @constCast(self.texture).destroy();
    }

    fn print_line(cpu: *CPU.CPU, count: u64) !void {
        if (Main.logs != Main.Logs.On) {
            return;
        }
        var buffer: [256]u8 = undefined;
        var f: u8 = 0;
        f |= 0x80 & (@as(u8, @intFromBool(cpu.flags.zero)) << 7);
        f |= 0x40 & (@as(u8, @intFromBool(cpu.flags.subtract)) << 6);
        f |= 0x20 & (@as(u8, @intFromBool(cpu.flags.half_carry)) << 5);
        f |= 0x10 & (@as(u8, @intFromBool(cpu.flags.carry)) << 4);
        const line = try std.fmt.bufPrint(&buffer, "Tick count: {d} A:{X:0>2} F:{X:0>2} B:{X:0>2} C:{X:0>2} D:{X:0>2} E:{X:0>2} H:{X:0>2} L:{X:0>2} SP:{X:0>4} PC:{X:0>4}\n", .{
            count,
            cpu.registers.a,
            f,
            cpu.registers.b,
            cpu.registers.c,
            cpu.registers.d,
            cpu.registers.e,
            cpu.registers.h,
            cpu.registers.l,
            cpu.sp,
            cpu.pc,
        });
        std.debug.print(" cpu {s}", .{line});
    }
};
