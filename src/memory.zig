const std = @import("std");
const Catridge = @import("catridge.zig").Catridge;
const Timer = @import("timer.zig").Timer;
const Interrupt = @import("interrupt.zig").Interrupt;
const GPU = @import("gpu.zig").GPU;
const Controller = @import("controller.zig");

const HIGH_RAM_LENGTH = 127;
const WORK_RAM_LENGTH = 8192;

pub const Memory = struct {
    high_ram: [HIGH_RAM_LENGTH]u8,
    work_ram: [WORK_RAM_LENGTH]u8, // 2 4kb work ram
    catridge: Catridge,
    timer: Timer,
    interrupt_enabled: u8,
    interrupt: *Interrupt,
    gpu: GPU,
    keypad: Controller.KeyPad,

    pub fn init(catridge: Catridge) Memory {
        const high_ram = [_]u8{0} ** HIGH_RAM_LENGTH;
        const work_ram = [_]u8{0} ** WORK_RAM_LENGTH;
        var interrupt = Interrupt.init();
        var memory = Memory{
            .high_ram = high_ram,
            .work_ram = work_ram,
            .catridge = catridge,
            .timer = Timer.init(&interrupt),
            .interrupt_enabled = 0,
            .interrupt = &interrupt,
            .gpu = GPU.init(&interrupt),
            .keypad = Controller.KeyPad.init(&interrupt),
        };
        memory.initialiseMemory();
        return memory;
    }

    pub fn update(self: *Memory, cycles: u32) void {
        self.timer.update(cycles);
        self.gpu.update(cycles);
        //self.audio.update(cycles);
    }

    pub fn read_byte(self: *Memory, address: u16) u8 {
        return switch (address) {
            0x000...0x7FFF => return self.catridge.read_byte(address),
            0x8000...0x9FFF => return self.gpu.read_byte(address),
            0xA000...0xBFFF => return self.catridge.read_byte(address),
            0xC000...0xDFFF => { // work ram 8 kb
                const index: u16 = address - 0xC000;
                return self.work_ram[index];
            },
            0xE000...0xEFFF => { // work ram 8 kb
                return self.work_ram[address - 0xE000];
            },
            0xFE00...0xFE9F => return self.gpu.read_byte(address),
            0xFF00 => self.keypad.read_byte(address),
            0xFF01...0xFF02 => {
                std.debug.print("Memory address[{d}] is not implemented yet.", .{address});
                return 0;
                // read serial
            },
            0xFF04...0xFF07 => return self.timer.read_byte(address),
            0xFF0F => return self.interrupt.read_byte(address),
            0xFF10...0xFF3F => {
                std.debug.print("Memory address[{d}] is not implemented yet.", .{address});
                return 0;
                // read audio byte
            },
            0xFF40...0xFF4B => self.gpu.read_byte(address),
            0xFF80...0xFFFE => return self.high_ram[address - 0xFF80],
            0xFFFF => return self.interrupt_enabled,
            else => {
                std.debug.print("Memory address[{d}] is not implemented yet.", .{address});
                return 0;
            },
        };
    }

    pub fn read_word(self: *Memory, address: u16) u16 {
        // const one = self.read_byte(address);
        // const two = self.read_byte(address + 1);
        // const three = @as(u16, two) << 8;
        //std.debug.print("\naddress is: {X}, One is : {X}. Two is : {X}, Three is : {X}", .{ address, one, two, three });
        return @as(u16, self.read_byte(address)) | (@as(u16, self.read_byte(address + 1)) << 8);
    }

    pub fn write_byte(self: *Memory, address: u16, value: u8) void {
        switch (address) {
            0x0000...0x7FFF => self.catridge.write_byte(address, value),
            0x8000...0x9FFF => self.gpu.write_bye(address, value),
            0xA000...0xBFFF => self.catridge.write_byte(address, value),
            0xC000...0xDFFF => { // work ram 8 kb
                if ((address - 0xC000) > 8180) {
                    std.debug.print("\nWriting to ram value: {X}, value:{X}", .{ (address - 0xC000), value });
                }
                self.work_ram[address - 0xC000] = value;
            },
            0xE000...0xEFFF => {
                self.work_ram[address - 0xE000] = value;
            },
            0xFE00...0xFE9F => self.gpu.write_bye(address, value),
            0xFF00 => self.keypad.write_byte(address, value),
            0xFF01...0xFF02 => {
                // write to serial
            },
            0xFF04...0xFF07 => self.timer.write_byte(address, value),
            0xFF0F => self.interrupt.write_byte(address, value),
            0xFF10...0xFF3F => {
                // write to audio
            },
            0xFF40...0xFF45 => self.gpu.write_bye(address, value),
            0xFF46 => {
                // dma transfer
            },
            0xFF47...0xFf4B => self.gpu.write_bye(address, value),
            0xFF80...0xFFFE => {
                self.high_ram[address - 0xFF80] = value;
            },
            0xFFFF => self.interrupt_enabled = value,
            else => std.debug.print("Memory address[{d}] is not implemented yet.", .{address}),
        }
    }

    pub fn write_word(self: *Memory, address: u16, value: u16) void {
        self.write_byte(address, @truncate(value & 0xFF));
        self.write_byte(address, @truncate(value >> 8));
    }

    fn initialiseMemory(self: *Memory) void {
        // http://www.codeslinger.co.uk/pages/projects/gameboy/hardware.html
        self.write_byte(0xFF05, 0x00);
        self.write_byte(0xFF06, 0x00);
        self.write_byte(0xFF07, 0x00);
        self.write_byte(0xFF10, 0x80);
        self.write_byte(0xFF11, 0xBF);
        self.write_byte(0xFF12, 0xF3);
        self.write_byte(0xFF14, 0xBF);
        self.write_byte(0xFF16, 0x3F);
        self.write_byte(0xFF17, 0x00);
        self.write_byte(0xFF19, 0xBF);
        self.write_byte(0xFF1A, 0x7F);
        self.write_byte(0xFF1B, 0xFF);
        self.write_byte(0xFF1C, 0x9F);
        self.write_byte(0xFF1E, 0xBF);
        self.write_byte(0xFF20, 0xFF);
        self.write_byte(0xFF21, 0x00);
        self.write_byte(0xFF22, 0x00);
        self.write_byte(0xFF23, 0xBF);
        self.write_byte(0xFF24, 0x77);
        self.write_byte(0xFF25, 0xF3);
        self.write_byte(0xFF26, 0xF1);
        self.write_byte(0xFF40, 0x91);
        self.write_byte(0xFF42, 0x00);
        self.write_byte(0xFF43, 0x00);
        self.write_byte(0xFF45, 0x00);
        self.write_byte(0xFF47, 0xFC);
        self.write_byte(0xFF48, 0xFF);
        self.write_byte(0xFF49, 0xFF);
        self.write_byte(0xFF4A, 0x00);
        self.write_byte(0xFF4B, 0x00);
        self.write_byte(0xFFFF, 0x00);
    }
};
