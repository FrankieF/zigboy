const std = @import("std");
const ctime = @cImport({
    @cInclude("time.h");
});

pub const RealTimeClock = struct {
    seconds: u8,
    minutes: u8,
    hours: u8,
    low_bits: u8,
    high_bits: u8,
    zero: u64,

    pub fn init(path: ?[]const u8) RealTimeClock {
        const zero = get_zero(path);
        return RealTimeClock{
            .seconds = 0,
            .minutes = 0,
            .hours = 0,
            .low_bits = 0,
            .high_bits = 0,
            .zero = zero,
        };
    }

    pub fn step(self: *RealTimeClock) void {
        const duration: u64 = get_time_since_epoch() - self.zero;
        self.seconds = @truncate(@mod(duration, 60));
        self.minutes = @truncate(@mod(duration / 60, 60));
        self.hours = @truncate(@mod(duration / 3600, 24));

        const days = duration / 3600 / 24;
        self.low_bits = @truncate(@mod(days, 256));
        switch (days) {
            0x0...0x0FF => {},
            0x100...0x1FF => self.high_bits |= 1,
            else => {
                self.high_bits |= 1;
                self.high_bits |= 0x80;
            },
        }
    }

    pub fn read_byte(self: RealTimeClock, address: u16) u8 {
        return switch (address) {
            0x80 => self.seconds,
            0x09 => self.minutes,
            0x0A => self.hours,
            0x0B => self.low_bits,
            0x0C => self.high_bits,
            else => unreachable,
        };
    }

    pub fn write_byte(self: *RealTimeClock, address: u16, value: u8) void {
        switch (address) {
            0x80 => self.seconds = value,
            0x09 => self.minutes = value,
            0x0A => self.hours = value,
            0x0B => self.low_bits = value,
            0x0C => self.high_bits = value,
            else => unreachable,
        }
    }

    fn get_zero(path: ?[]const u8) u64 {
        if (path) |p| {
            std.fs.accessAbsolute(p, .{}) catch {
                return get_time_since_epoch();
            };
            const file = std.fs.cwd().openFile(p, .{}) catch {
                //std.log.err("Failed to open file {s}", @errorName(err));
                return get_time_since_epoch();
            };
            defer file.close();
            var buffer: [8]u8 = .{0} ** 8;
            _ = file.reader().readAll(buffer[0..]) catch {
                //std.log.err("Failed to open file {s}", @errorName(err));
                return get_time_since_epoch();
            };
            const zero = std.mem.readInt(u64, &buffer, .big);
            return zero;
        }
        return get_time_since_epoch();
    }

    fn get_time_since_epoch() u64 {
        var now: ctime.time_t = undefined;
        _ = ctime.time(&now);
        return @intCast(now);
    }
};
