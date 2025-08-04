pub const Clock = struct {
    period: u32,
    value: u32,

    pub fn init(period: u32) Clock {
        return Clock{
            .period = period,
            .value = 0,
        };
    }

    pub fn tick(self: *Clock, cycles: u32) u32 {
        self.value += cycles;
        const val = self.value / self.period;
        self.value %= self.period;
        return val;
    }
};
