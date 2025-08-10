pub fn set(value: u8, bit: u3) u8 {
    const one: u8 = 1;
    const mask: u8 = one << bit;
    return value | mask;
}

pub fn is_set(value: u8, bit: u3) bool {
    const one: u8 = 1;
    const mask: u8 = one << bit;
    return (value & mask) != 0;
}
