pub fn set(value: u8, bit: u3) u8 {
    const one: u8 = 1;
    const mask: u8 = one << bit;
    return value | mask;
}

pub fn turn_off(value: u8, bit: u3) u8 {
    const one: u8 = 1;
    const mask: u8 = ~(one << bit);
    return value & mask;
}

pub fn is_set(value: u8, bit: u3) bool {
    const one: u8 = 1;
    const mask: u8 = one << bit;
    return (value & mask) != 0;
}

pub fn is_set16(value: u16, bit: u3) bool {
    const one: u8 = 1;
    const mask: u8 = one << bit;
    return (value & mask) != 0;
}

const expect = @import("std").testing.expect;

test "set" {
    var actual: u8 = 247;
    const expected: u8 = 255;
    actual = set(actual, 3);
    try expect(actual == expected);
}

test "set_when_already_set" {
    var actual: u8 = 255;
    const expected: u8 = 255;
    actual = set(actual, 3);
    try expect(actual == expected);
}

test "turn_off" {
    var actual: u8 = 255;
    const expected: u8 = 247;
    actual = turn_off(actual, 3);
    try expect(actual == expected);
}

test "is_set" {
    const value: u8 = 255;
    const result = is_set(value, 4);
    try expect(result);
}

test "is_set_false" {
    const value: u8 = 0;
    const result = is_set(value, 4);
    try expect(!result);
}
