const InstructionType = enum {
    NONE,
    NOP,
    LD,
    INC,
    DEC,
    RLCA,
    ADD,
    RRCA,
    STOP,
    RLA,
    JR,
    RRA,
    DAA,
    CPL,
    SCF,
    CCF,
    HALT,
    ADC,
    SUB,
    SBC,
    AND,
    XOR,
    OR,
    CP,
    POP,
    JP,
    PUSH,
    RET,
    CB,
    CALL,
    RETI,
    LDH,
    JPHL,
    DI,
    EI,
    RST,
    ERR,
    // CB INSTRUCTIONS,
    RLC,
    RRC,
    RL,
    RR,
    SLA,
    SRA,
    SWAP,
    SRL,
    BIT,
    RES,
    SET,
};

const AddressMode = enum {
    IMP,
    R_D16,
    R_R,
    MR_R,
    R,
    R_D8,
    R_MR,
    R_HLI,
    R_HLD,
    HLI_R,
    HLD_R,
    R_A8,
    A8_R,
    HL_SPR,
    D16,
    D8,
    D16_R,
    MR_D8,
    MR,
    A16_R,
    R_A16,
};

const RegisterType = enum {
    NONE,
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
};

const ConditionType = enum {
    NONE,
    ZERO,
    SUBTRACT,
    HALF_CARRY,
    CARRY,
};

const Instruction = struct {
    instruction_type: InstructionType,
    address_mode: AddressMode,
    register1: RegisterType,
    register2: RegisterType,
    condition_type: ConditionType,
    param: u8,
};

const instructions: [256]Instruction = .{
    // 0x00
    .{
        .instruction_type = InstructionType.NOP,
        .address_mode = AddressMode.IMP,
    },
    // 0x01
    .{
        .instruction_type = Instruction.LD,
        .address_mode = AddressMode.R_D16,
        .register1 = RegisterType.BC,
    },
    // 0x02
    .{
        .instruction_type = Instruction.LD,
        .address_mode = AddressMode.MR_R,
        .register1 = RegisterType.BC,
        .register2 = RegisterType.A,
    },
    // 0x03
    .{
        .instruction_type = InstructionType.INC,
        .address_mode = AddressMode.R,
        .register1 = RegisterType.BC,
    },
    // 0x04
    .{
        .instruction_type = InstructionType.INC,
        .address_mode = AddressMode.R,
        .register1 = RegisterType.B,
    },
    // 0x05
    .{
        .instruction_type = InstructionType.DEC,
        .address_mode = AddressMode.R,
        .register1 = RegisterType.B,
    },
    // 0x06
    .{
        .instruction_type = InstructionType.LD,
        .address_mode = AddressMode.R_D8,
        .register1 = RegisterType.B,
    },
    // 0x07
    .{
        .instruction_type = InstructionType.RLA,
    },
    // 0x08
    .{
        .instruction_type = InstructionType.LD,
        .address_mode = AddressMode.A16_R,
        .register1 = RegisterType.SP,
        .register2 = RegisterType.SP,
    },
};

pub fn opcode_to_instruction(opcode: u8) Instruction {
    switch (opcode) {
        0x00 => .{
            .instruction_type = InstructionType.NOP,
            .address_mode = AddressMode.IMP,
        },
        0x01 => .{
            .instruction_type = Instruction.LD,
            .address_mode = AddressMode.R_D16,
            .register1 = RegisterType.BC,
        },
        0x02 => .{
            .instruction_type = Instruction.LD,
            .address_mode = AddressMode.MR_R,
            .register1 = RegisterType.BC,
            .register2 = RegisterType.A,
        },
        0x03 => .{
            .instruction_type = InstructionType.INC,
            .address_mode = AddressMode.R,
            .register1 = RegisterType.BC,
        },
        0x04 => .{
            .instruction_type = InstructionType.INC,
            .address_mode = AddressMode.R,
            .register1 = RegisterType.B,
        },
        0x05 => .{
            .instruction_type = InstructionType.DEC,
            .address_mode = AddressMode.R,
            .register1 = RegisterType.B,
        },
        0x06 => .{
            .instruction_type = InstructionType.LD,
            .address_mode = AddressMode.R_D8,
            .register1 = RegisterType.B,
        },
        0x07 => .{
            .instruction_type = InstructionType.RLA,
        },
        0x08 => .{
            .instruction_type = InstructionType.LD,
            .address_mode = AddressMode.A16_R,
            .register1 = RegisterType.SP,
            .register2 = RegisterType.SP,
        },
        else => std.debug.print("Opcode [{d}] is not implemented yet.", .{opcode}),
    }
}

const std = @import("std");
