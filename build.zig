const std = @import("std");
const sfml = @import("sfml");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zigboy",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const dep = b.dependency("sfml", .{}).module("sfml");
    exe.root_module.addImport("sfml", dep);
    dep.addIncludePath(b.path("CSFML/include/"));
    exe.addLibraryPath(b.path("CSFML/lib/msvc/"));
    exe.linkLibC();
    exe.linkSystemLibrary("csfml-graphics");
    exe.linkSystemLibrary("csfml-system");
    exe.linkSystemLibrary("csfml-window");
    exe.linkSystemLibrary("csfml-audio");

    const run = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the game");
    run_step.dependOn(&run.step);
    b.installArtifact(exe);

    const cpu_tests = b.addTest(.{
        .root_source_file = b.path("src/cpu.zig"),
        .target = target,
        .optimize = optimize,
    });

    cpu_tests.linkLibC();

    const run_cpu_tests = b.addRunArtifact(cpu_tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_cpu_tests.step);
}
