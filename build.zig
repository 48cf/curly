const std = @import("std");

const build_mode = .Debug;

fn addExecutable(b: *std.build.Builder, target: std.zig.CrossTarget, comptime name: []const u8) void {
    const exe = b.addExecutable(name, name ++ "/main.zig");

    exe.addPackagePath("isa", "isa/isa.zig");
    exe.setTarget(target);
    exe.setBuildMode(build_mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run-" ++ name, "Run the " ++ name);
    run_step.dependOn(&run_cmd.step);
}

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});

    addExecutable(b, target, "assembler");
    addExecutable(b, target, "emulator");
}
