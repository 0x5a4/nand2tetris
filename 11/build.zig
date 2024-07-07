const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const is_release = switch (optimize) {
        .Debug => false,
        .ReleaseSafe, .ReleaseFast, .ReleaseSmall => true,
    };

    const exe = b.addExecutable(.{ .name = "hackvmc", .root_source_file = b.path("src/main.zig"), .target = target, .optimize = optimize, .strip = is_release });

    const pure_asm_option = b.option(bool, "pure-asm", "dont generate comments in asm output") orelse false;

    const exe_options = b.addOptions();
    exe.root_module.addOptions("build_options", exe_options);

    exe_options.addOption(bool, "pure_asm", pure_asm_option);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_options = b.addOptions();
    exe_unit_tests.root_module.addOptions("build_options", test_options);

    test_options.addOption(bool, "pure_asm", false);

    exe_unit_tests.root_module.addOptions("build_options", test_options);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
