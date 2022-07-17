const std = @import("std");

// as of writing, zig will not compile the recursive definitions of Expr below
const Expr = union(enum) {
    number: f64,
    atom: []const u8,
    string: []const u8,
    primative: *void, //fn (Expr, Expr) Expr,
    cons: *void, //*[2]Expr,
    closure: *void, //*[2]Expr,
    nil,
    fn equals(self: Expr, other: Expr) bool {
        return self == other;
    }
    fn not(self: Expr) bool {
        return self == .nil;
    }
    pub fn format(self: Expr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("Expr{{ .{s} = ", .{@tagName(self)});
        switch (self) {
            .number => |number| try writer.print("{}", .{number}),
            .atom => |name| try writer.writeAll(name),
            .nil => try writer.writeAll("void"),
            else => try writer.writeAll("other"),
        }
        try writer.writeAll(" }");
    }
};
var cell: [1024]Expr = [1]Expr{undefined} ** 1024;
const A: [*:0]u8 = @ptrCast([*:0]u8, &cell);
var hp: usize = 0;
var sp: usize = cell.len;
fn cell_space_check(heap_pointer: usize, stack_pointer: usize, wanted_space: usize) bool {
    return stack_pointer * @sizeOf(Expr) - heap_pointer >= wanted_space;
}
fn print_heap() void {
    std.log.info("HEAP: (hp={})", .{hp});
    var i: usize = 0;
    while (i < hp) {
        const heap_str = std.mem.span(A + i);
        std.log.info("A+{} => {s}", .{ i, heap_str });
        i += heap_str.len + 1;
    }
}
fn print_stack() void {
    std.log.info("STACK: (sp={})", .{sp});
    var i = sp;
    while (i < cell.len) {
        std.log.info("cell[{}] => {}", .{ i, cell[i] });
        i += 1;
    }
}

fn atom(name: []const u8) !Expr {
    var i: usize = 0;
    while (i < hp) {
        const heap_str = std.mem.span(A + i);
        if (std.mem.eql(u8, heap_str, name)) {
            return Expr{ .atom = heap_str };
        }
        i += heap_str.len + 1;
    }
    if (i != hp) {
        return error.MisalignedHeap;
    }
    if (!cell_space_check(i, sp, name.len + 1)) {
        return error.OutOfHeapMemory;
    }
    const name_ptr = A + i;
    for (name) |char| {
        (A + i).* = char;
        i += 1;
    }
    (A + i).* = 0;
    hp += name.len + 1;
    return Expr{ .atom = std.mem.span(name_ptr) };
}

fn cons(x: Expr, y: Expr) !Expr {
    if (!cell_space_check(hp, sp, 2 * @sizeOf(Expr))) {
        return error.OutOfStackMemory;
    }
    sp -= 1;
    cell[sp] = x;
    sp -= 1;
    cell[sp] = y;
    return Expr{ .cons = @ptrCast(*void, &cell[sp]) };
}

const nil: Expr = Expr.nil;
var tru: Expr = undefined;
var err: Expr = undefined;

pub fn main() !void {
    tru = try atom("#t");
    err = try atom("ERR");
    _ = tru;
    _ = err;
    _ = try cons(tru, nil);
    print_heap();
    print_stack();
}
