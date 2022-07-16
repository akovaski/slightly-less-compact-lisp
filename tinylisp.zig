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
};
var cell: [1024]Expr = [1]Expr{undefined} ** 1024;
const A: [*:0]u8 = @ptrCast([*:0]u8, &cell);
var hp: usize = 0;
const sp: i32 = cell.len;
fn atom(name: []const u8) !Expr {
    var i: usize = 0;
    while (i < hp) {
        const heap_str = std.mem.span(A + i);
        if (std.mem.eql(u8, heap_str, name)) {
            return Expr{ .atom = heap_str };
        }
        i += heap_str.len + 1;
    }
    if (i == hp and i + name.len + 1 <= sp * @sizeOf(Expr)) {
        for (name) |char| {
            (A + i).* = char;
            i += 1;
        }
        (A + i).* = 0;
        hp += name.len + 1;
        return Expr{ .atom = std.mem.span(A + i) };
    } else {
        return error.OutOfHeapMemory;
    }
}
fn print_heap() void {
    std.log.info("HEAP: (hp={})", .{hp});
    var i: usize = 0;
    while (i < hp) {
        const heap_str = std.mem.span(A + i);
        std.log.info(" => {s}", .{heap_str});
        i += heap_str.len + 1;
    }
}

const nil: Expr = Expr{.nil};
var tru: Expr = undefined;
var err: Expr = undefined;

pub fn main() !void {
    tru = try atom("#t");
    err = try atom("ERR");
    _ = tru;
    _ = err;
    print_heap();
}
