const std = @import("std");

// as of writing, zig will not compile the recursive definitions of Expr below
const Expr = union(enum) {
    number: f64,
    atom: []const u8,
    string: []const u8,
    primative: *const PrimativeMatch,
    cons: *ExprOpaque, //*[2]Expr,
    closure: *ExprOpaque, //*[2]Expr,
    nil,
    fn equals(self: Expr, other: Expr) bool {
        return switch (self) {
            .number => other == .number and self.number == other.number,
            .atom => other == .atom and std.mem.eql(u8, self.atom, other.atom),
            .string => other == .string and std.mem.eql(u8, self.string, other.string),
            .primative => other == .primative and self.primative == other.primative,
            .cons => other == .cons and self.cons == other.cons,
            .closure => other == .closure and self.closure == other.closure,
            .nil => other == .nil,
        };
    }
    fn not(self: Expr) bool {
        return self == .nil;
    }
    fn defaultTo(self: Expr, other: Expr) Expr {
        return if (self.not()) other else self;
    }
    pub fn format(self: Expr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .number => |number| try writer.print("{d}", .{number}),
            .atom => |name| try writer.writeAll(name),
            .string => |str| try writer.print("\"{s}\"", .{str}),
            .primative => |p| try writer.print("<{s}>", .{p.s}),
            .cons => try formatlist(self, writer),
            .closure => |arr_ptr| try writer.print("{}", .{(@ptrToInt(arr_ptr) - @ptrToInt(A)) / @sizeOf(Expr)}),
            .nil => try writer.writeAll("()"),
        }
    }
    fn formatlist(l: Expr, writer: anytype) !void {
        try writer.writeAll("(");
        var t = l;
        while (true) {
            try writer.print("{}", .{car(t)});
            t = cdr(t);
            switch (t) {
                .nil => break,
                .cons => try writer.writeAll(" "),
                else => {
                    try writer.print(" . {}", .{t});
                    break;
                },
            }
        }
        try writer.writeAll(")");
    }
    fn cons_arr(arr_ptr: *ExprOpaque) *[2]Expr {
        return @ptrCast(*[2]Expr, @alignCast(@alignOf(Expr), arr_ptr));
    }
    fn num(self: Expr) !f64 {
        return switch (self) {
            .number => |f| f,
            else => error.ExpressionIsNotNumber,
        };
    }
};
const ExprOpaque = opaque {};
const PrimativeOpaque = opaque {};
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

fn cons(x: Expr, y: Expr) error{OutOfStackMemory}!Expr {
    if (!cell_space_check(hp, sp, 2 * @sizeOf(Expr))) {
        return error.OutOfStackMemory;
    }
    sp -= 1;
    cell[sp] = x;
    sp -= 1;
    cell[sp] = y;
    return Expr{ .cons = @ptrCast(*ExprOpaque, &cell[sp]) };
}

fn car(p: Expr) Expr {
    return switch (p) {
        .cons, .closure => |arr_ptr| Expr.cons_arr(arr_ptr)[1],
        else => err,
    };
}
fn cdr(p: Expr) Expr {
    return switch (p) {
        .cons, .closure => |arr_ptr| Expr.cons_arr(arr_ptr)[0],
        else => err,
    };
}
fn pair(v: Expr, x: Expr, e: Expr) !Expr {
    return cons(try cons(v, x), e);
}
fn closure(v: Expr, x: Expr, e: Expr) !Expr {
    const c = try pair(v, x, if (e.equals(env)) nil else e);
    return Expr{ .closure = c.cons };
}
fn assoc(a: Expr, e: Expr) Expr {
    var x = e;
    while (x == .cons and !a.equals(car(car(x)))) {
        x = cdr(x);
    }
    return if (x == .cons) cdr(car(x)) else err;
}
fn eval(x: Expr, e: Expr) anyerror!Expr {
    return switch (x) {
        .atom => assoc(x, e),
        .cons => apply(try eval(car(x), e), cdr(x), e),
        else => x,
    };
}
fn apply(f: Expr, t: Expr, e: Expr) !Expr {
    return switch (f) {
        .primative => |p| p.f(t, e),
        .closure => reduce(f, t, e),
        else => err,
    };
}
fn reduce(f: Expr, t: Expr, e: Expr) !Expr {
    return eval(cdr(car(f)), try bind(car(car(f)), try evlis(t, e), cdr(f).defaultTo(env)));
}
fn evlis(t: Expr, e: Expr) anyerror!Expr {
    return switch (t) {
        .cons => cons(try eval(car(t), e), try evlis(cdr(t), e)),
        else => eval(t, e),
    };
}
fn bind(v: Expr, t: Expr, e: Expr) !Expr {
    return switch (v) {
        .nil => e,
        .cons => bind(cdr(v), cdr(t), try pair(car(v), car(t), e)),
        else => pair(v, t, e),
    };
}

const PrimativeFunc = fn (Expr, Expr) anyerror!Expr;
const PrimativeMatch = struct { s: []const u8, f: PrimativeFunc };
const prim = [_]PrimativeMatch{
    .{ .s = "eval", .f = f_eval },
    .{ .s = "quote", .f = f_quote },
    .{ .s = "cons", .f = f_cons },
    .{ .s = "car", .f = f_car },
    .{ .s = "cdr", .f = f_cdr },
    .{ .s = "+", .f = f_add },
    .{ .s = "-", .f = f_sub },
    .{ .s = "*", .f = f_mul },
    .{ .s = "/", .f = f_div },
    .{ .s = "int", .f = f_int },
    .{ .s = "<", .f = f_lt },
    .{ .s = "eq?", .f = f_eq },
    .{ .s = "or", .f = f_or },
    .{ .s = "and", .f = f_and },
    .{ .s = "not", .f = f_not },
    .{ .s = "cond", .f = f_cond },
    .{ .s = "if", .f = f_if },
    .{ .s = "let*", .f = f_leta },
    .{ .s = "lambda", .f = f_lambda },
    .{ .s = "define", .f = f_define },
};
fn f_eval(t: Expr, e: Expr) !Expr {
    return eval(car(try evlis(t, e)), e);
}
fn f_quote(t: Expr, _: Expr) !Expr {
    return car(t);
}
fn f_cons(t: Expr, e: Expr) !Expr {
    const tx = try evlis(t, e);
    return cons(car(tx), car(cdr(tx)));
}
fn f_car(t: Expr, e: Expr) !Expr {
    return car(car(try evlis(t, e)));
}
fn f_cdr(t: Expr, e: Expr) !Expr {
    return cdr(car(try evlis(t, e)));
}
fn f_add(t: Expr, e: Expr) !Expr {
    var tx = try evlis(t, e);
    var n = try car(tx).num();
    tx = cdr(tx);
    while (!tx.not()) {
        n += try car(tx).num();
        tx = cdr(tx);
    }
    return Expr{ .number = n };
}
fn f_sub(t: Expr, e: Expr) !Expr {
    var tx = try evlis(t, e);
    var n = try car(tx).num();
    tx = cdr(tx);
    while (!tx.not()) {
        n -= try car(tx).num();
        tx = cdr(tx);
    }
    return Expr{ .number = n };
}
fn f_mul(t: Expr, e: Expr) !Expr {
    var tx = try evlis(t, e);
    var n = try car(tx).num();
    tx = cdr(tx);
    while (!tx.not()) {
        n *= try car(tx).num();
        tx = cdr(tx);
    }
    return Expr{ .number = n };
}
fn f_div(t: Expr, e: Expr) !Expr {
    var tx = try evlis(t, e);
    var n = try car(tx).num();
    tx = cdr(tx);
    while (!tx.not()) {
        n /= try car(tx).num();
        tx = cdr(tx);
    }
    return Expr{ .number = n };
}
fn f_int(t: Expr, e: Expr) !Expr {
    const n = car(try evlis(t, e));
    return switch (n) {
        .number => |f| Expr{ .number = std.math.floor(f) },
        else => error.ExpressionIsNotNumber,
    };
}
fn f_lt(t: Expr, e: Expr) !Expr {
    const tx = try evlis(t, e);
    return if ((try car(tx).num()) - (try car(cdr(tx)).num()) < 0) tru else nil;
}
fn f_eq(t: Expr, e: Expr) !Expr {
    const tx = try evlis(t, e);
    return if (car(tx).equals(car(cdr(tx)))) tru else nil;
}
fn f_or(t: Expr, e: Expr) !Expr {
    var tx = t;
    while (tx != .nil) {
        if (!(try eval(car(tx), e)).not()) return tru;
        tx = cdr(tx);
    }
    return nil;
}
fn f_and(t: Expr, e: Expr) !Expr {
    var tx = t;
    while (tx != .nil) {
        if ((try eval(car(tx), e)).not()) return nil;
        tx = cdr(tx);
    }
    return tru;
}
fn f_not(t: Expr, e: Expr) !Expr {
    const tx = try evlis(t, e);
    return if (car(tx).not()) tru else nil;
}
fn f_cond(t: Expr, e: Expr) !Expr {
    var tx = t;
    while (tx != .nil and (try eval(car(car(tx)), e)).not()) {
        tx = cdr(tx);
    }
    return eval(car(cdr(car(tx))), e);
}
fn f_if(t: Expr, e: Expr) !Expr {
    return eval(car(cdr(if ((try eval(car(t), e)).not()) cdr(t) else t)), e);
}
fn f_leta(t: Expr, e: Expr) !Expr {
    var ex = e;
    var tx = t;
    while (tx != .nil and cdr(tx) != .nil) {
        ex = try pair(car(car(tx)), try eval(car(cdr(car(tx))), ex), ex);
        tx = cdr(tx);
    }
    return eval(car(tx), ex);
}
fn f_lambda(t: Expr, e: Expr) !Expr {
    return closure(car(t), car(cdr(t)), e);
}
fn f_define(t: Expr, e: Expr) !Expr {
    env = try pair(car(t), try eval(car(cdr(t)), e), env);
    return car(t);
}

const nil: Expr = Expr.nil;
var tru: Expr = undefined;
var err: Expr = undefined;
var env: Expr = undefined;

var buf: [40]u8 = [1]u8{0} ** 40;
const buf_str = @ptrCast([*:0]u8, &buf);
var see: u8 = ' ';
fn look() error{ EOF, StdInReadFailure }!void {
    var read_buf = [1]u8{0};
    const bytes_read = std.io.getStdIn().read(&read_buf) catch {
        return error.StdInReadFailure;
    };
    if (bytes_read == 0) {
        // may happen if EOF received
        return error.EOF;
    } else if (bytes_read != 1) {
        std.log.err("invalid number of bytes read: {}", .{bytes_read});
    }
    see = read_buf[0];
}
fn seeing(c: u8) bool {
    return if (c == ' ') see > 0 and see <= c else see == c;
}
fn get() error{ EOF, StdInReadFailure }!u8 {
    const c = see;
    try look();
    return c;
}
fn scan() error{ EOF, StdInReadFailure }!u8 {
    var i: usize = 0;
    while (seeing(' ')) try look();
    if (seeing('(') or seeing(')') or seeing('\'')) {
        buf[i] = try get();
        i += 1;
    } else {
        buf[i] = try get();
        i += 1;
        while (i < 39 and !seeing('(') and !seeing(')') and !seeing(' ')) {
            buf[i] = try get();
            i += 1;
        }
    }
    buf[i] = 0;
    return buf[0];
}
fn read() !Expr {
    _ = try scan();
    return parse();
}
fn parse() error{ParseError}!Expr {
    return switch (buf[0]) {
        '(' => list(),
        '\'' => quote(),
        else => atomic(),
    } catch {
        return error.ParseError;
    };
}
fn list() error{ ListError, ParseError, EOF, StdInReadFailure, OutOfStackMemory }!Expr {
    if ((try scan()) == ')') {
        return nil;
    } else if (std.mem.eql(u8, std.mem.span(buf_str), ".")) {
        const x = try read();
        _ = try scan();
        return x;
    } else {
        const x = try parse();
        return cons(x, try list());
    }
}
fn quote() !Expr {
    return cons(try atom("quote"), try cons(try read(), nil));
}
fn atomic() !Expr {
    const buf_slice = std.mem.span(buf_str);
    const x = std.fmt.parseFloat(f64, buf_slice) catch {
        return atom(buf_slice);
    };
    return Expr{ .number = x };
}
pub fn main() !void {
    tru = try atom("#t");
    err = try atom("ERR");
    env = try pair(tru, tru, nil);
    // const x = try atom("#x");
    // const y = try atom("#y");
    // var c = try cons(x, y);
    // std.log.info("c: {}", .{c});
    // std.log.info("car: {}", .{car(c)});
    // std.log.info("cdr: {}", .{cdr(c)});
    // std.log.info("assoc: {}", .{assoc(tru, env)});
    for (prim) |*p| {
        env = try pair(try atom(p.s), Expr{ .primative = p }, env);
    }
    while (true) {
        const r = read() catch |err| {
            switch (err) {
                error.EOF => {
                    // EOF Reached, exiting
                    return;
                },
                else => {
                    std.log.err("Unexpected error {}", .{err});
                    std.os.exit(1);
                },
            }
        };
        print_heap();
        print_stack();
        std.log.info("read: {}", .{r});
    }
}
