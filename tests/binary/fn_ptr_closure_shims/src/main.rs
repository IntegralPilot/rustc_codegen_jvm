// Regression test: captureless closures coerced to fn pointers.
//
// Reifying a closure resolves to an FnOnce::call_once shim whose calling
// convention is (closure_zst, args_tuple) — not the fn pointer's flat args.
// The generated FnPtrImpl adapter previously forwarded the flat args
// unchanged, leaving the stack one value short at the invokestatic (caught by
// ProGuard). The adapter must construct the ZST closure receiver and wrap the
// fn-ptr args into the shim's tuple carrier.
//
// The same adapter generation runs in two places: MIR rvalue casts
// (`let f: fn(..) = |..| ..`) and const evaluation (fn-pointer fields in
// consts/statics, which is how the original failure surfaced via
// io_error::OsFunctions::DEFAULT in core).

struct Ops {
    unary: fn(i32) -> i32,
    binary: fn(i32, i32) -> i32,
}

fn named_add_one(x: i32) -> i32 {
    x + 1
}

// Const-eval path: closures inside a const initializer.
const CLOSURE_OPS: Ops = Ops {
    unary: |x| x * 3,
    binary: |a, b| a + b,
};

// Const-eval path with a named fn, which must keep working alongside shims.
const NAMED_OPS: Ops = Ops {
    unary: named_add_one,
    binary: |a, b| a * b,
};

// Static (memory-read) path.
static STATIC_UNARY: fn(i32) -> i32 = |x| x - 4;

fn apply(f: fn(i32) -> i32, v: i32) -> i32 {
    f(v)
}

fn apply2(f: fn(i32, i32) -> i32, a: i32, b: i32) -> i32 {
    f(a, b)
}

fn main() {
    // MIR cast path: direct coercion in a let binding.
    let double: fn(i32) -> i32 = |x| x * 2;
    assert!(double(21) == 42);

    // MIR cast path: coercion at a call argument position.
    assert!(apply(|x| x + 10, 5) == 15);

    // Multi-arg closure: the shim's args tuple has two fields.
    let sub: fn(i32, i32) -> i32 = |a, b| a - b;
    assert!(sub(10, 3) == 7);
    assert!(apply2(|a, b| a * b + 1, 4, 5) == 21);

    // Const-eval path.
    assert!((CLOSURE_OPS.unary)(7) == 21);
    assert!((CLOSURE_OPS.binary)(20, 22) == 42);
    assert!((NAMED_OPS.unary)(41) == 42);
    assert!((NAMED_OPS.binary)(6, 7) == 42);

    // Static path.
    assert!(STATIC_UNARY(46) == 42);
    assert!(apply(STATIC_UNARY, 10) == 6);

    // Fn pointers stored and re-read through locals keep working.
    let picked = if sub(1, 0) == 1 {
        CLOSURE_OPS.unary
    } else {
        NAMED_OPS.unary
    };
    assert!(picked(2) == 6);
}
