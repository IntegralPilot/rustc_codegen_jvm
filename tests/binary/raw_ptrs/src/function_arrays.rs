use std::mem::MaybeUninit;

type Stage = fn(&mut u32);

#[repr(C)]
struct Stages {
    len: usize,
    data: [MaybeUninit<Stage>; 8],
}

fn add(value: &mut u32) {
    *value += 1;
}
fn double(value: &mut u32) {
    *value *= 2;
}

#[inline(never)]
fn cloned(stages: &Stages) -> Vec<Stage> {
    let slice =
        unsafe { std::slice::from_raw_parts(stages.data.as_ptr().cast::<Stage>(), stages.len) };
    slice.iter().cloned().collect()
}

const FUNCTIONS: [Stage; 2] = [add, double];

#[inline(never)]
fn table_function(index: usize) -> Stage {
    FUNCTIONS[index]
}
#[inline(never)]
fn direct_function() -> Stage {
    add
}

pub fn run() {
    assert_eq!(
        std::hint::black_box(table_function(0)) as *const (),
        std::hint::black_box(direct_function()) as *const ()
    );
    let mut stages = Stages {
        len: 2,
        data: [MaybeUninit::uninit(); 8],
    };
    stages.data[0].write(add);
    stages.data[1].write(double);
    let mut value = 20;
    for stage in cloned(&stages) {
        stage(&mut value);
    }
    assert_eq!(value, 42);
}
