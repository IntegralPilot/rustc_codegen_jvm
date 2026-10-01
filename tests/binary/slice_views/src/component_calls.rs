use std::hint::black_box;

#[inline(never)]
fn sum(values: &[u32], text: &str) -> usize {
    values.iter().map(|v| *v as usize).sum::<usize>() + text.len()
}

#[inline(never)]
fn tail<'a>(values: &'a [u32], text: &'a str) -> (&'a [u32], &'a str) {
    (&values[1..], &text[2..])
}

#[inline(never)]
fn count_empty(values: &[()]) -> usize { values.len() }

pub fn run() {
    super::stored_addresses::run();
    scalar_locations();
    let values = black_box([3, 5, 8, 13, 21]);
    let text = black_box("étoile");
    let (values, text) = tail(&values, text);
    let direct = sum(values, text);
    let indirect: fn(&[u32], &str) -> usize = black_box(sum);
    assert_eq!(direct, 52);
    assert_eq!(indirect(values, text), direct);
    let closure: fn(&[u32], &str) -> usize = black_box(|v, s| sum(v, s));
    assert_eq!(closure(values, text), direct);
    let huge = unsafe { std::slice::from_raw_parts(std::ptr::NonNull::<()>::dangling().as_ptr(), (1usize << 40) + 19) };
    assert_eq!(black_box(count_empty as fn(&[()]) -> usize)(huge), huge.len());
    let mut data = [1u32, 2, 3, 4];
    let write: fn(&mut [u32]) = black_box(|v| { v[1] = 27; });
    write(&mut data[1..]);
    assert_eq!(data, [1, 2, 27, 4]);
}

#[inline(never)]
fn scalar_call<T>(value: &mut u64, step: &u64, _: std::marker::PhantomData<T>) {
    *value += *step;
}

fn scalar_locations() {
    let mut value = black_box(17u64);
    let step = black_box(23u64);
    scalar_call(&mut value, &step, std::marker::PhantomData::<u8>);
    let indirect: fn(&mut u64, &u64, std::marker::PhantomData<u8>) = black_box(scalar_call::<u8>);
    indirect(&mut value, &step, std::marker::PhantomData);
    assert_eq!(value, 63);
    let pointer = &mut value as *mut u64;
    unsafe {
        let bytes = pointer.cast::<u8>();
        bytes.add(3).write(0xab);
        assert_eq!(*pointer, 0xab00003f);
        assert_eq!(bytes.add(3).wrapping_sub(3).cast::<u64>(), pointer);
    }
    let mut float = black_box(-0.0f64);
    let update: fn(&mut f64) = black_box(|x| *x = f64::from_bits(0x7ff8_0000_0000_1234));
    update(&mut float);
    assert_eq!(float.to_bits(), 0x7ff8_0000_0000_1234);
}
