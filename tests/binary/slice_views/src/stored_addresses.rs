use std::hint::black_box;

#[derive(Clone, Copy)]
#[repr(C)]
struct Cursor { current: *const u32, end: *const u32 }

#[inline(never)]
fn step(cursor: &mut Cursor) -> Option<u32> {
    if cursor.current == cursor.end { return None }
    unsafe {
        let value = *cursor.current;
        cursor.current = cursor.current.add(1);
        Some(value)
    }
}

pub fn run() {
    aggregate_ranges();
    let data = black_box([3, 5, 8, 13, 21]);
    let start = data.as_ptr();
    let mut cursor = Cursor { current: start, end: unsafe { start.add(data.len()) } };
    let copy = black_box(cursor);
    let mut sum = 0;
    while let Some(value) = step(&mut cursor) { sum += value; }
    assert_eq!(sum, 50);
    assert_eq!(copy.current, start);
    assert_eq!(unsafe { cursor.current.offset_from(copy.current) }, data.len() as isize);
    let mut words = [start, unsafe { start.add(1) }];
    let aliases: &mut [*const u32; 2] = unsafe { &mut *(&mut cursor as *mut Cursor).cast() };
    aliases.copy_from_slice(&words);
    assert_eq!(step(&mut cursor), Some(3));
    assert_eq!(step(&mut cursor), None);
    words[0] = unsafe { start.add(2) };
    let rebuilt: Cursor = unsafe { std::mem::transmute(words) };
    assert_eq!(unsafe { *rebuilt.current }, 8);
    let closure = { let stored = rebuilt.current; move || unsafe { *stored } };
    assert_eq!(black_box(closure)(), 8);
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
struct Pixel { r: u8, g: u8, b: u8, a: u8 }

#[inline(never)]
unsafe fn pixel_read(base: *const Pixel, index: usize) -> Pixel {
    unsafe { base.add(index).read() }
}

#[inline(never)]
unsafe fn pixel_write(base: *mut Pixel, index: usize, pixel: Pixel) {
    unsafe { base.add(index).write(pixel) }
}

fn aggregate_ranges() {
    let first = Pixel { r: 3, g: 5, b: 7, a: 11 };
    let mut pixels = vec![first; 12];
    let root = black_box(pixels.as_mut_ptr());
    unsafe {
        let field = std::ptr::addr_of_mut!((*root.add(4)).g);
        let mut copy = pixel_read(root, 4);
        copy.r = 17;
        copy.g = 19;
        assert_eq!(pixel_read(root, 4), first);
        pixel_write(root, 4, copy);
        assert_eq!(*field, 19);
        *field = 23;
        assert_eq!(pixel_read(root, 4).g, 23);
        assert_eq!(*root.cast::<u8>().add(16), 17);
        assert_eq!(pixel_read(root, 3), first);
        assert_eq!(pixel_read(root, 5), first);
    }
}
