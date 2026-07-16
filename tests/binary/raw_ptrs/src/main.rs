#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]
#![feature(f16, f128, ptr_internals, ptr_metadata, set_ptr_value)]

include!("../../../support/test_prelude.rs");

fn basic_raw_pointer_round_trip() {
    let mut value = 41_i32;
    let pointer = &mut value as *mut i32;

    unsafe {
        assert!(*pointer == 41);
        *pointer = 42;
    }

    assert!(value == 42);

    unsafe {
        let previous = pointer.read();
        pointer.write(previous + 1);
    }
    assert!(value == 43);
}

fn array_pointer_arithmetic() {
    let mut values = [10_i32, 20, 30, 40, 50];
    let base = values.as_mut_ptr();

    unsafe {
        assert!(*base == 10);
        assert!(*base.add(2) == 30);
        *base.add(1) = 22;
        *base.offset(3) = 44;
        assert!(*base.add(4).sub(2) == 30);
        assert!(base.add(5).offset_from(base) == 5);
    }

    assert!(values[0] == 10);
    assert!(values[1] == 22);
    assert!(values[2] == 30);
    assert!(values[3] == 44);
    assert!(values[4] == 50);
}

fn pointer_identity_and_casts() {
    let value = 7_i32;
    let first = &value as *const i32;
    let alias = first;
    let other_value = 7_i32;
    let other = &other_value as *const i32;

    assert!(first == alias);
    assert!(first != other);

    let mut mutable = 9_i32;
    let mutable_pointer = &mut mutable as *mut i32;
    let const_pointer = mutable_pointer as *const i32;
    unsafe {
        assert!(*const_pointer == 9);
        *mutable_pointer = 11;
    }
    assert!(mutable == 11);

    let exposed_address = mutable_pointer as usize;
    let restored_pointer = exposed_address as *mut i32;
    unsafe {
        *restored_pointer = 13;
    }
    assert!(mutable == 13);
}

struct Pair {
    left: i32,
    right: i32,
}

#[repr(C)]
struct PackedWords {
    low: u16,
    high: u16,
}

#[repr(u32)]
enum WireState {
    First = 0x1122_3344,
    Second = 0x5566_7788,
}

#[repr(C, u8)]
enum PayloadState {
    First(u32),
    Second(u32),
}

#[repr(C)]
struct PointerHolder {
    tag: u32,
    pointer: *mut i32,
}

#[repr(C)]
struct StringPointerHolder {
    text: &'static str,
    marker: u32,
}

#[repr(C)]
struct WideAggregate {
    value: u128,
    marker: u32,
}

#[repr(C)]
union WordBytes {
    word: u32,
    bytes: [u8; 4],
}

struct ZeroSized;

trait RawDynValue {
    fn get(&self) -> i32;
    fn set(&mut self, value: i32);
}

struct DynValue {
    value: i32,
}

impl RawDynValue for DynValue {
    fn get(&self) -> i32 {
        self.value
    }

    fn set(&mut self, value: i32) {
        self.value = value;
    }
}

fn advance_one_u32(address: usize) -> usize {
    address + 4
}

fn add_through_reference(value: &mut i32, amount: i32) {
    *value += amount;
}

fn read_through_reference(value: &i32) -> i32 {
    *value
}

fn projected_places_and_reborrows() {
    let mut pair = Pair {
        left: 12,
        right: 30,
    };
    let right = core::ptr::addr_of_mut!(pair.right);
    unsafe {
        *right += 12;
    }
    assert!(pair.left == 12);
    assert!(pair.right == 42);

    let mut value = 5_i32;
    let reference = &mut value;
    let raw = reference as *mut i32;
    let reborrow = unsafe { &mut *raw };
    *reborrow += 7;
    assert!(value == 12);
}

fn reference_identity() {
    let value = 17_i32;
    let first = &value;
    let second = &value;
    let alias = first;
    let equal_but_distinct = 17_i32;

    assert!(first as *const i32 == second as *const i32);
    assert!(first as *const i32 == alias as *const i32);
    assert!(first as *const i32 != &equal_but_distinct as *const i32);

    let mut across_call = 30_i32;
    add_through_reference(&mut across_call, 12);
    assert!(read_through_reference(&across_call) == 42);
}

fn nested_and_aggregate_pointers() {
    let mut first_value = 10_i32;
    let mut second_value = 20_i32;
    let mut active = &mut first_value as *mut i32;
    let active_pointer = &mut active as *mut *mut i32;

    unsafe {
        **active_pointer = 11;
        *active_pointer = &mut second_value;
        **active_pointer = 22;
    }
    assert!(first_value == 11);
    assert!(second_value == 22);

    // A pointer value is itself addressable memory: byte-level access must preserve its bits.
    let pointer_bytes = (&mut active as *mut *mut i32).cast::<u8>();
    unsafe {
        let low_byte = pointer_bytes.read();
        pointer_bytes.write(low_byte);
        **active_pointer = 23;
    }
    assert!(second_value == 23);

    let inners = [&mut first_value as *mut i32, &mut second_value as *mut i32];
    let outer = inners.as_ptr();

    unsafe {
        **outer += 10;
        **outer.add(1) += 20;
    }
    assert!(first_value == 21);
    assert!(second_value == 43);

    let mut pairs = [
        Pair { left: 1, right: 2 },
        Pair { left: 3, right: 4 },
        Pair { left: 5, right: 6 },
    ];
    let pairs_ptr = pairs.as_mut_ptr();
    unsafe {
        (*pairs_ptr.add(1)).right = 40;
        pairs_ptr.add(2).write(Pair {
            left: 20,
            right: 22,
        });
    }
    assert!(pairs[0].left == 1);
    assert!(pairs[1].right == 40);
    assert!(pairs[2].left + pairs[2].right == 42);
}

static STATIC_VALUE: i32 = 21;
static mut MUTABLE_STATIC_VALUE: i32 = 40;
static mut DROP_COUNT: u32 = 0;

struct NeedsDrop;

impl Drop for NeedsDrop {
    fn drop(&mut self) {
        unsafe {
            *core::ptr::addr_of_mut!(DROP_COUNT) += 1;
        }
    }
}

struct DropAmount {
    amount: u32,
}

impl Drop for DropAmount {
    fn drop(&mut self) {
        unsafe {
            *core::ptr::addr_of_mut!(DROP_COUNT) += self.amount;
        }
    }
}

struct DropContainer {
    first: DropAmount,
    second: DropAmount,
}

struct DropEnvelope {
    own_amount: u32,
    child: DropAmount,
}

enum DropChoice {
    Empty,
    One(DropAmount),
    Two(DropAmount, DropAmount),
}

impl Drop for DropEnvelope {
    fn drop(&mut self) {
        unsafe {
            *core::ptr::addr_of_mut!(DROP_COUNT) += self.own_amount;
        }
    }
}

fn static_addresses() {
    let first = core::ptr::addr_of!(STATIC_VALUE);
    let second = core::ptr::addr_of!(STATIC_VALUE);
    assert!(first == second);
    unsafe {
        assert!(*first == 21);
    }

    let mutable = core::ptr::addr_of_mut!(MUTABLE_STATIC_VALUE);
    unsafe {
        *mutable += 2;
        assert!(*core::ptr::addr_of!(MUTABLE_STATIC_VALUE) == 42);
    }
}

fn other_pointer_carriers() {
    let mut flags = [false, true, false];
    let flag_pointer = flags.as_mut_ptr();
    unsafe {
        *flag_pointer.add(2) = true;
    }
    assert!(flags[0] == false);
    assert!(flags[1]);
    assert!(flags[2]);

    let mut values = [1.5_f64, 2.5, 3.5];
    let value_pointer = values.as_mut_ptr();
    unsafe {
        *value_pointer.add(1) = 42.25;
    }
    assert!(values[0] == 1.5);
    assert!(values[1] == 42.25);
    assert!(values[2] == 3.5);
}

fn null_and_wrapping_arithmetic() {
    let null = 0_usize as *const i32;
    assert!(null.is_null());
    assert!(null as usize == 0);

    let values = [3_i32, 6, 9, 12];
    let base = values.as_ptr();
    let end = base.wrapping_add(4);
    let last = end.wrapping_sub(1);
    unsafe {
        assert!(*last == 12);
    }
}

fn same_width_pointer_reinterpretation() {
    let mut float = 1.0_f32;
    let bits = &mut float as *mut f32 as *mut i32;
    unsafe {
        assert!(*bits == 0x3f80_0000_i32);
        *bits = 0x4000_0000_i32;
    }
    assert!(float == 2.0);

    let mut double = 1.0_f64;
    let bits = &mut double as *mut f64 as *mut i64;
    unsafe {
        assert!(*bits == 0x3ff0_0000_0000_0000_i64);
        *bits = 0x4000_0000_0000_0000_i64;
    }
    assert!(double == 2.0);
}

fn f16_pointer_storage() {
    let mut value = 1.5_f16;
    let pointer = &mut value as *mut f16;
    unsafe {
        assert!(*pointer == 1.5_f16);
        let bits = pointer.cast::<u16>();
        assert!(*bits == 0x3e00);
        *bits = 0x4000;
    }
    assert!(value == 2.0_f16);
}

fn wide_scalar_pointer_storage() {
    let mut value64 = 0x1122_3344_5566_7788_u64;
    let bytes64 = (&mut value64 as *mut u64).cast::<u8>();
    unsafe {
        assert!(*bytes64 == 0x88);
        assert!(*bytes64.add(7) == 0x11);
        *bytes64.add(7) = 0xaa;
    }
    assert!(value64 == 0xaa22_3344_5566_7788_u64);

    let mut value128 = 0x1122_3344_5566_7788_99aa_bbcc_ddee_ff00_u128;
    let bytes128 = (&mut value128 as *mut u128).cast::<u8>();
    unsafe {
        assert!(*bytes128 == 0x00);
        assert!(*bytes128.add(15) == 0x11);
        *bytes128.add(1) = 0x42;
        *bytes128.add(15) = 0xfe;
    }
    assert!(value128 == 0xfe22_3344_5566_7788_99aa_bbcc_ddee_4200_u128);

    let mut signed = 0_i128;
    let signed_bytes = (&mut signed as *mut i128).cast::<u8>();
    unsafe {
        *signed_bytes.add(15) = 0x80;
    }
    assert!(signed == i128::MIN);

    const TWO_BITS: u128 = 0x4000_0000_0000_0000_0000_0000_0000_0000;
    let mut wide_float = 1.0_f128;
    let float_bits = (&mut wide_float as *mut f128).cast::<u128>();
    unsafe {
        assert!(*float_bits == 1.0_f128.to_bits());
        *float_bits = TWO_BITS;
    }
    assert!(wide_float == 2.0_f128);
    assert!(wide_float.to_bits() == TWO_BITS);
}

fn byte_addressing_and_integer_addresses() {
    let mut words = [0x1122_3344_u32, 0x5566_7788_u32];
    let words_ptr = words.as_mut_ptr();
    let bytes = words_ptr.cast::<u8>();

    unsafe {
        assert!(*bytes.add(0) == 0x44);
        assert!(*bytes.add(1) == 0x33);
        assert!(*bytes.add(2) == 0x22);
        assert!(*bytes.add(3) == 0x11);
        assert!(*bytes.add(4) == 0x88);
        *bytes.add(1) = 0xaa;
    }
    assert!(words[0] == 0x1122_aa44);

    let second_from_bytes = unsafe { bytes.add(core::mem::size_of::<u32>()) }.cast::<u32>();
    unsafe {
        assert!(*second_from_bytes == 0x5566_7788);
    }

    let second_address = bytes as usize + core::mem::size_of::<u32>();
    let second_from_address = second_address as *const u32;
    unsafe {
        assert!(*second_from_address == 0x5566_7788);
    }

    let mut unsigned_bytes = [0_u8, 127, 128, 255];
    let unsigned_ptr = unsigned_bytes.as_mut_ptr();
    unsafe {
        assert!(*unsigned_ptr.add(2) == 128);
        *unsigned_ptr.add(1) = 200;
        assert!(*unsigned_ptr.add(1) == 200);
    }
    assert!(unsigned_bytes[1] == 200);
}

fn pointer_ordering() {
    let values = [10_i32, 20, 30];
    let first = values.as_ptr();
    let second = unsafe { first.add(1) };
    let end = unsafe { first.add(values.len()) };

    assert!(first < second);
    assert!(second <= second);
    assert!(end > second);
    assert!(end >= first);
}

unsafe fn raw_binary_search(
    values: *const u32,
    length: usize,
    target: u32,
) -> Option<usize> {
    let mut low = 0;
    let mut high = length;
    while low < high {
        let middle = low + (high - low) / 2;
        let value = unsafe { *values.add(middle) };
        if value < target {
            low = middle + 1;
        } else if value > target {
            high = middle;
        } else {
            return Some(middle);
        }
    }
    None
}

fn raw_pointer_binary_search() {
    let values = [1_u32, 3, 5, 8, 13, 21, 34];
    unsafe {
        assert!(raw_binary_search(values.as_ptr(), values.len(), 1) == Some(0));
        assert!(raw_binary_search(values.as_ptr(), values.len(), 13) == Some(4));
        assert!(raw_binary_search(values.as_ptr(), values.len(), 34) == Some(6));
        assert!(raw_binary_search(values.as_ptr(), values.len(), 4).is_none());
        assert!(raw_binary_search(values.as_ptr(), values.len(), 35).is_none());
    }
}

fn unaligned_volatile_and_bulk_memory() {
    let mut bytes = [0_u8; 12];
    let unaligned = unsafe { bytes.as_mut_ptr().add(1) }.cast::<u32>();
    unsafe {
        unaligned.write_unaligned(0x4433_2211);
        assert!(unaligned.read_unaligned() == 0x4433_2211);
    }
    assert!(bytes[1] == 0x11);
    assert!(bytes[2] == 0x22);
    assert!(bytes[3] == 0x33);
    assert!(bytes[4] == 0x44);

    let mut volatile_value = 10_i32;
    let volatile_pointer = &mut volatile_value as *mut i32;
    unsafe {
        volatile_pointer.write_volatile(42);
        assert!(volatile_pointer.read_volatile() == 42);
    }
    assert!(volatile_value == 42);

    let source = [1_u8, 2, 3, 4, 5, 6];
    let mut destination = [0_u8; 6];
    unsafe {
        core::ptr::copy_nonoverlapping(source.as_ptr(), destination.as_mut_ptr(), source.len());
    }
    assert!(destination[0] == 1);
    assert!(destination[1] == 2);
    assert!(destination[2] == 3);
    assert!(destination[3] == 4);
    assert!(destination[4] == 5);
    assert!(destination[5] == 6);

    unsafe {
        core::ptr::copy(destination.as_ptr(), destination.as_mut_ptr().add(1), 5);
    }
    assert!(destination[0] == 1);
    assert!(destination[1] == 1);
    assert!(destination[2] == 2);
    assert!(destination[3] == 3);
    assert!(destination[4] == 4);
    assert!(destination[5] == 5);

    unsafe {
        core::ptr::write_bytes(destination.as_mut_ptr(), 0xaa, destination.len());
    }
    assert!(destination[0] == 0xaa);
    assert!(destination[1] == 0xaa);
    assert!(destination[2] == 0xaa);
    assert!(destination[3] == 0xaa);
    assert!(destination[4] == 0xaa);
    assert!(destination[5] == 0xaa);
}

fn aggregate_memory_layout() {
    let mut words = PackedWords {
        low: 0x1122,
        high: 0x3344,
    };
    let words_pointer = &mut words as *mut PackedWords;
    let bytes = words_pointer.cast::<u8>();
    unsafe {
        assert!(*bytes.add(0) == 0x22);
        assert!(*bytes.add(1) == 0x11);
        assert!(*bytes.add(2) == 0x44);
        assert!(*bytes.add(3) == 0x33);
        *bytes.add(2) = 0xaa;
        *bytes.add(3) = 0xbb;
    }
    assert!(words.low == 0x1122);
    assert!(words.high == 0xbbaa);

    let mut array = [
        PackedWords { low: 1, high: 2 },
        PackedWords { low: 3, high: 4 },
    ];
    let array_bytes = array.as_mut_ptr().cast::<u8>();
    let second = unsafe { array_bytes.add(core::mem::size_of::<PackedWords>()) };
    unsafe {
        *second = 0x34;
        *second.add(1) = 0x12;
    }
    assert!(array[0].low == 1);
    assert!(array[1].low == 0x1234);
    assert!(array[1].high == 4);

    assert!(WireState::Second as u32 == 0x5566_7788);
    let mut state = WireState::First;
    let replacement = [0x88_u8, 0x77, 0x66, 0x55];
    unsafe {
        core::ptr::copy_nonoverlapping(
            replacement.as_ptr(),
            (&mut state as *mut WireState).cast::<u8>(),
            replacement.len(),
        );
    }
    assert!(matches!(state, WireState::Second));

    let mut payload_state = PayloadState::First(1);
    match &payload_state {
        PayloadState::First(value) => assert!(*value == 1),
        PayloadState::Second(_) => panic!(),
    }
    let replacement_state = PayloadState::Second(42);
    unsafe {
        core::ptr::copy_nonoverlapping(
            (&replacement_state as *const PayloadState).cast::<u8>(),
            (&mut payload_state as *mut PayloadState).cast::<u8>(),
            core::mem::size_of::<PayloadState>(),
        );
    }
    match payload_state {
        PayloadState::Second(value) => assert!(value == 42),
        PayloadState::First(_) => panic!(),
    }

    let mut first_target = 10_i32;
    let mut second_target = 20_i32;
    let mut first_holder = PointerHolder {
        tag: 1,
        pointer: &mut first_target,
    };
    let second_holder = PointerHolder {
        tag: 2,
        pointer: &mut second_target,
    };
    let first_bytes = (&mut first_holder as *mut PointerHolder).cast::<u8>();
    let second_bytes = (&second_holder as *const PointerHolder).cast::<u8>();
    unsafe {
        // repr(C) places the 8-byte-aligned pointer after four bytes of padding.
        core::ptr::copy_nonoverlapping(second_bytes.add(8), first_bytes.add(8), 8);
        *first_holder.pointer = 42;
    }
    assert!(first_target == 10);
    assert!(second_target == 42);
    assert!(first_holder.tag == 1);

    let mut wide = WideAggregate {
        value: 0x1122_3344_5566_7788_99aa_bbcc_ddee_ff00_u128,
        marker: 42,
    };
    let wide_bytes = (&mut wide as *mut WideAggregate).cast::<u8>();
    unsafe {
        assert!(*wide_bytes == 0x00);
        assert!(*wide_bytes.add(15) == 0x11);
        *wide_bytes.add(15) = 0xaa;
    }
    assert!(wide.value == 0xaa22_3344_5566_7788_99aa_bbcc_ddee_ff00_u128);
    assert!(wide.marker == 42);
}

fn managed_reference_fields_have_memory_bits() {
    let mut destination = StringPointerHolder {
        text: "destination",
        marker: 11,
    };
    let source = StringPointerHolder {
        text: "copied reference",
        marker: 22,
    };
    unsafe {
        core::ptr::copy_nonoverlapping(
            (&source as *const StringPointerHolder).cast::<u8>(),
            (&mut destination as *mut StringPointerHolder).cast::<u8>(),
            core::mem::size_of::<&'static str>(),
        );
    }
    assert!(destination.text == "copied reference");
    assert!(destination.marker == 11);

    let field = core::ptr::addr_of_mut!(destination.text);
    unsafe {
        field.write("field pointer update");
        assert!(*field == "field pointer update");
    }
    assert!(destination.text == "field pointer update");
}

fn projected_field_addresses_share_allocations() {
    let mut words = PackedWords {
        low: 0x1122,
        high: 0x3344,
    };
    let whole = &mut words as *mut PackedWords;
    let low = core::ptr::addr_of_mut!(words.low);
    let high = core::ptr::addr_of_mut!(words.high);
    assert!(whole.cast::<u16>() == low);
    assert!(high.addr() - low.addr() == 2);
    unsafe {
        *low.add(1) = 0x5566;
    }
    assert!(words.low == 0x1122);
    assert!(words.high == 0x5566);
    let restored = whole.addr() as *mut PackedWords;
    unsafe {
        (*restored).high = 0x7788;
    }
    assert!(words.high == 0x7788);

    let mut overlay = WordBytes { word: 0x1122_3344 };
    let word = core::ptr::addr_of_mut!(overlay.word);
    let bytes = core::ptr::addr_of_mut!(overlay.bytes).cast::<u8>();
    assert!(word.cast::<u8>() == bytes);
    unsafe {
        *bytes = 0xaa;
        assert!(*word == 0x1122_33aa);
    }
}

fn byte_methods_alignment_provenance_and_zsts() {
    let values = [0x1122_3344_u32, 0x5566_7788_u32];
    let first = values.as_ptr();
    let second = unsafe { first.add(1) };
    unsafe {
        assert!(first.byte_add(4) == second);
        assert!(second.byte_sub(4) == first);
        assert!(first.byte_offset(4) == second);
        assert!(second.wrapping_byte_sub(4) == first);
    }

    assert!(first.align_offset(core::mem::align_of::<u32>()) == 0);
    let misaligned = unsafe { first.cast::<u8>().add(1) };
    assert!(misaligned.align_offset(4) == 3);

    let first_address = first.addr();
    let second_from_address = first.with_addr(first_address + 4);
    assert!(second_from_address == second);
    assert!(first.expose_provenance() == first_address);
    assert!(core::ptr::null::<u32>().is_null());
    assert!(core::ptr::null_mut::<u32>().is_null());

    let mapped = first.map_addr(|address| address + 4);
    assert!(mapped == second);
    let captured_delta = 4;
    let captured_mapped = first.map_addr(|address| address + captured_delta);
    assert!(captured_mapped == second);
    assert!(first.map_addr(advance_one_u32) == second);
    let address_mapper: fn(usize) -> usize = advance_one_u32;
    assert!(first.map_addr(address_mapper) == second);
    let address_only = core::ptr::without_provenance::<u32>(first_address);
    assert!(address_only.addr() == first_address);
    assert!(address_only == first);
    assert!(address_only.with_addr(second.addr()) == second);
    let restored = core::ptr::with_exposed_provenance::<u32>(first.expose_provenance());
    unsafe {
        assert!(*restored == 0x1122_3344);
    }
    let (data, metadata) = first.to_raw_parts();
    assert!(data.addr() == first.addr());
    let rebuilt = core::ptr::from_raw_parts::<u32>(data, metadata);
    assert!(rebuilt == first);
    let metadata_from = first.cast::<u8>().with_metadata_of(first);
    assert!(metadata_from == first);
    let sized_metadata = core::ptr::metadata(first);
    assert!(sized_metadata == ());

    let zsts = [ZeroSized, ZeroSized, ZeroSized];
    let zst_pointer = zsts.as_ptr();
    assert!(unsafe { zst_pointer.add(1) } == zst_pointer);
    assert!(zst_pointer.wrapping_add(100) == zst_pointer);
}

fn stable_pointer_api_surface() {
    let mut values = [10_u32, 20, 30, 40];
    let base = values.as_mut_ptr();
    let end = unsafe { base.add(values.len()) };
    unsafe {
        assert!(end.offset_from_unsigned(base) == values.len());
        assert!(end.byte_offset_from(base) == 16);
        assert!(end.byte_offset_from_unsigned(base) == 16);
    }
    assert!(base.is_aligned());
    let shared = unsafe { base.as_ref() };
    assert!(shared.is_some());
    match shared {
        Some(value) => assert!(*value == 10),
        None => panic!(),
    }
    assert!(unsafe { core::ptr::null::<u32>().as_ref() }.is_none());
    match unsafe { base.as_mut() } {
        Some(value) => *value = 11,
        None => panic!(),
    }
    assert!(values[0] == 11);

    let by_ref = core::ptr::from_ref(&values[1]);
    assert!(by_ref == unsafe { base.add(1) });
    let by_mut = core::ptr::from_mut(&mut values[2]);
    unsafe {
        *by_mut = 31;
    }
    assert!(values[2] == 31);

    let mut copied = [0_u32; 4];
    unsafe {
        base.copy_to_nonoverlapping(copied.as_mut_ptr(), values.len());
    }
    assert!(copied[0] == values[0]);
    assert!(copied[1] == values[1]);
    assert!(copied[2] == values[2]);
    assert!(copied[3] == values[3]);
    let replacement = [1_u32, 2, 3, 4];
    unsafe {
        copied
            .as_mut_ptr()
            .copy_from_nonoverlapping(replacement.as_ptr(), replacement.len());
    }
    assert!(copied[0] == 1);
    assert!(copied[1] == 2);
    assert!(copied[2] == 3);
    assert!(copied[3] == 4);
    unsafe {
        copied.as_mut_ptr().copy_to(copied.as_mut_ptr().add(1), 3);
    }
    assert!(copied[0] == 1);
    assert!(copied[1] == 1);
    assert!(copied[2] == 2);
    assert!(copied[3] == 3);

    let previous = unsafe { copied.as_mut_ptr().replace(9) };
    assert!(previous == 1);
    assert!(copied[0] == 9);
    unsafe {
        copied.as_mut_ptr().swap(copied.as_mut_ptr().add(3));
    }
    assert!(copied[0] == 3);
    assert!(copied[1] == 1);
    assert!(copied[2] == 2);
    assert!(copied[3] == 9);
    unsafe {
        core::ptr::swap_nonoverlapping(copied.as_mut_ptr(), copied.as_mut_ptr().add(2), 2);
    }
    assert!(copied[0] == 2);
    assert!(copied[1] == 9);
    assert!(copied[2] == 3);
    assert!(copied[3] == 1);

    let mut bytes = [0_u16; 3];
    unsafe {
        bytes.as_mut_ptr().write_bytes(0x5a, bytes.len());
    }
    assert!(bytes[0] == 0x5a5a);
    assert!(bytes[1] == 0x5a5a);
    assert!(bytes[2] == 0x5a5a);

    assert!(core::ptr::addr_eq(base, base.cast::<u8>()));
    assert!(core::ptr::eq(base, base));
    assert!(!core::ptr::eq(base, unsafe { base.add(1) }));
    let dangling = core::ptr::dangling::<u32>();
    let dangling_mut = core::ptr::dangling_mut::<u32>();
    assert!(!dangling.is_null());
    assert!(dangling.is_aligned());
    assert!(dangling.addr() == dangling_mut.addr());
}

fn raw_slice_metadata() {
    let values = [3_i32, 6, 9, 12];
    let raw = core::ptr::slice_from_raw_parts(values.as_ptr(), values.len());
    assert!(core::ptr::metadata(raw) == 4);
    let slice = unsafe { &*raw };
    assert!(slice.len() == 4);
    assert!(slice[0] == 3);
    assert!(slice[3] == 12);

    let data = raw.cast::<()>();
    let rebuilt = core::ptr::from_raw_parts::<[i32]>(data, core::ptr::metadata(raw));
    let rebuilt_slice = unsafe { &*rebuilt };
    assert!(rebuilt_slice[1] == 6);
    assert!(rebuilt_slice[2] == 9);
}

fn rebuild_sized_pointer<T>(pointer: *const T) -> *const T {
    let (data, metadata) = pointer.to_raw_parts();
    core::ptr::from_raw_parts::<T>(data, metadata)
}

fn generic_sized_raw_pointer_metadata() {
    let word = 0x1234_5678_u32;
    let rebuilt_word = rebuild_sized_pointer(&word);
    assert!(unsafe { *rebuilt_word } == word);

    let pair = Pair { left: 17, right: 29 };
    let rebuilt_pair = rebuild_sized_pointer(&pair);
    assert!(unsafe { (*rebuilt_pair).left } == 17);
    assert!(unsafe { (*rebuilt_pair).right } == 29);
}

fn raw_str_metadata() {
    let text = "raw UTF-8 pointer";
    let raw = text as *const str;
    assert!(core::ptr::metadata(raw) == text.len());

    let data = raw.cast::<()>();
    let rebuilt = core::ptr::from_raw_parts::<str>(data, text.len());
    let rebuilt_text = unsafe { &*rebuilt };
    assert!(rebuilt_text == text);
}

fn pointer_backed_raw_slices() {
    let mut scalar = 41_u32;
    let shared = unsafe { core::slice::from_raw_parts(&scalar, 1) };
    assert!(shared.len() == 1);
    assert!(shared[0] == 41);

    let mutable = unsafe { core::slice::from_raw_parts_mut(&mut scalar, 1) };
    mutable[0] = 42;
    assert!(scalar == 42);

    let mut values = [5_u32, 10, 15, 20];
    let middle = unsafe { core::slice::from_raw_parts_mut(values.as_mut_ptr().add(1), 2) };
    assert!(middle[0] == 10);
    assert!(middle[1] == 15);
    middle[1] = 16;
    assert!(values[2] == 16);

    let mut pair = Pair { left: 20, right: 21 };
    let pair_slice = unsafe { core::slice::from_raw_parts_mut(&mut pair, 1) };
    pair_slice[0].right = 22;
    assert!(pair.left + pair.right == 42);
}

fn raw_trait_object_metadata() {
    let mut value = DynValue { value: 40 };
    let raw: *mut dyn RawDynValue = &mut value;
    unsafe {
        assert!((*raw).get() == 40);
        (*raw).set(41);
    }

    let metadata = core::ptr::metadata(raw);
    let data = raw.cast::<()>();
    let rebuilt = core::ptr::from_raw_parts_mut::<dyn RawDynValue>(data, metadata);
    unsafe {
        (*rebuilt).set(42);
        assert!((*rebuilt).get() == 42);
    }
    let (data_again, metadata_again) = raw.to_raw_parts();
    let rebuilt_again =
        core::ptr::from_raw_parts_mut::<dyn RawDynValue>(data_again, metadata_again);
    unsafe {
        assert!((*rebuilt_again).get() == 42);
    }
    assert!(value.value == 42);
}

fn replace_and_swap() {
    let mut value = 10_i32;
    let old = unsafe { core::ptr::replace(&mut value, 42) };
    assert!(old == 10);
    assert!(value == 42);

    let mut left = Pair { left: 1, right: 2 };
    let mut right = Pair {
        left: 20,
        right: 22,
    };
    unsafe {
        core::ptr::swap(&mut left, &mut right);
    }
    assert!(left.left == 20);
    assert!(left.right == 22);
    assert!(right.left == 1);
    assert!(right.right == 2);
}

fn raw_drop_in_place() {
    let mut value = NeedsDrop;
    let pointer = &mut value as *mut NeedsDrop;
    unsafe {
        core::ptr::drop_in_place(pointer);
    }
    core::mem::forget(value);
    unsafe {
        assert!(*core::ptr::addr_of!(DROP_COUNT) == 1);
    }

    let mut container = DropContainer {
        first: DropAmount { amount: 2 },
        second: DropAmount { amount: 3 },
    };
    assert!(container.first.amount + container.second.amount == 5);
    unsafe {
        core::ptr::drop_in_place(&mut container);
    }
    core::mem::forget(container);
    unsafe {
        assert!(*core::ptr::addr_of!(DROP_COUNT) == 6);
    }

    let mut choice = DropChoice::Two(
        DropAmount { amount: 6 },
        DropAmount { amount: 7 },
    );
    match &choice {
        DropChoice::Two(first, second) => assert!(first.amount + second.amount == 13),
        _ => panic!(),
    }
    unsafe {
        core::ptr::drop_in_place(&mut choice);
    }
    core::mem::forget(choice);
    unsafe {
        assert!(*core::ptr::addr_of!(DROP_COUNT) == 19);
    }
}

fn automatic_recursive_drop_glue() {
    let before = unsafe { *core::ptr::addr_of!(DROP_COUNT) };
    {
        let _value = DropEnvelope {
            own_amount: 4,
            child: DropAmount { amount: 5 },
        };
        let _choice = DropChoice::One(DropAmount { amount: 6 });
        let _empty = DropChoice::Empty;
        let _option = Some(DropAmount { amount: 8 });
        assert!(_value.child.amount == 5);
        match &_choice {
            DropChoice::One(value) => assert!(value.amount == 6),
            _ => panic!(),
        }
    }
    unsafe {
        assert!(*core::ptr::addr_of!(DROP_COUNT) == before + 23);
    }
}

fn pointer_niche_layouts() {
    use core::ptr::NonNull;

    assert_eq!(
        core::mem::size_of::<Option<NonNull<i32>>>(),
        core::mem::size_of::<*mut i32>()
    );

    let opt_none: Option<NonNull<i32>> = None;
    let raw_none = unsafe { core::mem::transmute::<Option<NonNull<i32>>, *mut i32>(opt_none) };
    assert!(raw_none.is_null());
}

#[repr(C)]
struct CustomDst {
    header: u32,
    slice: [u8],
}

fn custom_dst_projections() {
    let mut storage = (42_u32, [1_u8, 2, 3, 4]);
    let raw_dst = core::ptr::slice_from_raw_parts_mut(&mut storage as *mut _ as *mut u8, 4)
        as *mut CustomDst;

    unsafe {
        assert_eq!((*raw_dst).header, 42);
        assert_eq!((*raw_dst).slice[2], 3);

        assert_eq!(core::ptr::metadata(raw_dst), 4);
    }
}

#[inline(never)]
unsafe fn aliasing_test(ref_val: &mut i32, raw_ptr: *mut i32) {
    *ref_val = 10;
    *raw_ptr = 20;
    assert_eq!(*ref_val, 20);
}

fn verify_noalias_handling() {
    let mut val = 0_i32;
    let raw = &mut val as *mut i32;
    unsafe {
        aliasing_test(&mut val, raw);
    }
}

trait Inspector {
    fn inspect(&self) -> i32;
}

struct Item {
    value: i32,
}

impl Inspector for Item {
    fn inspect(&self) -> i32 {
        self.value
    }
}

#[inline(never)]
fn pass_trait_object(ptr: *const dyn Inspector) -> *const dyn Inspector {
    ptr
}

#[inline(never)]
fn pass_slice_pointer(ptr: *const [i32]) -> *const [i32] {
    ptr
}

fn fat_pointer_abi_boundaries() {
    let mut item = Item { value: 1234 };
    let raw_trait: *const dyn Inspector = &item;

    let returned_trait = pass_trait_object(raw_trait);
    unsafe {
        assert_eq!((*returned_trait).inspect(), 1234);
        assert_eq!(core::ptr::metadata(raw_trait), core::ptr::metadata(returned_trait));
    }

    let array = [10, 20, 30, 40];
    let raw_slice: *const [i32] = &array;
    let returned_slice = pass_slice_pointer(raw_slice);
    unsafe {
        assert_eq!((&(*returned_slice)).len(), 4);
        assert_eq!((*returned_slice)[3], 40);
    }
}

#[repr(align(128))]
struct AlignedData {
    value: u32,
    payload: [u8; 124],
}

#[inline(never)]
fn allocate_on_stack() -> usize {
    let data = AlignedData { value: 42, payload: [0; 124] };
    let ptr = &data as *const AlignedData;
    ptr as usize
}

fn stack_realignment() {
    let addr = allocate_on_stack();
    assert_eq!(addr % 128, 0);
}

struct Empty;

#[repr(C)]
struct MixedStruct {
    first: u16,
    empty_middle: Empty,
    second: u32,
    empty_end: Empty,
    third: u64,
}

fn zst_and_padding_offsets() {
    let mut data = MixedStruct {
        first: 0x1122,
        empty_middle: Empty,
        second: 0x55667788,
        empty_end: Empty,
        third: 0x99AABBCCDDEEFF00,
    };

    let base = &mut data as *mut MixedStruct;
    unsafe {
        let first_ptr = core::ptr::addr_of!(data.first);
        let second_ptr = core::ptr::addr_of!(data.second);
        let third_ptr = core::ptr::addr_of!(data.third);

        let offset_second = (second_ptr as usize) - (first_ptr as usize);
        let offset_third = (third_ptr as usize) - (first_ptr as usize);

        assert_eq!(offset_second, 4);
        assert_eq!(offset_third, 8);

        assert_eq!(*second_ptr, 0x55667788);
        assert_eq!(*third_ptr, 0x99AABBCCDDEEFF00);
    }
}

#[repr(u8)]
enum CustomEnum {
    Alpha(u32) = 1,
    Beta(u32) = 2,
}

fn enum_discriminant_pointer_manipulation() {
    let mut val = CustomEnum::Alpha(100);
    let ptr = &mut val as *mut CustomEnum;

    unsafe {
        let tag_ptr = ptr as *mut u8;
        assert_eq!(*tag_ptr, 1);

        *tag_ptr = 2;

        let payload_ptr = (ptr as *mut u8).add(4) as *mut u32;
        *payload_ptr = 200;
    }

    match val {
        CustomEnum::Beta(payload) => assert_eq!(payload, 200),
        CustomEnum::Alpha(_) => panic!("Failed to change variant via raw pointer"),
    }
}

fn multidimensional_pointer_flat_map() {
    let mut matrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
    ];

    let base_ptr = matrix.as_mut_ptr() as *mut i32;
    unsafe {
        let row = 2;
        let col = 1;
        let width = 3;
        let element_ptr = base_ptr.add(row * width + col);
        assert_eq!(*element_ptr, 8);

        *element_ptr = 88;
    }
    assert_eq!(matrix[2][1], 88);
}

#[repr(packed)]
struct PackedDataset {
    tag: u8,
    data_u64: u64,
    data_u32: u32,
}

fn packed_unaligned_fields() {
    let dataset = PackedDataset {
        tag: 0xAA,
        data_u64: 0x1122334455667788,
        data_u32: 0x99AABBCC,
    };

    let base = &dataset as *const PackedDataset;
    unsafe {
        let u64_ptr = core::ptr::addr_of!(dataset.data_u64);
        let u32_ptr = core::ptr::addr_of!(dataset.data_u32);

        assert_eq!(u64_ptr as usize - base as usize, 1);
        assert_eq!(u32_ptr as usize - base as usize, 9);

        assert_eq!(u64_ptr.read_unaligned(), 0x1122334455667788);
        assert_eq!(u32_ptr.read_unaligned(), 0x99AABBCC);
    }
}

fn sized_to_unsized_coercion() {
    let mut data = [100_i32, 200, 300, 400];
    let sized_ptr: *mut [i32; 4] = &mut data;

    let unsized_ptr: *mut [i32] = sized_ptr;

    unsafe {
        assert!(core::ptr::metadata(unsized_ptr) == 4);

        assert!((*unsized_ptr)[0] == 100);
        assert!( (*unsized_ptr)[3] == 400);

        (*unsized_ptr)[2] = 999;
    }
    assert!(data[2] == 999);
}

fn pointer_wrapper_unsized_coercions() {
    use core::ptr::{NonNull, Unique};

    let mut data = [10_u8, 20, 30, 40];
    let non_null_array = unsafe { NonNull::new_unchecked(&mut data as *mut [u8; 4]) };
    let non_null_slice: NonNull<[u8]> = non_null_array;
    assert!(core::ptr::metadata(non_null_slice.as_ptr()) == 4);

    let mut empty = [];
    let unique_array = unsafe { Unique::new_unchecked(&mut empty as *mut [u8; 0]) };
    let unique_slice: Unique<[u8]> = unique_array;
    assert!(core::ptr::metadata(unique_slice.as_ptr()) == 0);
}

#[repr(C)]
#[derive(Copy, Clone)]
struct SegmentA {
    low: u16,
    high: u16,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct SegmentB {
    full: u32,
}

#[repr(C)]
#[derive(Copy, Clone)]
union NestedUnion {
    parts: SegmentA,
    whole: SegmentB,
}

fn nested_union_reinterpretation() {
    assert!(core::mem::size_of::<NestedUnion>() == 4);

    let mut u = NestedUnion {
        parts: SegmentA { low: 0x4433, high: 0x2211 }
    };

    unsafe {
        assert!(u.whole.full == 0x22114433);

        u.whole.full = 0xAABBCCDD;

        assert!(u.parts.low == 0xCCDD);
        assert!(u.parts.high == 0xAABB);
    }
}

fn main() {
    basic_raw_pointer_round_trip();
    array_pointer_arithmetic();
    pointer_identity_and_casts();
    projected_places_and_reborrows();
    reference_identity();
    nested_and_aggregate_pointers();
    static_addresses();
    other_pointer_carriers();
    null_and_wrapping_arithmetic();
    same_width_pointer_reinterpretation();
    f16_pointer_storage();
    wide_scalar_pointer_storage();
    byte_addressing_and_integer_addresses();
    pointer_ordering();
    raw_pointer_binary_search();
    unaligned_volatile_and_bulk_memory();
    aggregate_memory_layout();
    managed_reference_fields_have_memory_bits();
    projected_field_addresses_share_allocations();
    byte_methods_alignment_provenance_and_zsts();
    stable_pointer_api_surface();
    raw_slice_metadata();
    generic_sized_raw_pointer_metadata();
    raw_str_metadata();
    pointer_backed_raw_slices();
    raw_trait_object_metadata();
    replace_and_swap();
    raw_drop_in_place();
    automatic_recursive_drop_glue();
    pointer_niche_layouts();
    custom_dst_projections();
    verify_noalias_handling();
    fat_pointer_abi_boundaries();
    stack_realignment();
    zst_and_padding_offsets();
    enum_discriminant_pointer_manipulation();
    multidimensional_pointer_flat_map();
    packed_unaligned_fields();
    sized_to_unsized_coercion();
    pointer_wrapper_unsized_coercions();
    nested_union_reinterpretation();
}
