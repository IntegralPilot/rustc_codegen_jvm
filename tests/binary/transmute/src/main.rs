use core::mem::{size_of, transmute};

#[repr(u8)]
enum Sparse {
    One = 1,
    Eight = 8,
    High = 128,
}

#[repr(transparent)]
struct TransparentSparse(Sparse);

#[repr(transparent)]
struct NestedTransparent(TransparentSparse);

#[repr(i16)]
enum SignedDiscriminant {
    Negative = -7,
    Positive = 300,
}

#[repr(C)]
struct Pair {
    first: u16,
    second: u16,
}

#[repr(C)]
struct Padded {
    first: u8,
    middle: u16,
    last: u8,
}

#[repr(C, u8)]
enum Payload {
    Number(u16),
    Bytes(u8, u8),
}

#[repr(C)]
union WordBytes {
    word: u32,
    bytes: [u8; 4],
}

struct Marker;

#[repr(transparent)]
struct ByteSlice([u8]);

#[repr(transparent)]
struct OuterByteSlice(ByteSlice);

impl ByteSlice {
    #[inline(never)]
    fn first(&self) -> u8 {
        self.0[0]
    }

    #[inline(never)]
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl OuterByteSlice {
    #[inline(never)]
    fn first(&self) -> u8 {
        self.0.first()
    }

    #[inline(never)]
    fn len(&self) -> usize {
        self.0.len()
    }
}

fn test_scalar_bits() {
    let truth: bool = unsafe { transmute(1u8) };
    assert!(truth);
    let truth_bits: u8 = unsafe { transmute(truth) };
    assert!(truth_bits == 1);

    let lambda: char = unsafe { transmute(0x03bbu32) };
    assert!(lambda == 'λ');
    let lambda_bits: u32 = unsafe { transmute(lambda) };
    assert!(lambda_bits == 0x03bb);
}

fn test_float_bits() {
    let nan_bits = 0x7fc1_2345u32;
    let nan: f32 = unsafe { transmute(nan_bits) };
    let round_trip: u32 = unsafe { transmute(nan) };
    assert!(round_trip == nan_bits);

    let pi_bits = 0x4009_21fb_5444_2d18i64;
    let pi: f64 = unsafe { transmute(pi_bits) };
    assert!(pi > 3.141 && pi < 3.142);
    let round_trip: i64 = unsafe { transmute(pi) };
    assert!(round_trip == pi_bits);
}

fn test_fieldless_enums_and_transparent_wrappers() {
    let sparse: Sparse = unsafe { transmute(128u8) };
    assert!(matches!(sparse, Sparse::High));
    let sparse_bits: u8 = unsafe { transmute(sparse) };
    assert!(sparse_bits == 128);

    let wrapped: TransparentSparse = unsafe { transmute(8u8) };
    assert!(matches!(wrapped.0, Sparse::Eight));
    let wrapped_bits: u8 = unsafe { transmute(wrapped) };
    assert!(wrapped_bits == 8);

    let nested: NestedTransparent = unsafe { transmute(1u8) };
    assert!(matches!((nested.0).0, Sparse::One));
    let nested_bits: u8 = unsafe { transmute(nested) };
    assert!(nested_bits == 1);

    let signed: SignedDiscriminant = unsafe { transmute(-7i16) };
    assert!(matches!(signed, SignedDiscriminant::Negative));
    let signed_bits: i16 = unsafe { transmute(signed) };
    assert!(signed_bits == -7);

    let positive: SignedDiscriminant = unsafe { transmute(300i16) };
    assert!(matches!(positive, SignedDiscriminant::Positive));
}

fn test_arrays_tuples_and_structs() {
    let bytes = [0x78u8, 0x56, 0x34, 0x12];
    let word: u32 = unsafe { transmute(bytes) };
    assert!(word == 0x1234_5678);
    let round_trip: [u8; 4] = unsafe { transmute(word) };
    assert!(round_trip[0] == 0x78);
    assert!(round_trip[1] == 0x56);
    assert!(round_trip[2] == 0x34);
    assert!(round_trip[3] == 0x12);

    let pair: Pair = unsafe { transmute((0x1234u16, 0x5678u16)) };
    assert!(pair.first == 0x1234);
    assert!(pair.second == 0x5678);
    let tuple: (u16, u16) = unsafe { transmute(pair) };
    assert!(tuple.0 == 0x1234);
    assert!(tuple.1 == 0x5678);

    let padded = Padded {
        first: 3,
        middle: 0x4567,
        last: 9,
    };
    let encoded: [u8; size_of::<Padded>()] = unsafe { transmute(padded) };
    let decoded: Padded = unsafe { transmute(encoded) };
    assert!(decoded.first == 3);
    assert!(decoded.middle == 0x4567);
    assert!(decoded.last == 9);
}

fn test_data_carrying_enums() {
    let number = Payload::Number(0x3456);
    let number_bytes: [u8; size_of::<Payload>()] = unsafe { transmute(number) };
    let number_round_trip: Payload = unsafe { transmute(number_bytes) };
    match number_round_trip {
        Payload::Number(value) => assert!(value == 0x3456),
        Payload::Bytes(_, _) => panic!(),
    }

    let bytes = Payload::Bytes(17, 29);
    let encoded: [u8; size_of::<Payload>()] = unsafe { transmute(bytes) };
    let decoded: Payload = unsafe { transmute(encoded) };
    match decoded {
        Payload::Bytes(first, second) => {
            assert!(first == 17);
            assert!(second == 29);
        }
        Payload::Number(_) => panic!(),
    }
}

fn test_union_storage_round_trips() {
    let from_bytes = WordBytes {
        bytes: [0x44, 0x33, 0x22, 0x11],
    };
    let word: u32 = unsafe { transmute(from_bytes) };
    assert!(word == 0x1122_3344);

    let from_word: WordBytes = unsafe { transmute(0x89ab_cdefu32) };
    let bytes: [u8; 4] = unsafe { transmute(from_word) };
    assert!(bytes[0] == 0xef);
    assert!(bytes[1] == 0xcd);
    assert!(bytes[2] == 0xab);
    assert!(bytes[3] == 0x89);
}

fn test_zero_sized_values() {
    let marker: Marker = unsafe { transmute(()) };
    let unit: () = unsafe { transmute(marker) };
    assert!(unit == ());
}

fn test_unsized_transparent_reference_transmute() {
    let bytes = [11_u8, 22, 33, 44];
    let wrapped: &ByteSlice = unsafe { transmute::<&[u8], &ByteSlice>(&bytes) };
    assert!(wrapped.len() == 4);
    assert!(wrapped.first() == 11);

    let outer = unsafe { &*(wrapped as *const ByteSlice as *const OuterByteSlice) };
    assert!(outer.len() == 4);
    assert!(outer.first() == 11);

    let empty: &[u8] = &[];
    let empty_wrapped: &ByteSlice = unsafe { transmute(empty) };
    assert!(empty_wrapped.len() == 0);
}

fn main() {
    test_scalar_bits();
    test_float_bits();
    test_fieldless_enums_and_transparent_wrappers();
    test_arrays_tuples_and_structs();
    test_data_carrying_enums();
    test_union_storage_round_trips();
    test_zero_sized_values();
    test_unsized_transparent_reference_transmute();
}
