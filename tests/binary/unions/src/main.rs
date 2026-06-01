#[derive(Copy, Clone)]
#[repr(C)]
struct TwoGroupsOfOneByte {
    a: u8,
    b: u8,
}

#[derive(Copy, Clone)]
#[repr(C)]
struct OneGroupOfTwoBytes {
    c: u16,
}

#[derive(Copy, Clone)]
#[repr(C)]
struct FourBytes {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
}

union Bytes {
    group1: TwoGroupsOfOneByte,
    group2: OneGroupOfTwoBytes,
}

union ScalarBytes {
    bytes: FourBytes,
    unsigned: u32,
    signed: i32,
    float: f32,
}

fn main() {
    let bytes = Bytes {
        group1: TwoGroupsOfOneByte { a: 0x01, b: 0x02 },
    };

    unsafe {
        assert!(bytes.group1.a == 0x01);
        assert!(bytes.group1.b == 0x02);
        assert!(bytes.group2.c == 0x0201);

        let word = ScalarBytes {
            unsigned: 0x44332211,
        };
        assert!(word.bytes.a == 0x11);
        assert!(word.bytes.b == 0x22);
        assert!(word.bytes.c == 0x33);
        assert!(word.bytes.d == 0x44);
        assert!(word.signed == 0x44332211);

        let negative = ScalarBytes { signed: -2 };
        assert!(negative.unsigned == 0xffff_fffe);

        let one = ScalarBytes { float: 1.0 };
        assert!(one.unsigned == 0x3f80_0000);

        let mut mutable = ScalarBytes { unsigned: 0 };
        mutable.bytes = FourBytes {
            a: 0xaa,
            b: 0xbb,
            c: 0xcc,
            d: 0x7d,
        };
        assert!(mutable.unsigned == 0x7dcc_bbaa);
    }
}
