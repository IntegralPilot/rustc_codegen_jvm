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

#[derive(Copy, Clone)]
#[repr(C)]
struct CharacterAndUnit {
    character: char,
    unit: (),
}

union CharacterBytes {
    character: char,
    code_point: u32,
    bytes: FourBytes,
    character_and_unit: CharacterAndUnit,
    unit: (),
}

#[derive(Copy, Clone)]
enum DefaultDiscriminant {
    Zero,
    Three = 3,
    Four,
}

union DefaultEnumByte {
    value: DefaultDiscriminant,
    byte: u8,
}

#[derive(Copy, Clone)]
#[repr(i16)]
enum SignedDiscriminant {
    Negative = -2,
    Seven = 7,
    Eight,
}

union SignedEnumBytes {
    value: SignedDiscriminant,
    signed: i16,
    unsigned: u16,
    bytes: TwoGroupsOfOneByte,
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum EscapeCharacter {
    Null = 0,
    Quote = 34,
    UpperA = 65,
    Backslash = 92,
}

union EnumArrayBytes4 {
    values: [DefaultDiscriminant; 4],
    bytes: [u8; 4],
    word: u32,
}

union EnumArrayBytes10 {
    values: [EscapeCharacter; 10],
    bytes: [u8; 10],
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

        // A Rust char occupies four bytes. Exercise ASCII and non-ASCII scalar
        // values through constructors, getters, setters, and aggregate fields.
        let ascii = CharacterBytes { character: 'A' };
        assert!(ascii.character == 'A');
        assert!(ascii.code_point == 0x41);
        assert!(ascii.bytes.a == 0x41);
        assert!(ascii.bytes.b == 0);
        assert!(ascii.bytes.c == 0);
        assert!(ascii.bytes.d == 0);

        let crab = CharacterBytes {
            code_point: 0x1f980,
        };
        assert!(crab.character as u32 == 0x1f980);
        assert!(crab.bytes.a == 0x80);
        assert!(crab.bytes.b == 0xf9);
        assert!(crab.bytes.c == 0x01);
        assert!(crab.bytes.d == 0);

        let mut character = CharacterBytes {
            character_and_unit: CharacterAndUnit {
                character: '\u{10ffff}',
                unit: (),
            },
        };
        assert!(character.character as u32 == 0x10ffff);
        assert!(character.character_and_unit.character as u32 == 0x10ffff);
        character.character_and_unit.unit;

        character.character = '\u{2764}';
        assert!(character.code_point == 0x2764);
        character.character_and_unit = CharacterAndUnit {
            character: '\u{03bb}',
            unit: (),
        };
        assert!(character.character as u32 == 0x03bb);

        // Reading or writing a zero-sized field must not inspect or modify the
        // union's storage.
        let empty = CharacterBytes { unit: () };
        empty.unit;

        let mut unit_write = CharacterBytes {
            code_point: 0x4433_2211,
        };
        unit_write.unit;
        unit_write.unit = ();
        assert!(unit_write.code_point == 0x4433_2211);

        // Fieldless enums use their numeric discriminants in union storage,
        // including implicit values following an explicit discriminant.
        let default_zero = DefaultEnumByte {
            value: DefaultDiscriminant::Zero,
        };
        assert!(default_zero.byte == 0);

        let default_enum = DefaultEnumByte {
            value: DefaultDiscriminant::Three,
        };
        assert!(default_enum.byte == 3);
        assert!(matches!(default_enum.value, DefaultDiscriminant::Three));

        let default_from_byte = DefaultEnumByte { byte: 4 };
        assert!(matches!(default_from_byte.value, DefaultDiscriminant::Four));

        let mut signed_enum = SignedEnumBytes {
            value: SignedDiscriminant::Negative,
        };
        assert!(signed_enum.signed == -2);
        assert!(signed_enum.unsigned == 0xfffe);
        assert!(signed_enum.bytes.a == 0xfe);
        assert!(signed_enum.bytes.b == 0xff);
        assert!(matches!(signed_enum.value, SignedDiscriminant::Negative));

        signed_enum.value = SignedDiscriminant::Seven;
        assert!(signed_enum.signed == 7);
        signed_enum.value = SignedDiscriminant::Eight;
        assert!(signed_enum.signed == 8);

        // Fixed-size arrays recurse through the same enum and scalar union
        // representation, including the lengths used by core's escape code.
        let mut four = EnumArrayBytes4 {
            values: [
                DefaultDiscriminant::Zero,
                DefaultDiscriminant::Three,
                DefaultDiscriminant::Four,
                DefaultDiscriminant::Zero,
            ],
        };
        assert!(four.bytes[0] == 0);
        assert!(four.bytes[1] == 3);
        assert!(four.bytes[2] == 4);
        assert!(four.bytes[3] == 0);
        assert!(four.word == 0x0004_0300);
        assert!(matches!(four.values[1], DefaultDiscriminant::Three));
        assert!(matches!(four.values[2], DefaultDiscriminant::Four));

        four.bytes = [4, 0, 3, 4];
        assert!(matches!(four.values[0], DefaultDiscriminant::Four));
        assert!(matches!(four.values[1], DefaultDiscriminant::Zero));
        assert!(matches!(four.values[2], DefaultDiscriminant::Three));
        assert!(matches!(four.values[3], DefaultDiscriminant::Four));

        let mut ten = EnumArrayBytes10 {
            values: [EscapeCharacter::UpperA; 10],
        };
        assert!(ten.bytes[0] == 65);
        assert!(ten.bytes[4] == 65);
        assert!(ten.bytes[9] == 65);
        ten.values = [
            EscapeCharacter::Backslash,
            EscapeCharacter::Quote,
            EscapeCharacter::Null,
            EscapeCharacter::UpperA,
            EscapeCharacter::UpperA,
            EscapeCharacter::Null,
            EscapeCharacter::Quote,
            EscapeCharacter::Backslash,
            EscapeCharacter::Null,
            EscapeCharacter::UpperA,
        ];
        assert!(ten.bytes[0] == 92);
        assert!(ten.bytes[1] == 34);
        assert!(ten.bytes[2] == 0);
        assert!(ten.bytes[3] == 65);
        assert!(ten.bytes[4] == 65);
        assert!(ten.bytes[5] == 0);
        assert!(ten.bytes[6] == 34);
        assert!(ten.bytes[7] == 92);
        assert!(ten.bytes[8] == 0);
        assert!(ten.bytes[9] == 65);

        let from_bytes = EnumArrayBytes10 {
            bytes: [0, 34, 65, 92, 65, 34, 0, 92, 34, 65],
        };
        assert!(matches!(from_bytes.values[0], EscapeCharacter::Null));
        assert!(matches!(from_bytes.values[1], EscapeCharacter::Quote));
        assert!(matches!(from_bytes.values[2], EscapeCharacter::UpperA));
        assert!(matches!(from_bytes.values[3], EscapeCharacter::Backslash));
    }
}
