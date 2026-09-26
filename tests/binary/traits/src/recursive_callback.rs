use std::cell::Cell;

struct Field<'a> {
    value: FieldType<'a>,
}

enum FieldType<'a> {
    Record(Resolver<'a>),
    Scalar(u8),
}

struct Resolver<'a> {
    get_field: Box<dyn Fn(&'a [u8]) -> Option<Field<'a>> + 'a>,
}

struct DropCounter<'a>(&'a Cell<usize>);

impl Drop for DropCounter<'_> {
    fn drop(&mut self) {
        self.0.set(self.0.get() + 1);
    }
}

// Naming this callback's drop glue used to create fresh type definitions,
// recursively lowering its return type and the callback again until stack overflow.
pub fn run() {
    let drops = Cell::new(0);
    let counter = DropCounter(&drops);
    let bytes = [17, 34];
    let root = Field {
        value: FieldType::Record(Resolver {
            get_field: Box::new(move |data| {
                let _ = &counter;
                data.first().map(|&value| Field {
                    value: FieldType::Scalar(value),
                })
            }),
        }),
    };
    let FieldType::Record(resolver) = &root.value else {
        unreachable!();
    };
    let Some(Field {
        value: FieldType::Scalar(value),
    }) = (resolver.get_field)(&bytes)
    else {
        panic!("callback did not return a scalar field");
    };
    assert_eq!(value, 17);
    assert!((resolver.get_field)(&bytes[..0]).is_none());
    assert_eq!(drops.get(), 0);
    drop(root);
    assert_eq!(drops.get(), 1);
}
