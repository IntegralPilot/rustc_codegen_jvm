// Mirrors an inline vector whose elements live in union byte storage.
#[derive(Debug)]
enum Item {
    Component(Component),
    Spacing(f64, bool),
    Space,
}
#[derive(Debug)]
struct Component {
    props: Props,
    kind: Kind,
}
#[derive(Debug)]
struct Props {
    class: u8,
    left: Option<f64>,
    right: Option<f64>,
}
#[derive(Debug)]
enum Kind {
    Text(String),
    Glyph(Box<char>),
}
impl Item {
    #[inline(never)]
    fn new() -> Self {
        Self::Component(Component {
            props: Props {
                class: 1,
                left: None,
                right: None,
            },
            kind: Kind::Text("item".into()),
        })
    }
}

use std::mem::{ManuallyDrop, MaybeUninit};
union Data {
    inline: ManuallyDrop<MaybeUninit<[Item; 8]>>,
    heap: (*mut Item, usize),
}
struct Inline {
    data: Data,
    len: usize,
}
impl Inline {
    #[inline(never)]
    unsafe fn ptr(&mut self) -> *mut Item {
        unsafe { (*self.data.inline).as_mut_ptr().cast::<Item>() }
    }
    #[inline(never)]
    unsafe fn triple(&mut self) -> (*mut Item, &mut usize) {
        (unsafe { self.ptr() }, &mut self.len)
    }
    #[inline(never)]
    unsafe fn insert_end(&mut self, item: Item) {
        let (base, len) = unsafe { self.triple() };
        let target = unsafe { base.add(*len) };
        *len += 1;
        unsafe { target.write(item) };
    }
    #[inline(never)]
    unsafe fn push(&mut self, item: Item) {
        unsafe {
            self.ptr().add(self.len).write(item);
        }
        self.len += 1;
    }
}
unsafe extern "C" {
    #[link_name = "jvm:static:java/lang/System:gc"]
    fn java_gc();
}
#[inline(never)]
fn acquire(items: &mut Inline) -> &mut Component {
    if let Item::Component(comp) = unsafe { &mut *items.ptr() } {
        comp
    } else {
        panic!("wrong variant")
    }
}
#[inline(never)]
fn modify(comp: &mut Component) {
    comp.props.right = Some(2.5);
}
pub fn run() {
    let mut items = Inline {
        data: Data {
            inline: ManuallyDrop::new(MaybeUninit::uninit()),
        },
        len: 0,
    };
    unsafe {
        items.push(Item::new());
        let payload = acquire(&mut items);
        java_gc();
        modify(payload);
        items.push(Item::new());
        if let Item::Component(c) = &*items.ptr() {
            assert_eq!(c.props.right, Some(2.5));
        } else {
            panic!("wrong variant");
        }
        items.insert_end(Item::Space);
        assert_eq!(items.len, 3);
        assert!(matches!(*items.ptr().add(2), Item::Space));
        items.ptr().drop_in_place();
        items.ptr().add(1).drop_in_place();
    }
}
