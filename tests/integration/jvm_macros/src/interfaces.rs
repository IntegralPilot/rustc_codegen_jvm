#[jvm::interface("Main.Measure")]
impl Measure {
    #[jvm::static_method]
    fn create(bias: i32) -> *mut Self {}

    #[jvm::method]
    fn measure(&self, value: i64, scale: f64) -> i64 {}

    #[jvm::static_field]
    fn base() -> i32 {}

    // An explicitly different owner remains a class unless marked otherwise.
    #[jvm::static_method(class = "java.lang.Math", name = "max")]
    fn max(left: i32, right: i32) -> i32 {}
}

#[jvm::bindings]
impl Measure {
    #[jvm::method]
    fn doubled(&self, value: i64, scale: f64) -> i64 {}
}

#[jvm::bindings("Main.Measure", interface = true)]
impl Measure {
    #[jvm::static_method("constant")]
    fn constant() -> i32 {}
}

#[jvm::static_method(class = "Main", name = "makeMeasure")]
fn make_measure(bias: i32) -> *mut Measure {}

#[jvm::static_method(class = "Main.Measure", name = "constant", interface = true)]
fn constant() -> i32 {}

// No receiver or return type identifies this owner as an interface.
#[jvm::static_method(class = "Main.StaticDirect", name = "value", interface = true)]
fn static_direct(value: i32) -> i32 {}

unsafe extern "C" {
    #[jvm::interface("Main.Measure")]
    type RawMeasure;

    #[jvm::static_method(class = "Main.Measure", name = "create", interface = true)]
    fn raw_create(bias: i32) -> *mut RawMeasure;

    #[jvm::method("measure", "(JD)J")]
    fn raw_measure(receiver: &RawMeasure, value: i64, scale: f64) -> i64;

    // This owner is never declared as a type or used by a direct call. Its
    // interface kind must survive taking the imported function's address.
    #[jvm::static_method(class = "Main.StaticOnly", name = "value", interface = true)]
    fn raw_static_only(value: i64) -> i64;
}

#[jvm::class("Main.Measurement")]
impl Measurement {
    #[jvm::constructor]
    fn new(bias: i32) -> *mut Self {}

    #[jvm::method]
    fn measure(&self, value: i64, scale: f64) -> i64 {}
}

pub fn exercise() {
    unsafe {
        let measure = &*make_measure(7);
        assert_eq!(measure.measure(10, 2.0), 27);
        assert_eq!(measure.doubled(10, 2.0), 54);
        assert_eq!((&*Measure::create(9)).measure(10, 2.0), 29);
        assert_eq!(Measure::base(), 3);
        assert_eq!(Measure::max(4, 8), 8);
        assert_eq!(Measure::constant(), 42);
        assert_eq!(constant(), 42);
        assert_eq!(static_direct(10), 15);

        let create =
            std::hint::black_box(raw_create as unsafe extern "C" fn(i32) -> *mut RawMeasure);
        let invoke =
            std::hint::black_box(raw_measure as unsafe extern "C" fn(&RawMeasure, i64, f64) -> i64);
        assert_eq!(invoke(&*create(11), 10, 2.0), 31);
        let static_only = std::hint::black_box(raw_static_only as unsafe extern "C" fn(i64) -> i64);
        assert_eq!(static_only(13), 39);

        let construct = std::hint::black_box(Measurement::new as fn(i32) -> *mut Measurement);
        assert_eq!((&*construct(17)).measure(10, 2.0), 37);
    }
}
