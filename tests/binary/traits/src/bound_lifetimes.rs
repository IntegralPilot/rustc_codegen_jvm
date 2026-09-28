// Each case has its own payload so type caching cannot hide a skipped binder.

mod callable {
    struct Section<'a>(&'a [u8]);
    enum Payload<'a> {
        Data(Section<'a>),
    }
    type Callback<'a> = fn(Section<'a>) -> Payload<'a>;
    fn wrap(section: Section<'_>) -> Payload<'_> {
        Payload::Data(section)
    }
    fn consume<'a>(f: Callback<'a>, data: &'a [u8]) -> u8 {
        let Payload::Data(section) = f(Section(data));
        section.0[0]
    }
    #[inline(never)]
    fn invoke(f: &dyn for<'a> Fn(Callback<'a>, &'a [u8]) -> u8) -> u8 {
        f(wrap, &[42])
    }
    pub fn run() {
        assert_eq!(invoke(&consume), 42);
    }
}

mod methods {
    struct Section<'a>(&'a [u8]);
    enum Payload<'a> {
        Data(Section<'a>),
    }
    type Callback<'a> = fn(Section<'a>) -> Payload<'a>;
    fn wrap(section: Section<'_>) -> Payload<'_> {
        Payload::Data(section)
    }
    fn consume<'a>(f: Callback<'a>, data: &'a [u8]) -> u8 {
        let Payload::Data(section) = f(Section(data));
        section.0[0]
    }
    trait Consumer {
        fn consume<'a>(&self, f: Callback<'a>, data: &'a [u8]) -> u8;
    }
    struct Concrete;
    impl Consumer for Concrete {
        fn consume<'a>(&self, f: Callback<'a>, data: &'a [u8]) -> u8 {
            consume(f, data)
        }
    }
    #[inline(never)]
    fn invoke(f: &dyn Consumer) -> u8 {
        f.consume(wrap, &[42])
    }
    pub fn run() {
        assert_eq!(invoke(&Concrete), 42);
    }
}

mod associated {
    struct Section<'a>(&'a [u8]);
    enum Payload<'a> {
        Data(Section<'a>),
    }
    type Callback<'a> = fn(Section<'a>) -> Payload<'a>;
    fn wrap(section: Section<'_>) -> Payload<'_> {
        Payload::Data(section)
    }
    fn consume<'a>(f: Callback<'a>, data: &'a [u8]) -> u8 {
        let Payload::Data(section) = f(Section(data));
        section.0[0]
    }
    trait Factory<'a> {
        type Output;
        fn make(&self) -> Self::Output;
    }
    struct Concrete;
    impl<'a> Factory<'a> for Concrete {
        type Output = Callback<'a>;
        fn make(&self) -> Self::Output {
            wrap
        }
    }
    #[inline(never)]
    fn invoke(f: &dyn for<'a> Factory<'a, Output = Callback<'a>>) -> u8 {
        consume(f.make(), &[42])
    }
    pub fn run() {
        assert_eq!(invoke(&Concrete), 42);
    }
}

mod generic {
    struct Section<'a>(&'a [u8]);
    enum Payload<'a> {
        Data(Section<'a>),
    }
    type Callback<'a> = fn(Section<'a>) -> Payload<'a>;
    fn wrap(section: Section<'_>) -> Payload<'_> {
        Payload::Data(section)
    }
    fn consume<'a>(f: Callback<'a>, data: &'a [u8]) -> u8 {
        let Payload::Data(section) = f(Section(data));
        section.0[0]
    }
    trait Consumer<T> {
        fn consume(&self, callback: T) -> u8;
    }
    struct Concrete;
    impl<'a> Consumer<Callback<'a>> for Concrete {
        fn consume(&self, callback: Callback<'a>) -> u8 {
            consume(callback, &[42])
        }
    }
    #[inline(never)]
    fn invoke(f: &dyn for<'a> Consumer<Callback<'a>>) -> u8 {
        f.consume(wrap)
    }
    pub fn run() {
        assert_eq!(invoke(&Concrete), 42);
    }
}

pub fn run() {
    callable::run();
    methods::run();
    associated::run();
    generic::run();
}
