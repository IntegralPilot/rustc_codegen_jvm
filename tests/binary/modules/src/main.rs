mod chemistry {
    pub struct Atom {
        pub protons: u32,
    }

    impl Atom {
        pub fn new(protons: u32) -> Atom {
            Atom { protons }
        }

        pub fn proton_count(self) -> u32 {
            self.protons
        }

        pub fn isotope(self, neutrons: u32) -> Isotope {
            Isotope {
                atom: self,
                neutrons,
            }
        }
    }

    pub struct Isotope {
        pub atom: Atom,
        pub neutrons: u32,
    }

    impl Isotope {
        pub fn mass_number(self) -> u32 {
            self.atom.protons + self.neutrons
        }
    }

    pub enum BondKind {
        Single,
        Double,
    }

    pub fn atom(protons: u32) -> Atom {
        Atom { protons }
    }

    pub fn shape(electron_domains: u32, molecular_domains: u32) -> &'static str {
        if electron_domains == 2 && molecular_domains == 2 {
            "linear"
        } else if electron_domains == 3 && molecular_domains == 3 {
            "trigonal planar"
        } else if electron_domains == 3 && molecular_domains == 2 {
            "bent"
        } else if electron_domains == 4 && molecular_domains == 4 {
            "tetrahedral"
        } else if electron_domains == 4 && molecular_domains == 3 {
            "trigonal pyramidal"
        } else if electron_domains == 4 && molecular_domains == 2 {
            "bent"
        } else {
            "unknown"
        }
    }

    pub fn label(code: u32) -> &'static str {
        if code == 1 { "chemistry" } else { "unknown" }
    }

    pub mod bonding {
        use super::{Atom, BondKind};

        pub struct Bond {
            pub left: Atom,
            pub right: Atom,
            pub kind: BondKind,
        }

        impl Bond {
            pub fn new(left: Atom, right: Atom, kind: BondKind) -> Bond {
                Bond { left, right, kind }
            }

            pub fn score(self) -> u32 {
                let base = self.left.protons + self.right.protons;
                match self.kind {
                    BondKind::Single => base,
                    BondKind::Double => base * 2,
                }
            }
        }

        pub fn make_bond(left: Atom, right: Atom, kind: BondKind) -> Bond {
            Bond { left, right, kind }
        }

        pub fn bond_score(bond: Bond) -> u32 {
            let base = bond.left.protons + bond.right.protons;
            match bond.kind {
                BondKind::Single => base,
                BondKind::Double => base * 2,
            }
        }

        pub fn family(order: u32) -> &'static str {
            if order == 1 {
                "single"
            } else if order == 2 {
                "double"
            } else if order == 3 {
                "triple"
            } else {
                "unknown"
            }
        }
    }
}

mod maths {
    use crate::chemistry;

    pub struct Point {
        pub x: i32,
        pub y: i32,
    }

    impl Point {
        pub fn new(x: i32, y: i32) -> Point {
            Point { x, y }
        }

        pub fn manhattan(self) -> i32 {
            self.x + self.y
        }

        pub fn translated(self, dx: i32, dy: i32) -> Point {
            Point {
                x: self.x + dx,
                y: self.y + dy,
            }
        }
    }

    pub struct Segment {
        pub start: Point,
        pub end: Point,
    }

    impl Segment {
        pub fn new(start: Point, end: Point) -> Segment {
            Segment { start, end }
        }

        pub fn horizontal_delta(self) -> i32 {
            self.end.x - self.start.x
        }
    }

    pub enum Axis {
        X,
        Y,
    }

    pub fn point(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    pub fn segment(start: Point, end: Point) -> Segment {
        Segment { start, end }
    }

    pub fn axis_value(point: Point, axis: Axis) -> i32 {
        match axis {
            Axis::X => point.x,
            Axis::Y => point.y,
        }
    }

    pub fn atom_offset(atom: chemistry::Atom, offset: i32) -> Point {
        Point {
            x: atom.protons as i32,
            y: offset,
        }
    }

    pub fn shape(sides: u32) -> &'static str {
        if sides == 3 {
            "triangle"
        } else if sides == 4 {
            "quadrilateral"
        } else if sides == 5 {
            "pentagon"
        } else if sides == 6 {
            "hexagon"
        } else if sides == 7 {
            "heptagon"
        } else if sides == 8 {
            "octagon"
        } else {
            "unknown"
        }
    }

    pub fn label(code: u32) -> &'static str {
        if code == 1 { "maths" } else { "unknown" }
    }
}

mod physics {
    pub struct Point {
        pub magnitude: i32,
    }

    impl Point {
        pub fn new(magnitude: i32) -> Point {
            Point { magnitude }
        }

        pub fn doubled(self) -> Point {
            Point {
                magnitude: self.magnitude * 2,
            }
        }

        pub fn as_math(self) -> crate::maths::Point {
            crate::maths::Point::new(self.magnitude, self.magnitude + 1)
        }
    }

    pub fn point(magnitude: i32) -> Point {
        Point { magnitude }
    }
}

#[derive(Copy, Clone)]
struct RootPair<T, U> {
    first: T,
    second: U,
}

impl<T, U> RootPair<T, U> {
    fn new(first: T, second: U) -> Self {
        RootPair { first, second }
    }

    fn flip(self) -> RootPair<U, T> {
        RootPair {
            first: self.second,
            second: self.first,
        }
    }
}

struct RootMeter {
    value: i32,
}

trait RootMeasure {
    fn read(&self) -> i32;
    fn add(&mut self, value: i32);
}

impl RootMeasure for RootMeter {
    fn read(&self) -> i32 {
        self.value
    }

    fn add(&mut self, value: i32) {
        self.value += value;
    }
}

fn root_identity<T>(value: T) -> T {
    value
}

fn root_swap<T, U>(left: T, right: U) -> (U, T) {
    (right, left)
}

fn root_add(left: i32, right: i32) -> i32 {
    left + right
}

fn root_skew(left: i32, right: i32) -> i32 {
    left * 3 - right
}

fn root_choose_op(addition: bool) -> fn(i32, i32) -> i32 {
    if addition { root_add } else { root_skew }
}

fn root_read_metric(metric: &dyn RootMeasure) -> i32 {
    metric.read()
}

fn root_add_metric(metric: &mut dyn RootMeasure, value: i32) -> i32 {
    metric.add(value);
    metric.read()
}

fn root_closure_value(seed: i32) -> i32 {
    let offset = root_identity(7);
    let add_offset = |value: i32| value + offset;
    add_offset(seed)
}

fn root_binary_search(slice: &[u32], target: u32) -> Option<usize> {
    let mut low = 0;
    let mut high = slice.len();

    while low < high {
        let mid = low + (high - low) / 2;
        let value = slice[mid];
        if value < target {
            low = mid + 1;
        } else if value > target {
            high = mid;
        } else {
            return Some(mid);
        }
    }

    None
}

fn root_guarded_value(use_value: bool) -> i32 {
    if use_value {
        73
    } else {
        panic!("unreachable root guarded panic")
    }
}

mod feature_matrix {
    pub fn run_module_uses_free() {
        assert!(crate::root_identity(41) == 41);
        assert!(crate::root_swap(1, "two") == ("two", 1));

        let flipped = crate::RootPair::new(10, 20).flip();
        assert!(flipped.first == 20);
        assert!(flipped.second == 10);

        let add = crate::root_choose_op(true);
        let skew = crate::root_choose_op(false);
        assert!(add(5, 6) == 11);
        assert!(skew(9, 4) == 23);

        let mut meter = crate::RootMeter { value: 30 };
        assert!(crate::root_read_metric(&meter) == 30);
        assert!(crate::root_add_metric(&mut meter, 12) == 42);

        assert!(crate::root_closure_value(5) == 12);
        assert!(crate::root_binary_search(&[3, 5, 8, 13], 8) == Some(2));
        assert!(crate::root_binary_search(&[3, 5, 8, 13], 9).is_none());
        assert!(crate::root_guarded_value(true) == 73);
    }

    pub mod same_module {
        #[derive(Copy, Clone)]
        pub struct Point {
            pub x: i32,
            pub y: i32,
        }

        impl Point {
            pub fn new(x: i32, y: i32) -> Point {
                Point { x, y }
            }

            pub fn translated(self, dx: i32, dy: i32) -> Point {
                Point {
                    x: self.x + dx,
                    y: self.y + dy,
                }
            }
        }

        pub struct Nested {
            pub point: Point,
            pub data: [i32; 3],
            pub pair: (i32, i32),
        }

        #[derive(Copy, Clone)]
        pub enum State {
            Empty,
            Count(i32),
            Named { label: &'static str, value: i32 },
            Data([u8; 4]),
        }

        #[derive(Copy, Clone)]
        pub struct Pair<T, U> {
            pub first: T,
            pub second: U,
        }

        impl<T, U> Pair<T, U> {
            pub fn new(first: T, second: U) -> Pair<T, U> {
                Pair { first, second }
            }

            pub fn flip(self) -> Pair<U, T> {
                Pair {
                    first: self.second,
                    second: self.first,
                }
            }
        }

        pub struct Counter {
            pub name: &'static str,
            pub count: u32,
            pub limit: u32,
            pub enabled: bool,
        }

        impl Counter {
            pub fn new(name: &'static str, limit: u32) -> Self {
                Counter {
                    name,
                    count: 0,
                    limit,
                    enabled: true,
                }
            }

            pub fn new_disabled(name: &'static str, limit: u32) -> Self {
                let mut counter = Self::new(name, limit);
                counter.enabled = false;
                counter
            }

            pub fn increment(&mut self) -> bool {
                if !self.enabled {
                    return false;
                }
                if self.count >= self.limit {
                    return false;
                }
                self.count += 1;
                true
            }

            pub fn increment_by(&mut self, amount: u32) -> u32 {
                if !self.enabled {
                    return 0;
                }

                let old = self.count;
                let next = self.count + amount;
                if next > self.limit {
                    self.count = self.limit;
                    self.limit - old
                } else {
                    self.count = next;
                    amount
                }
            }
        }

        pub trait Calculator {
            fn calculate(&self, left: i32, right: i32) -> i32;
            fn accumulate(&mut self, value: i32);
            fn get_value(&self) -> i32;
        }

        pub struct Adder {
            pub total: i32,
        }

        impl Calculator for Adder {
            fn calculate(&self, left: i32, right: i32) -> i32 {
                left + right
            }

            fn accumulate(&mut self, value: i32) {
                self.total += value;
            }

            fn get_value(&self) -> i32 {
                self.total
            }
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct TwoBytes {
            pub first: u8,
            pub second: u8,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct FourBytes {
            pub a: u8,
            pub b: u8,
            pub c: u8,
            pub d: u8,
        }

        #[repr(C)]
        pub union Bytes {
            pub pair: TwoBytes,
            pub word: u16,
        }

        #[repr(C)]
        pub union ScalarBytes {
            pub bytes: FourBytes,
            pub unsigned: u32,
            pub signed: i32,
            pub float: f32,
        }

        pub const DEFAULT_COUNTER: Counter = Counter {
            name: "same",
            count: 2,
            limit: 5,
            enabled: true,
        };

        pub fn identity<T>(value: T) -> T {
            value
        }

        pub fn add(left: i32, right: i32) -> i32 {
            left + right
        }

        pub fn multiply(left: i32, right: i32) -> i32 {
            left * right
        }

        pub fn choose_op(multiply_values: bool) -> fn(i32, i32) -> i32 {
            if multiply_values { multiply } else { add }
        }

        pub fn use_calculator(calc: &dyn Calculator, left: i32, right: i32) -> i32 {
            calc.calculate(left, right)
        }

        pub fn update_calculator(calc: &mut dyn Calculator, value: i32) -> i32 {
            calc.accumulate(value);
            calc.get_value()
        }

        pub fn fibonacci(n: usize) -> usize {
            if n == 0 {
                return 0;
            }
            if n == 1 {
                return 1;
            }

            let mut prev = 0;
            let mut current = 1;
            let mut index = 2;

            while index <= n {
                let next = prev + current;
                prev = current;
                current = next;
                index += 1;
            }

            current
        }

        pub fn collatz(n: u32) -> u32 {
            if n == 1 {
                1
            } else if n % 2 == 0 {
                collatz(n / 2)
            } else {
                collatz(3 * n + 1)
            }
        }

        pub fn binary_search(slice: &[u32], target: u32) -> Option<usize> {
            let mut low = 0;
            let mut high = slice.len();

            while low < high {
                let mid = low + (high - low) / 2;
                let value = slice[mid];
                if value < target {
                    low = mid + 1;
                } else if value > target {
                    high = mid;
                } else {
                    return Some(mid);
                }
            }

            None
        }

        pub fn make_y_negative(y: &mut i32) {
            if *y > 0 {
                *y = -*y;
            }
        }

        pub fn shift_point(point: &mut Point) {
            point.x += 10;
            make_y_negative(&mut point.y);
            point.x += 5;
        }

        pub fn guarded_value(use_value: bool) -> i32 {
            if use_value {
                42
            } else {
                panic!("unreachable same-module panic")
            }
        }

        pub fn run() {
            let point = Point::new(3, 4).translated(5, 7);
            assert!(point.x == 8);
            assert!(point.y == 11);

            let mut nested = Nested {
                point,
                data: [1, 2, 3],
                pair: (5, 10),
            };
            nested.point.x = 99;
            nested.data[1] = 42;
            nested.pair.1 = 77;
            assert!(nested.point.x == 99);
            assert!(nested.data[1] == 42);
            assert!(nested.pair.1 == 77);

            let state = State::Named {
                label: "ready",
                value: 12,
            };
            match state {
                State::Named { label, value } => {
                    assert!(label == "ready");
                    assert!(value == 12);
                }
                _ => panic!("expected named state"),
            }
            if let State::Data(bytes) = State::Data([4, 5, 6, 7]) {
                assert!(bytes[2] == 6);
            } else {
                panic!("expected data state");
            }
            if let State::Count(value) = State::Count(33) {
                assert!(value == 33);
            } else {
                panic!("expected count state");
            }
            if let State::Empty = State::Empty {
            } else {
                panic!("expected empty state");
            }

            let flipped = Pair::new(10, "ten").flip();
            assert!(flipped.first == "ten");
            assert!(flipped.second == 10);
            assert!(identity("generic").len() == 7);

            let mut counter = Counter::new("count", 3);
            assert!(counter.name == "count");
            assert!(counter.increment());
            assert!(counter.increment_by(2) == 2);
            assert!(counter.count == 3);
            assert!(!counter.increment());
            let mut disabled = Counter::new_disabled("disabled", 3);
            assert!(disabled.name == "disabled");
            assert!(!disabled.increment());
            assert!(DEFAULT_COUNTER.count == 2);

            let mut adder = Adder { total: 10 };
            assert!(adder.get_value() == 10);
            assert!(use_calculator(&adder, 7, 8) == 15);
            assert!(update_calculator(&mut adder, 5) == 15);

            let op = choose_op(true);
            assert!(op(6, 7) == 42);
            let other = choose_op(false);
            assert!(other(6, 7) == 13);

            assert!(fibonacci(10) == 55);
            assert!(collatz(27) == 1);
            assert!(binary_search(&[1, 2, 3, 5, 8, 13], 5) == Some(3));
            assert!(binary_search(&[1, 2, 3, 5, 8, 13], 4).is_none());

            let mut borrowed = Point::new(1, 5);
            shift_point(&mut borrowed);
            assert!(borrowed.x == 16);
            assert!(borrowed.y == -5);

            let offset = 11;
            let add_offset = |value: i32| value + offset;
            assert!(add_offset(31) == 42);

            unsafe {
                let bytes = Bytes {
                    pair: TwoBytes {
                        first: 0x01,
                        second: 0x02,
                    },
                };
                assert!(bytes.pair.first == 0x01);
                assert!(bytes.pair.second == 0x02);
                assert!(bytes.word == 0x0201);

                let mut scalar = ScalarBytes { unsigned: 0 };
                scalar.bytes = FourBytes {
                    a: 0xaa,
                    b: 0xbb,
                    c: 0xcc,
                    d: 0x7d,
                };
                assert!(scalar.unsigned == 0x7dcc_bbaa);

                let one = ScalarBytes { float: 1.0 };
                assert!(one.unsigned == 0x3f80_0000);
            }

            assert!(guarded_value(true) == 42);
        }
    }

    pub mod common_parent {
        pub mod producer {
            #[derive(Copy, Clone)]
            pub struct Reading {
                pub value: i32,
                pub scale: i32,
            }

            impl Reading {
                pub fn new(value: i32, scale: i32) -> Reading {
                    Reading { value, scale }
                }

                pub fn scaled(self) -> i32 {
                    self.value * self.scale
                }
            }

            #[derive(Copy, Clone)]
            pub enum ReadingState {
                Missing,
                Single(Reading),
                Pair { left: Reading, right: Reading },
            }

            #[derive(Copy, Clone)]
            pub struct Wrapper<T> {
                pub value: T,
            }

            impl<T> Wrapper<T> {
                pub fn new(value: T) -> Wrapper<T> {
                    Wrapper { value }
                }

                pub fn value(self) -> T {
                    self.value
                }
            }

            pub trait Sample {
                fn amount(&self) -> i32;
                fn add(&mut self, value: i32);
            }

            pub struct SampleBox {
                pub amount: i32,
            }

            impl Sample for SampleBox {
                fn amount(&self) -> i32 {
                    self.amount
                }

                fn add(&mut self, value: i32) {
                    self.amount += value;
                }
            }

            #[derive(Copy, Clone)]
            #[repr(C)]
            pub struct Bytes4 {
                pub a: u8,
                pub b: u8,
                pub c: u8,
                pub d: u8,
            }

            #[repr(C)]
            pub union Scalar {
                pub bytes: Bytes4,
                pub unsigned: u32,
            }

            pub const DEFAULT_READING: Reading = Reading { value: 6, scale: 7 };

            pub fn identity<T>(value: T) -> T {
                value
            }

            pub fn plus(left: i32, right: i32) -> i32 {
                left + right
            }

            pub fn minus(left: i32, right: i32) -> i32 {
                left - right
            }

            pub fn choose(addition: bool) -> fn(i32, i32) -> i32 {
                if addition { plus } else { minus }
            }

            pub fn sample_amount(sample: &dyn Sample) -> i32 {
                sample.amount()
            }

            pub fn add_sample(sample: &mut dyn Sample, value: i32) -> i32 {
                sample.add(value);
                sample.amount()
            }

            pub fn fib_recursive(n: usize) -> usize {
                match n {
                    0 => 0,
                    1 => 1,
                    _ => fib_recursive(n - 1) + fib_recursive(n - 2),
                }
            }

            pub fn first_match(slice: &[u32], target: u32) -> Option<usize> {
                let mut index = 0;
                while index < slice.len() {
                    if slice[index] == target {
                        return Some(index);
                    }
                    index += 1;
                }
                None
            }

            pub fn normalize(point: &mut Reading) {
                if point.value < 0 {
                    point.value = -point.value;
                }
                point.scale += 1;
            }

            pub fn closure_score(seed: i32) -> i32 {
                let offset = DEFAULT_READING.value;
                let add_offset = |value: i32| value + offset;
                add_offset(seed)
            }
        }

        pub mod consumer {
            use super::producer;

            pub fn run() {
                let reading = producer::Reading::new(6, 7);
                assert!(reading.scaled() == 42);
                assert!(producer::DEFAULT_READING.scaled() == 42);

                let state = producer::ReadingState::Pair {
                    left: reading,
                    right: producer::Reading::new(3, 5),
                };
                match state {
                    producer::ReadingState::Pair { left, right } => {
                        assert!(left.scaled() == 42);
                        assert!(right.scaled() == 15);
                    }
                    _ => panic!("expected pair reading"),
                }
                if let producer::ReadingState::Single(value) =
                    producer::ReadingState::Single(reading)
                {
                    assert!(value.scaled() == 42);
                } else {
                    panic!("expected single reading");
                }
                if let producer::ReadingState::Missing = producer::ReadingState::Missing {
                } else {
                    panic!("expected missing reading");
                }

                let wrapped = producer::Wrapper::new("common parent");
                assert!(wrapped.value().len() == 13);
                assert!(producer::identity(99) == 99);

                let add = producer::choose(true);
                let minus = producer::choose(false);
                assert!(add(20, 22) == 42);
                assert!(minus(50, 8) == 42);

                let mut sample = producer::SampleBox { amount: 30 };
                assert!(producer::sample_amount(&sample) == 30);
                assert!(producer::add_sample(&mut sample, 12) == 42);

                assert!(producer::fib_recursive(10) == 55);
                assert!(producer::first_match(&[7, 11, 13, 17], 13) == Some(2));
                assert!(producer::first_match(&[7, 11, 13, 17], 12).is_none());

                let mut point = producer::Reading::new(-5, 4);
                producer::normalize(&mut point);
                assert!(point.value == 5);
                assert!(point.scale == 5);

                assert!(producer::closure_score(36) == 42);

                unsafe {
                    let scalar = producer::Scalar {
                        unsigned: 0x4433_2211,
                    };
                    assert!(scalar.bytes.a == 0x11);
                    assert!(scalar.bytes.b == 0x22);
                    assert!(scalar.bytes.c == 0x33);
                    assert!(scalar.bytes.d == 0x44);
                }
            }
        }
    }
}

mod distant_provider {
    #[derive(Copy, Clone)]
    pub struct Packet {
        pub header: u32,
        pub payload: (i32, i32),
    }

    impl Packet {
        pub fn new(header: u32, left: i32, right: i32) -> Packet {
            Packet {
                header,
                payload: (left, right),
            }
        }

        pub fn checksum(self) -> i32 {
            self.header as i32 + self.payload.0 + self.payload.1
        }
    }

    #[derive(Copy, Clone)]
    pub enum PacketState {
        Ready(Packet),
        Retry(u8),
        Done,
    }

    #[derive(Copy, Clone)]
    pub struct Pair<T, U> {
        pub first: T,
        pub second: U,
    }

    pub trait Sink {
        fn total(&self) -> i32;
        fn push(&mut self, value: i32);
    }

    pub struct SumSink {
        pub total: i32,
    }

    impl Sink for SumSink {
        fn total(&self) -> i32 {
            self.total
        }

        fn push(&mut self, value: i32) {
            self.total += value;
        }
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct TwoU16 {
        pub low: u16,
        pub high: u16,
    }

    #[repr(C)]
    pub union Wide {
        pub words: TwoU16,
        pub value: u32,
    }

    pub const DEFAULT_PACKET: Packet = Packet {
        header: 10,
        payload: (20, 12),
    };

    pub fn pair<T, U>(first: T, second: U) -> Pair<T, U> {
        Pair { first, second }
    }

    pub fn double(value: i32) -> i32 {
        value * 2
    }

    pub fn triple(value: i32) -> i32 {
        value * 3
    }

    pub fn choose(triple_value: bool) -> fn(i32) -> i32 {
        if triple_value { triple } else { double }
    }

    pub fn sink_total(sink: &dyn Sink) -> i32 {
        sink.total()
    }

    pub fn sink_push(sink: &mut dyn Sink, value: i32) -> i32 {
        sink.push(value);
        sink.total()
    }

    pub fn sum_slice(values: &[i32]) -> i32 {
        let mut sum = 0;
        let mut index = 0;
        while index < values.len() {
            sum += values[index];
            index += 1;
        }
        sum
    }

    pub fn factorial(n: u32) -> u32 {
        if n <= 1 { 1 } else { n * factorial(n - 1) }
    }

    pub fn closure_value(seed: i32) -> i32 {
        let offset = DEFAULT_PACKET.header as i32;
        let add_offset = |value: i32| value + offset;
        add_offset(seed)
    }

    pub fn adjust_payload(packet: &mut Packet) {
        packet.payload.0 += 1;
        packet.payload.1 += 2;
    }
}

mod distant_consumer {
    pub fn run() {
        let mut packet = crate::distant_provider::Packet::new(10, 20, 12);
        assert!(packet.checksum() == 42);
        crate::distant_provider::adjust_payload(&mut packet);
        assert!(packet.payload.0 == 21);
        assert!(packet.payload.1 == 14);
        assert!(packet.checksum() == 45);
        assert!(crate::distant_provider::DEFAULT_PACKET.checksum() == 42);

        match crate::distant_provider::PacketState::Ready(crate::distant_provider::DEFAULT_PACKET) {
            crate::distant_provider::PacketState::Ready(value) => {
                assert!(value.checksum() == 42);
            }
            _ => panic!("expected ready packet"),
        }
        if let crate::distant_provider::PacketState::Retry(attempts) =
            crate::distant_provider::PacketState::Retry(3)
        {
            assert!(attempts == 3);
        } else {
            panic!("expected retry packet");
        }
        if let crate::distant_provider::PacketState::Done =
            crate::distant_provider::PacketState::Done
        {
        } else {
            panic!("expected done packet");
        }

        let pair = crate::distant_provider::pair("payload", 42);
        assert!(pair.first == "payload");
        assert!(pair.second == 42);

        let op = crate::distant_provider::choose(true);
        assert!(op(14) == 42);
        let other = crate::distant_provider::choose(false);
        assert!(other(21) == 42);

        let mut sink = crate::distant_provider::SumSink { total: 5 };
        assert!(crate::distant_provider::sink_total(&sink) == 5);
        assert!(crate::distant_provider::sink_push(&mut sink, 37) == 42);

        assert!(crate::distant_provider::sum_slice(&[5, 7, 11, 19]) == 42);
        assert!(crate::distant_provider::factorial(5) == 120);
        assert!(crate::distant_provider::closure_value(32) == 42);

        unsafe {
            let wide = crate::distant_provider::Wide { value: 0x3344_1122 };
            assert!(wide.words.low == 0x1122);
            assert!(wide.words.high == 0x3344);
        }

        assert!(crate::root_choose_op(true)(20, 22) == 42);
        assert!(crate::root_closure_value(35) == 42);
    }
}

struct Report {
    anchor: maths::Point,
    atom: chemistry::Atom,
    bond: chemistry::bonding::Bond,
}

fn label(code: u32) -> &'static str {
    if code == 1 { "root" } else { "unknown" }
}

fn main() {
    let chem_shape = chemistry::shape(4, 3);
    assert!(chem_shape == "trigonal pyramidal");

    let maths_shape = maths::shape(5);
    assert!(maths_shape == "pentagon");

    let root_label = label(1);
    assert!(root_label == "root");

    let chem_label = chemistry::label(1);
    assert!(chem_label == "chemistry");

    let maths_label = maths::label(1);
    assert!(maths_label == "maths");

    let bond_family = chemistry::bonding::family(2);
    assert!(bond_family == "double");

    let lithium = chemistry::Atom::new(3);
    assert!(lithium.proton_count() == 3);
    assert!(chemistry::Atom::new(6).isotope(7).mass_number() == 13);

    let hydrogen = chemistry::atom(1);
    let oxygen = chemistry::atom(8);
    let water_bond = chemistry::bonding::make_bond(hydrogen, oxygen, chemistry::BondKind::Double);
    let bond_score = chemistry::bonding::bond_score(water_bond);
    assert!(bond_score == 18);

    let ionic_bond = chemistry::bonding::Bond::new(
        chemistry::Atom::new(11),
        chemistry::Atom::new(17),
        chemistry::BondKind::Single,
    );
    assert!(ionic_bond.score() == 28);

    let start = maths::Point::new(3, 4);
    assert!(maths::Point::new(5, 6).manhattan() == 11);
    let translated = maths::Point::new(1, 2).translated(5, 7);
    assert!(translated.x == 6);
    assert!(translated.y == 9);

    let end = maths::atom_offset(chemistry::atom(6), 9);
    let segment = maths::Segment::new(start, end);
    assert!(segment.start.x == 3);
    assert!(segment.end.x == 6);
    let segment_from_free = maths::segment(maths::point(1, 2), maths::point(4, 8));
    assert!(segment_from_free.start.y == 2);
    assert!(segment_from_free.end.y == 8);
    assert!(maths::Segment::new(maths::point(2, 0), maths::point(9, 0)).horizontal_delta() == 7);
    assert!(maths::axis_value(maths::point(6, 9), maths::Axis::X) == 6);
    assert!(maths::axis_value(maths::point(6, 9), maths::Axis::Y) == 9);

    let physics_point = physics::Point::new(12).doubled();
    assert!(physics_point.magnitude == 24);
    let math_from_physics = physics::point(30).as_math();
    assert!(math_from_physics.x == 30);
    assert!(math_from_physics.y == 31);

    let report = Report {
        anchor: maths::point(10, 20),
        atom: chemistry::atom(7),
        bond: chemistry::bonding::make_bond(
            chemistry::atom(2),
            chemistry::atom(3),
            chemistry::BondKind::Single,
        ),
    };
    assert!(report.anchor.y == 20);
    assert!(report.atom.protons == 7);
    assert!(chemistry::bonding::bond_score(report.bond) == 5);

    feature_matrix::same_module::run();
    feature_matrix::common_parent::consumer::run();
    feature_matrix::run_module_uses_free();
    distant_consumer::run();

    let module_point = feature_matrix::same_module::Point::new(20, 22);
    assert!(module_point.x + module_point.y == 42);

    let common_reading = feature_matrix::common_parent::producer::Reading::new(6, 7);
    assert!(common_reading.scaled() == 42);

    let distant_packet = distant_provider::Packet::new(10, 20, 12);
    assert!(distant_packet.checksum() == 42);

    let mut root_meter = RootMeter { value: 21 };
    assert!(root_add_metric(&mut root_meter, 21) == 42);

    let root_pair = RootPair::new("left", 42);
    assert!(root_pair.first == "left");
    assert!(root_pair.second == 42);
    assert!(root_pair.flip().first == 42);

    assert!(root_identity("root").len() == 4);
    assert!(root_swap("module", 42) == (42, "module"));
    assert!(root_choose_op(false)(15, 3) == 42);
    assert!(root_closure_value(35) == 42);
    assert!(root_binary_search(&[1, 3, 5, 7, 9], 7) == Some(3));
    assert!(root_binary_search(&[1, 3, 5, 7, 9], 8).is_none());
    assert!(root_guarded_value(true) == 73);
}
