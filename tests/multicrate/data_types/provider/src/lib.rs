#![no_std]

extern crate alloc;

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use kinds::{OptionArgument, OptionOccurrence};

#[derive(Clone, Copy)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    pub fn sum(self) -> i32 {
        self.x + self.y
    }
}

#[derive(Clone, Copy)]
pub struct Packet {
    pub point: Point,
    pub bytes: [u8; 4],
}

#[derive(Clone, Copy)]
pub struct Pair<T, U> {
    pub first: T,
    pub second: U,
}

#[derive(Clone, Copy)]
pub enum Status {
    Empty,
    Point(Point),
    Named { value: i32, flag: bool },
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct FourBytes {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
}

pub union ScalarBytes {
    pub bytes: FourBytes,
    pub unsigned: u32,
}

struct OptionGroup {
    short_name: String,
    long_name: String,
    hint: String,
    description: String,
    argument: OptionArgument,
    occurrence: OptionOccurrence,
}

pub struct Options {
    groups: Vec<OptionGroup>,
}

impl Options {
    pub fn new() -> Self {
        Self { groups: Vec::new() }
    }

    pub fn add(
        &mut self,
        short_name: &str,
        long_name: &str,
        hint: &str,
        description: &str,
        argument: bool,
        multiple: bool,
    ) {
        self.groups.push(OptionGroup {
            short_name: short_name.to_string(),
            long_name: long_name.to_string(),
            hint: hint.to_string(),
            description: description.to_string(),
            argument: if argument {
                OptionArgument::Yes
            } else {
                OptionArgument::No
            },
            occurrence: if multiple {
                OptionOccurrence::Multiple
            } else {
                OptionOccurrence::Optional
            },
        });
    }

    pub fn check(&self) -> bool {
        self.groups.len() == 2
            && self.groups[0].short_name == "h"
            && self.groups[0].long_name == "help"
            && self.groups[0].description == "show help"
            && matches!(self.groups[0].argument, OptionArgument::No)
            && self.groups[1].hint == "FILE"
            && matches!(self.groups[1].occurrence, OptionOccurrence::Multiple)
    }
}

trait Score {
    fn score(&self) -> i32;
}

struct Meter {
    value: i32,
}

impl Score for Meter {
    fn score(&self) -> i32 {
        self.value * 2
    }
}

fn add_three(value: i32) -> i32 {
    value + 3
}

fn apply(value: i32, f: fn(i32) -> i32) -> i32 {
    f(value)
}

pub fn move_point(point: Point, dx: i32, dy: i32) -> Point {
    Point {
        x: point.x + dx,
        y: point.y + dy,
    }
}

pub fn sum_point(point: Point) -> i32 {
    point.x + point.y
}

pub fn make_packet(x: i32, y: i32) -> Packet {
    Packet {
        point: Point { x, y },
        bytes: [1, 5, 9, 13],
    }
}

pub fn packet_score(packet: Packet) -> i32 {
    packet.point.x + packet.point.y + packet.bytes[2] as i32 + packet.bytes[3] as i32
}

pub fn make_pair(first: i32, second: i32) -> Pair<i32, i32> {
    Pair { first, second }
}

pub fn pair_score(pair: Pair<i32, i32>) -> i32 {
    pair.first + pair.second
}

pub fn status_point(x: i32, y: i32) -> Status {
    Status::Point(Point { x, y })
}

pub fn named_status(value: i32, flag: bool) -> Status {
    Status::Named { value, flag }
}

pub fn status_value(status: Status) -> i32 {
    match status {
        Status::Empty => 0,
        Status::Point(point) => point.x + point.y,
        Status::Named { value, flag } => {
            if flag {
                value * 2
            } else {
                value
            }
        }
    }
}

pub fn tuple_score() -> i32 {
    let pair = (6, 7);
    pair.0 + pair.1
}

pub fn trait_score(value: i32) -> i32 {
    let meter = Meter { value };
    let score: &dyn Score = &meter;
    score.score()
}

pub fn union_low_byte(value: u32) -> u8 {
    let scalar = ScalarBytes { unsigned: value };
    unsafe { scalar.bytes.a }
}

pub fn closure_score(value: i32) -> i32 {
    let offset = 13;
    let add_offset = |input: i32| input + offset;
    add_offset(value)
}

pub fn fn_pointer_score(value: i32) -> i32 {
    let f: fn(i32) -> i32 = add_three;
    apply(value, f)
}
