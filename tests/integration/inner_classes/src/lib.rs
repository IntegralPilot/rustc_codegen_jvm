#![feature(register_tool)]
#![register_tool(jvm)]

pub enum TrafficLight {
    Red,
    Yellow,
    Green,
}

pub enum MaybeNumber {
    None,
    Some(i32),
    Pair(i32, bool),
}

pub struct NumberBox {
    pub value: i32,
}

pub enum NestedEquality {
    Maybe(MaybeNumber),
    Boxed(NumberBox),
}

pub enum LeafEvent {
    Number(i32),
    Empty,
}

pub enum Event {
    #[jvm::subtype]
    Leaf(LeafEvent),
    Message(i32),
}

pub trait EventScore {
    fn score(&self) -> i32;
}

impl Event {
    pub fn category(&self) -> i32 {
        match self {
            Event::Leaf(_) => 7,
            Event::Message(_) => 9,
        }
    }
}

impl LeafEvent {
    pub fn category(&self) -> i32 {
        match self {
            LeafEvent::Number(_) => 70,
            LeafEvent::Empty => 71,
        }
    }
}

impl EventScore for Event {
    fn score(&self) -> i32 {
        match self {
            Event::Leaf(_) => 101,
            Event::Message(_) => 102,
        }
    }
}

impl EventScore for LeafEvent {
    fn score(&self) -> i32 {
        match self {
            LeafEvent::Number(_) => 201,
            LeafEvent::Empty => 202,
        }
    }
}

fn generic_event_score<T: EventScore + ?Sized>(value: &T) -> i32 {
    value.score()
}

pub fn get_light_action(light: TrafficLight) -> &'static str {
    match light {
        TrafficLight::Red => "Stop",
        TrafficLight::Yellow => "Caution",
        TrafficLight::Green => "Go",
    }
}

pub fn make_none() -> MaybeNumber {
    MaybeNumber::None
}

pub fn make_some(value: i32) -> MaybeNumber {
    MaybeNumber::Some(value)
}

pub fn make_pair(value: i32, flag: bool) -> MaybeNumber {
    MaybeNumber::Pair(value, flag)
}

pub fn make_number_box(value: i32) -> NumberBox {
    NumberBox { value }
}

pub fn wrap_maybe(value: MaybeNumber) -> NestedEquality {
    NestedEquality::Maybe(value)
}

pub fn wrap_boxed(value: NumberBox) -> NestedEquality {
    NestedEquality::Boxed(value)
}

pub fn make_leaf_number(value: i32) -> LeafEvent {
    LeafEvent::Number(value)
}

pub fn promote_leaf(value: LeafEvent) -> Event {
    Event::Leaf(value)
}

pub fn read_event(value: Event) -> i32 {
    match value {
        Event::Leaf(LeafEvent::Number(number)) => number,
        Event::Leaf(LeafEvent::Empty) => -1,
        Event::Message(number) => number + 1000,
    }
}

pub fn event_category(value: Event) -> i32 {
    value.category()
}

pub fn leaf_category(value: LeafEvent) -> i32 {
    value.category()
}

pub fn event_score_direct(value: Event) -> i32 {
    value.score()
}

pub fn leaf_score_direct(value: LeafEvent) -> i32 {
    value.score()
}

pub fn event_score_generic(value: Event) -> i32 {
    generic_event_score(&value)
}

pub fn event_score_dyn(value: Event) -> i32 {
    let value: &dyn EventScore = &value;
    value.score()
}
