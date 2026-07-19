#![no_std]
#![feature(lang_items)]
#![allow(internal_features)]

include!("../../../support/test_prelude.rs");

fn main() {
    let point = provider::Point::new(7, 11);
    assert!(point.x == 7);
    assert!(point.y == 11);
    assert!(point.sum() == 18);

    let moved = provider::move_point(point, -2, 5);
    assert!(moved.x == 5);
    assert!(moved.y == 16);
    assert!(provider::sum_point(moved) == 21);

    let packet = provider::make_packet(3, 4);
    assert!(packet.point.x == 3);
    assert!(packet.bytes[2] == 9);
    assert!(provider::packet_score(packet) == 29);

    let pair = provider::make_pair(12, 30);
    assert!(pair.first == 12);
    assert!(pair.second == 30);
    assert!(provider::pair_score(pair) == 42);

    let state = provider::status_point(4, 5);
    assert!(provider::status_value(state) == 9);
    match state {
        provider::Status::Point(point) => {
            assert!(point.x == 4);
            assert!(point.y == 5);
        }
        _ => panic!("expected point status"),
    }

    let named = provider::Status::Named {
        value: 10,
        flag: true,
    };
    assert!(provider::status_value(named) == 20);

    assert!(provider::tuple_score() == 13);
    assert!(provider::trait_score(21) == 42);
    assert!(provider::union_low_byte(0x44332211) == 0x11);
    assert!(provider::closure_score(9) == 22);
    assert!(provider::fn_pointer_score(8) == 11);

    let mut options = provider::Options::new();
    options.add("h", "help", "", "show help", false, false);
    options.add("o", "output", "FILE", "write output", true, true);
    assert!(options.check());
}
