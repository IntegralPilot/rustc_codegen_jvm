#![feature(extern_types)]

unsafe extern "C" {
    #[link_name = "Main$State"]
    type JavaState;

    #[link_name = "jvm:new:Main$State"]
    fn state_new(value: i32, wide: i64) -> *mut JavaState;

    #[link_name = "jvm:field:value"]
    fn state_value(state: &JavaState) -> i32;

    #[link_name = "jvm:field:value"]
    fn state_set_value(state: &mut JavaState, value: i32);

    #[link_name = "jvm:field:wide"]
    fn state_wide(state: &JavaState) -> i64;

    #[link_name = "jvm:field:next"]
    fn state_next(state: &JavaState) -> *mut JavaState;

    #[link_name = "jvm:field:next"]
    fn state_set_next(state: &mut JavaState, next: *mut JavaState);

    #[link_name = "jvm:field:pointer"]
    fn state_pointer(state: &JavaState) -> *mut i32;

    #[link_name = "jvm:field:pointer"]
    fn state_set_pointer(state: &mut JavaState, pointer: *mut i32);

    #[link_name = "jvm:static-field:Main:shared"]
    fn shared() -> i64;

    #[link_name = "jvm:static-field:Main:shared"]
    fn set_shared(value: i64);

    #[link_name = "jvm:static-field:Main:sharedState"]
    fn shared_state() -> *mut JavaState;

    #[link_name = "jvm:static-field:Main:sharedState"]
    fn set_shared_state(value: *mut JavaState);

    #[link_name = "jvm:static-field:Main:sharedPointer"]
    fn shared_pointer() -> *mut i32;

    #[link_name = "jvm:static-field:Main:sharedPointer"]
    fn set_shared_pointer(value: *mut i32);
}

pub fn exercise() -> i64 {
    unsafe {
        let first = state_new(7, 4_000_000_000);
        let constructor: unsafe extern "C" fn(i32, i64) -> *mut JavaState = state_new;
        let second = constructor(11, 9);

        assert_eq!(state_value(&*first), 7);
        assert_eq!(state_wide(&*first), 4_000_000_000);

        let value_getter: unsafe extern "C" fn(&JavaState) -> i32 = state_value;
        let value_setter: unsafe extern "C" fn(&mut JavaState, i32) = state_set_value;
        value_setter(&mut *first, 13);
        assert_eq!(value_getter(&*first), 13);

        state_set_next(&mut *first, second);
        assert_eq!(state_value(&*first), 13);
        assert_eq!(state_value(&*state_next(&*first)), 11);

        let static_getter: unsafe extern "C" fn() -> i64 = shared;
        let static_setter: unsafe extern "C" fn(i64) = set_shared;
        assert_eq!(static_getter(), 21);
        static_setter(34);
        assert_eq!(static_getter(), 34);

        let object_static_getter: unsafe extern "C" fn() -> *mut JavaState = shared_state;
        let object_static_setter: unsafe extern "C" fn(*mut JavaState) = set_shared_state;
        object_static_setter(first);
        assert_eq!(state_value(&*object_static_getter()), 13);

        let mut values = [10, 20, 30, 40];
        state_set_pointer(&mut *first, values.as_mut_ptr().add(1));
        *state_pointer(&*first) = 22;
        assert_eq!(values[1], 22);

        set_shared_pointer(values.as_mut_ptr().add(2));
        *shared_pointer() = 33;
        assert_eq!(values[2], 33);

        state_value(&*first) as i64 + state_value(&*state_next(&*first)) as i64 + shared()
    }
}
