#![feature(extern_types)]

unsafe extern "C" {
    #[link_name = "java/lang/Object"]
    type JavaObject;

    #[link_name = "java/time/LocalDate"]
    type JavaLocalDate;

    #[link_name = "jvm:static:java/time/LocalDate:of"]
    fn java_local_date_of(year: i32, month: i32, day: i32) -> *const JavaLocalDate;

    #[link_name = "jvm:static:java/util/Objects:equals"]
    fn java_objects_equal(left: *const JavaObject, right: *const JavaObject) -> bool;
}

fn main() {
    unsafe {
        let leap_day = java_local_date_of(2024, 2, 29);
        let same_day = java_local_date_of(2024, 2, 29);
        let next_day = java_local_date_of(2024, 3, 1);

        assert!(java_objects_equal(
            leap_day as *const JavaObject,
            same_day as *const JavaObject,
        ));
        assert!(!java_objects_equal(
            leap_day as *const JavaObject,
            next_day as *const JavaObject,
        ));
    }
}
