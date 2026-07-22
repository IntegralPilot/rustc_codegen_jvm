#![feature(portable_simd)]

use std::simd::{Simd, prelude::SimdFloat};

fn main() {
    let values = Simd::<f32, 4>::from_array([1.0, -2.5, 3.0, -4.0]);
    assert_eq!(values, values);
    assert_ne!(values, -values);
    assert_eq!((-values).to_array(), [-1.0, 2.5, -3.0, 4.0]);
    assert_eq!(values.abs().to_array(), [1.0, 2.5, 3.0, 4.0]);
    assert_eq!(
        (values * Simd::splat(2.0)).to_array(),
        [2.0, -5.0, 6.0, -8.0]
    );

    let wide = Simd::<f64, 2>::from_array([-1.25, 3.5]);
    assert_eq!((-wide).abs().to_array(), [1.25, 3.5]);

    let integers = Simd::<i32, 4>::from_array([1, -2, i32::MIN, 4]);
    assert_eq!((-integers).to_array(), [-1, 2, i32::MIN, -4]);
    assert_eq!(
        (integers * Simd::from_array([3, 4, -1, i32::MAX])).to_array(),
        [3, -8, i32::MIN, -4]
    );
}
