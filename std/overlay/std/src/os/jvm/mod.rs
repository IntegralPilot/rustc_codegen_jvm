//! JVM-specific extensions to the standard library.

#![stable(feature = "jvm_os_ext", since = "1.99.0")]
#![doc(cfg(target_os = "jvm"))]

pub mod fs;

/// Commonly used JVM extension traits.
#[stable(feature = "jvm_os_ext", since = "1.99.0")]
pub mod prelude {
    #[doc(no_inline)]
    #[stable(feature = "jvm_os_ext", since = "1.99.0")]
    pub use super::fs::{FileExt, MetadataExt, OpenOptionsExt};
}
