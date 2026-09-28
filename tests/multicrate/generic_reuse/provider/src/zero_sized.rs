// These local types are first instantiated in the downstream crate. MIR
// represents their constructors as ZeroSized constants, not aggregates.
#[inline(never)]
pub fn check<E>(error: Option<E>) -> Result<(), E> {
    enum Field {
        Only,
    }
    struct Marker;
    struct Wrapper(Marker);

    let field: Result<Field, E> = match error {
        Some(error) => Err(error),
        None => Ok(Field::Only),
    };
    core::hint::black_box(field)?;
    core::hint::black_box(Wrapper(Marker));
    Ok(())
}
