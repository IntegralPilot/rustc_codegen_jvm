//! Streaming class-file encoding with dense instruction offsets. Ristretto's
//! model uses logical branch/frame indexes; translate them without cloning the
//! instruction list or constructing a second set of attributes.
use ristretto_classfile::{ClassFile, Error, Method, Result, attributes::Attribute};

mod code;
mod frames;

pub fn class_file(class: &ClassFile<'_>, bytes: &mut Vec<u8>) -> Result<()> {
    bytes.extend_from_slice(&0xcafebabe_u32.to_be_bytes());
    class.version.to_bytes(bytes)?;
    class.constant_pool.to_bytes(bytes)?;
    class.access_flags.to_bytes(bytes)?;
    u16(bytes, class.this_class);
    u16(bytes, class.super_class);
    count(bytes, class.interfaces.len())?;
    for &interface in &class.interfaces {
        u16(bytes, interface);
    }
    count(bytes, class.fields.len())?;
    for field in &class.fields {
        field.to_bytes(bytes)?;
    }
    count(bytes, class.methods.len())?;
    for method in &class.methods {
        method_info(method, bytes)?;
    }
    attributes(&class.attributes, bytes)
}

pub fn method_info(method: &Method, bytes: &mut Vec<u8>) -> Result<()> {
    method.access_flags.to_bytes(bytes)?;
    u16(bytes, method.name_index);
    u16(bytes, method.descriptor_index);
    attributes(&method.attributes, bytes)
}

fn attributes(attributes: &[Attribute], bytes: &mut Vec<u8>) -> Result<()> {
    count(bytes, attributes.len())?;
    for attribute in attributes {
        if let Attribute::Code {
            name_index,
            max_stack,
            max_locals,
            code,
            exception_table,
            attributes,
        } = attribute
        {
            let length = begin(bytes, *name_index);
            u16(bytes, *max_stack);
            u16(bytes, *max_locals);
            let offsets = code::instructions(code, bytes)?;
            count(bytes, exception_table.len())?;
            for exception in exception_table {
                u16(bytes, offsets.at(exception.range_pc.start as usize)?);
                // Preserve Ristretto's boundary convention, including its
                // clamping of a past-the-last logical exception end.
                u16(
                    bytes,
                    offsets
                        .at((exception.range_pc.end as usize).min(code.len().saturating_sub(1)))?,
                );
                u16(bytes, offsets.at(exception.handler_pc as usize)?);
                u16(bytes, exception.catch_type);
            }
            count(bytes, attributes.len())?;
            for attribute in attributes {
                match attribute {
                    Attribute::LineNumberTable {
                        name_index,
                        line_numbers,
                    } => {
                        let length = begin(bytes, *name_index);
                        count(bytes, line_numbers.len())?;
                        for line in line_numbers {
                            u16(bytes, offsets.at(line.start_pc as usize)?);
                            u16(bytes, line.line_number);
                        }
                        end(bytes, length)?;
                    }
                    Attribute::StackMapTable { name_index, frames } => {
                        let length = begin(bytes, *name_index);
                        frames::encode(frames, &offsets, bytes)?;
                        end(bytes, length)?;
                    }
                    _ => attribute.to_bytes(bytes)?,
                }
            }
            end(bytes, length)?;
        } else {
            attribute.to_bytes(bytes)?;
        }
    }
    Ok(())
}

fn u16(bytes: &mut Vec<u8>, value: u16) {
    bytes.extend_from_slice(&value.to_be_bytes());
}
fn count(bytes: &mut Vec<u8>, value: usize) -> Result<()> {
    u16(bytes, value.try_into()?);
    Ok(())
}
fn begin(bytes: &mut Vec<u8>, name: u16) -> usize {
    u16(bytes, name);
    let length = bytes.len();
    bytes.extend_from_slice(&[0; 4]);
    length
}
fn end(bytes: &mut [u8], length: usize) -> Result<()> {
    let size = u32::try_from(bytes.len() - length - 4)?;
    bytes[length..length + 4].copy_from_slice(&size.to_be_bytes());
    Ok(())
}

#[cfg(test)]
mod tests;
