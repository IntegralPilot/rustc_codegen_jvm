use crate::io::{self, BorrowedCursor, IoSlice, IoSliceMut};

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:writeStdout"]
    fn write_stdout(bytes: *const u8, length: usize);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:writeStderr"]
    fn write_stderr(bytes: *const u8, length: usize);

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:flushStdout"]
    fn flush_stdout();

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:flushStderr"]
    fn flush_stderr();

    #[link_name = "jvm:static:org/rustlang/runtime/RuntimeSupport:readStdin"]
    fn read_stdin(destination: *mut u8, length: usize) -> isize;
}

pub struct Stdin;
pub struct Stdout;
pub struct Stderr;

impl Stdin {
    pub const fn new() -> Stdin {
        Stdin
    }
}

impl io::Read for Stdin {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let read = unsafe { read_stdin(buf.as_mut_ptr(), buf.len()) };
        if read < 0 {
            Err(io::const_error!(io::ErrorKind::Other, "failed to read JVM standard input"))
        } else {
            Ok(read as usize)
        }
    }

    fn read_buf(&mut self, mut cursor: BorrowedCursor<'_, u8>) -> io::Result<()> {
        let read = unsafe {
            read_stdin(cursor.as_mut().as_mut_ptr().cast(), cursor.capacity())
        };
        if read < 0 {
            return Err(io::const_error!(
                io::ErrorKind::Other,
                "failed to read JVM standard input"
            ));
        }
        unsafe { cursor.advance(read as usize) };
        Ok(())
    }

    fn read_vectored(&mut self, bufs: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        match bufs.iter_mut().find(|buf| !buf.is_empty()) {
            Some(buf) => self.read(buf),
            None => Ok(0),
        }
    }

    fn is_read_vectored(&self) -> bool {
        false
    }
}

impl Stdout {
    pub const fn new() -> Stdout {
        Stdout
    }
}

impl io::Write for Stdout {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        unsafe { write_stdout(buf.as_ptr(), buf.len()) };
        Ok(buf.len())
    }

    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> io::Result<usize> {
        let mut written = 0;
        for buf in bufs {
            unsafe { write_stdout(buf.as_ptr(), buf.len()) };
            written += buf.len();
        }
        Ok(written)
    }

    fn is_write_vectored(&self) -> bool {
        true
    }

    fn flush(&mut self) -> io::Result<()> {
        unsafe { flush_stdout() };
        Ok(())
    }
}

impl Stderr {
    pub const fn new() -> Stderr {
        Stderr
    }
}

impl io::Write for Stderr {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        unsafe { write_stderr(buf.as_ptr(), buf.len()) };
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        unsafe { flush_stderr() };
        Ok(())
    }
}

pub const STDIN_BUF_SIZE: usize = crate::sys::io::DEFAULT_BUF_SIZE;

pub fn is_ebadf(_error: &io::Error) -> bool {
    false
}

pub fn panic_output() -> Option<Stderr> {
    Some(Stderr::new())
}
