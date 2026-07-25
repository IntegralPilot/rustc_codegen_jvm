use crate::fmt;
use crate::io::{self, BorrowedCursor, IoSlice, IoSliceMut};
use crate::net::{
    IpAddr, Ipv4Addr, Ipv6Addr, Shutdown, SocketAddr, SocketAddrV4, SocketAddrV6, ToSocketAddrs,
};
use crate::time::Duration;
use crate::vec::Vec;

const OPTION_KEEPALIVE: i32 = 1;
const OPTION_NODELAY: i32 = 2;
const OPTION_BROADCAST: i32 = 3;
const OPTION_MULTICAST_LOOP_V4: i32 = 4;
const OPTION_MULTICAST_LOOP_V6: i32 = 5;
const OPTION_MULTICAST_TTL_V4: i32 = 6;

unsafe extern "C" {
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastErrorKind"]
    fn jvm_last_error_kind() -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastErrorMessageLength"]
    fn jvm_last_error_message_length() -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:copyLastErrorMessage"]
    fn jvm_copy_last_error_message(destination: *mut u8);

    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastAddressLength"]
    fn jvm_last_address_length() -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:copyLastAddress"]
    fn jvm_copy_last_address(destination: *mut u8);
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastAddressPort"]
    fn jvm_last_address_port() -> u16;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lastAddressScopeId"]
    fn jvm_last_address_scope_id() -> u32;

    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:createIoVector"]
    fn jvm_create_io_vector(count: usize, read: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:addIoVectorBuffer"]
    fn jvm_add_io_vector_buffer(handle: u64, buffer: *mut u8, length: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:closeIoVector"]
    fn jvm_close_io_vector(handle: u64);

    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpConnect"]
    fn jvm_tcp_connect(
        address: *const u8,
        address_length: usize,
        port: u16,
        scope_id: u32,
        timeout_seconds: i64,
        timeout_nanos: u32,
    ) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpClose"]
    fn jvm_tcp_close(handle: u64);
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpDuplicate"]
    fn jvm_tcp_duplicate(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpRead"]
    fn jvm_tcp_read(handle: u64, destination: *mut u8, length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpReadVectored"]
    fn jvm_tcp_read_vectored(handle: u64, vector: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpPeek"]
    fn jvm_tcp_peek(handle: u64, destination: *mut u8, length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpWrite"]
    fn jvm_tcp_write(handle: u64, source: *const u8, length: usize) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpWriteVectored"]
    fn jvm_tcp_write_vectored(handle: u64, vector: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpAddress"]
    fn jvm_tcp_address(handle: u64, peer: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpShutdown"]
    fn jvm_tcp_shutdown(handle: u64, how: i32) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpSetTimeout"]
    fn jvm_tcp_set_timeout(handle: u64, read: bool, enabled: bool, seconds: i64, nanos: u32)
    -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpTimeoutSeconds"]
    fn jvm_tcp_timeout_seconds(handle: u64, read: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpTimeoutNanos"]
    fn jvm_tcp_timeout_nanos(handle: u64, read: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpSetBooleanOption"]
    fn jvm_tcp_set_boolean_option(handle: u64, option: i32, value: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpBooleanOption"]
    fn jvm_tcp_boolean_option(handle: u64, option: i32) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpSetLinger"]
    fn jvm_tcp_set_linger(handle: u64, enabled: bool, seconds: i32) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpLinger"]
    fn jvm_tcp_linger(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpTakeError"]
    fn jvm_tcp_take_error(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:tcpSetNonblocking"]
    fn jvm_tcp_set_nonblocking(handle: u64, nonblocking: bool) -> i32;

    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerBind"]
    fn jvm_listener_bind(
        address: *const u8,
        address_length: usize,
        port: u16,
        scope_id: u32,
    ) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerClose"]
    fn jvm_listener_close(handle: u64);
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerDuplicate"]
    fn jvm_listener_duplicate(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerAddress"]
    fn jvm_listener_address(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerAccept"]
    fn jvm_listener_accept(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerTakeError"]
    fn jvm_listener_take_error(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:listenerSetNonblocking"]
    fn jvm_listener_set_nonblocking(handle: u64, nonblocking: bool) -> i32;

    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpBind"]
    fn jvm_udp_bind(address: *const u8, address_length: usize, port: u16, scope_id: u32) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpClose"]
    fn jvm_udp_close(handle: u64);
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpDuplicate"]
    fn jvm_udp_duplicate(handle: u64) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpAddress"]
    fn jvm_udp_address(handle: u64, peer: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpRecvFrom"]
    fn jvm_udp_recv_from(handle: u64, destination: *mut u8, length: usize, peek: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpSendTo"]
    fn jvm_udp_send_to(
        handle: u64,
        source: *const u8,
        length: usize,
        address: *const u8,
        address_length: usize,
        port: u16,
        scope_id: u32,
    ) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpSetTimeout"]
    fn jvm_udp_set_timeout(handle: u64, read: bool, enabled: bool, seconds: i64, nanos: u32)
    -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpTimeoutSeconds"]
    fn jvm_udp_timeout_seconds(handle: u64, read: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpTimeoutNanos"]
    fn jvm_udp_timeout_nanos(handle: u64, read: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpSetBooleanOption"]
    fn jvm_udp_set_boolean_option(handle: u64, option: i32, value: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpBooleanOption"]
    fn jvm_udp_boolean_option(handle: u64, option: i32) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpSetIntegerOption"]
    fn jvm_udp_set_integer_option(handle: u64, option: i32, value: u32) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpIntegerOption"]
    fn jvm_udp_integer_option(handle: u64, option: i32) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpMulticast"]
    fn jvm_udp_multicast(
        handle: u64,
        join: bool,
        address: *const u8,
        address_length: usize,
        interface_address: *const u8,
        interface_length: usize,
        interface_index: u32,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpTakeError"]
    fn jvm_udp_take_error(handle: u64) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpSetNonblocking"]
    fn jvm_udp_set_nonblocking(handle: u64, nonblocking: bool) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpConnect"]
    fn jvm_udp_connect(
        handle: u64,
        address: *const u8,
        address_length: usize,
        port: u16,
        scope_id: u32,
    ) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpRecv"]
    fn jvm_udp_recv(handle: u64, destination: *mut u8, length: usize, peek: bool) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:udpSend"]
    fn jvm_udp_send(handle: u64, source: *const u8, length: usize) -> i64;

    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lookupHost"]
    fn jvm_lookup_host(host: *const u8, host_length: usize, port: u16) -> i64;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lookupCount"]
    fn jvm_lookup_count(handle: u64) -> usize;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:lookupAddress"]
    fn jvm_lookup_address(handle: u64, index: usize) -> i32;
    #[link_name = "jvm:static:org/rustlang/runtime/NetworkSupport:closeLookup"]
    fn jvm_close_lookup(handle: u64);
}

struct IoVector(u64);

impl Drop for IoVector {
    fn drop(&mut self) {
        unsafe { jvm_close_io_vector(self.0) };
    }
}

pub struct TcpStream(u64);

impl TcpStream {
    pub fn connect<A: ToSocketAddrs>(addr: A) -> io::Result<TcpStream> {
        super::each_addr(addr, |addr| connect_tcp(addr, None))
    }

    pub fn connect_timeout(addr: &SocketAddr, timeout: Duration) -> io::Result<TcpStream> {
        if timeout.is_zero() {
            return Err(io::Error::ZERO_TIMEOUT);
        }
        connect_tcp(addr, Some(timeout))
    }

    pub fn set_read_timeout(&self, duration: Option<Duration>) -> io::Result<()> {
        set_tcp_timeout(self.0, true, duration)
    }

    pub fn set_write_timeout(&self, duration: Option<Duration>) -> io::Result<()> {
        set_tcp_timeout(self.0, false, duration)
    }

    pub fn read_timeout(&self) -> io::Result<Option<Duration>> {
        tcp_timeout(self.0, true)
    }

    pub fn write_timeout(&self) -> io::Result<Option<Duration>> {
        tcp_timeout(self.0, false)
    }

    pub fn peek(&self, buffer: &mut [u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_tcp_peek(self.0, buffer.as_mut_ptr(), buffer.len()) })
    }

    pub fn read(&self, buffer: &mut [u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_tcp_read(self.0, buffer.as_mut_ptr(), buffer.len()) })
    }

    pub fn read_buf(&self, mut cursor: BorrowedCursor<'_, u8>) -> io::Result<()> {
        let count = result_count(unsafe {
            jvm_tcp_read(
                self.0,
                cursor.as_mut().as_mut_ptr().cast(),
                cursor.capacity(),
            )
        })?;
        unsafe { cursor.advance(count) };
        Ok(())
    }

    pub fn read_vectored(&self, buffers: &mut [IoSliceMut<'_>]) -> io::Result<usize> {
        if buffers.is_empty() {
            return Ok(0);
        }
        let vector = create_io_vector(buffers.len(), true)?;
        for buffer in buffers {
            result_code(unsafe {
                jvm_add_io_vector_buffer(vector.0, buffer.as_mut_ptr(), buffer.len())
            })?;
        }
        result_count(unsafe { jvm_tcp_read_vectored(self.0, vector.0) })
    }

    pub fn is_read_vectored(&self) -> bool {
        true
    }

    pub fn write(&self, buffer: &[u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_tcp_write(self.0, buffer.as_ptr(), buffer.len()) })
    }

    pub fn write_vectored(&self, buffers: &[IoSlice<'_>]) -> io::Result<usize> {
        if buffers.is_empty() {
            return Ok(0);
        }
        let vector = create_io_vector(buffers.len(), false)?;
        for buffer in buffers {
            result_code(unsafe {
                jvm_add_io_vector_buffer(vector.0, buffer.as_ptr().cast_mut(), buffer.len())
            })?;
        }
        result_count(unsafe { jvm_tcp_write_vectored(self.0, vector.0) })
    }

    pub fn is_write_vectored(&self) -> bool {
        true
    }

    pub fn peer_addr(&self) -> io::Result<SocketAddr> {
        result_code(unsafe { jvm_tcp_address(self.0, true) })?;
        last_address()
    }

    pub fn socket_addr(&self) -> io::Result<SocketAddr> {
        result_code(unsafe { jvm_tcp_address(self.0, false) })?;
        last_address()
    }

    pub fn shutdown(&self, how: Shutdown) -> io::Result<()> {
        let how = match how {
            Shutdown::Read => 0,
            Shutdown::Write => 1,
            Shutdown::Both => 2,
        };
        result_code(unsafe { jvm_tcp_shutdown(self.0, how) })
    }

    pub fn duplicate(&self) -> io::Result<TcpStream> {
        result_handle(unsafe { jvm_tcp_duplicate(self.0) }).map(TcpStream)
    }

    pub fn set_linger(&self, linger: Option<Duration>) -> io::Result<()> {
        let seconds = linger
            .map(|duration| duration.as_secs().min(i32::MAX as u64) as i32)
            .unwrap_or(0);
        result_code(unsafe { jvm_tcp_set_linger(self.0, linger.is_some(), seconds) })
    }

    pub fn linger(&self) -> io::Result<Option<Duration>> {
        match unsafe { jvm_tcp_linger(self.0) } {
            value if value < -1 => Err(last_error()),
            -1 => Ok(None),
            seconds => Ok(Some(Duration::from_secs(seconds as u64))),
        }
    }

    pub fn set_keepalive(&self, keepalive: bool) -> io::Result<()> {
        set_tcp_boolean(self.0, OPTION_KEEPALIVE, keepalive)
    }

    pub fn keepalive(&self) -> io::Result<bool> {
        tcp_boolean(self.0, OPTION_KEEPALIVE)
    }

    pub fn set_nodelay(&self, nodelay: bool) -> io::Result<()> {
        set_tcp_boolean(self.0, OPTION_NODELAY, nodelay)
    }

    pub fn nodelay(&self) -> io::Result<bool> {
        tcp_boolean(self.0, OPTION_NODELAY)
    }

    pub fn set_ttl(&self, _ttl: u32) -> io::Result<()> {
        unsupported("Java does not expose the TCP unicast IP TTL")
    }

    pub fn ttl(&self) -> io::Result<u32> {
        unsupported("Java does not expose the TCP unicast IP TTL")
    }

    pub fn take_error(&self) -> io::Result<Option<io::Error>> {
        take_error(unsafe { jvm_tcp_take_error(self.0) })
    }

    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        result_code(unsafe { jvm_tcp_set_nonblocking(self.0, nonblocking) })
    }
}

impl Drop for TcpStream {
    fn drop(&mut self) {
        unsafe { jvm_tcp_close(self.0) };
    }
}

impl fmt::Debug for TcpStream {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = formatter.debug_struct("TcpStream");
        if let Ok(address) = self.socket_addr() {
            debug.field("addr", &address);
        }
        if let Ok(address) = self.peer_addr() {
            debug.field("peer", &address);
        }
        debug.field("handle", &self.0).finish()
    }
}

pub struct TcpListener(u64);

impl TcpListener {
    pub fn bind<A: ToSocketAddrs>(addr: A) -> io::Result<TcpListener> {
        super::each_addr(addr, |address| {
            with_address(address, |bytes, length, port, scope| {
                result_handle(unsafe { jvm_listener_bind(bytes, length, port, scope) })
                    .map(TcpListener)
            })
        })
    }

    pub fn socket_addr(&self) -> io::Result<SocketAddr> {
        result_code(unsafe { jvm_listener_address(self.0) })?;
        last_address()
    }

    pub fn accept(&self) -> io::Result<(TcpStream, SocketAddr)> {
        let stream = TcpStream(result_handle(unsafe { jvm_listener_accept(self.0) })?);
        let address = last_address()?;
        Ok((stream, address))
    }

    pub fn duplicate(&self) -> io::Result<TcpListener> {
        result_handle(unsafe { jvm_listener_duplicate(self.0) }).map(TcpListener)
    }

    pub fn set_ttl(&self, _ttl: u32) -> io::Result<()> {
        unsupported("Java does not expose the TCP listener unicast IP TTL")
    }

    pub fn ttl(&self) -> io::Result<u32> {
        unsupported("Java does not expose the TCP listener unicast IP TTL")
    }

    pub fn set_only_v6(&self, _only_v6: bool) -> io::Result<()> {
        unsupported("Java does not expose the IPV6_V6ONLY socket option")
    }

    pub fn only_v6(&self) -> io::Result<bool> {
        unsupported("Java does not expose the IPV6_V6ONLY socket option")
    }

    pub fn take_error(&self) -> io::Result<Option<io::Error>> {
        take_error(unsafe { jvm_listener_take_error(self.0) })
    }

    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        result_code(unsafe { jvm_listener_set_nonblocking(self.0, nonblocking) })
    }
}

impl Drop for TcpListener {
    fn drop(&mut self) {
        unsafe { jvm_listener_close(self.0) };
    }
}

impl fmt::Debug for TcpListener {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = formatter.debug_struct("TcpListener");
        if let Ok(address) = self.socket_addr() {
            debug.field("addr", &address);
        }
        debug.field("handle", &self.0).finish()
    }
}

pub struct UdpSocket(u64);

impl UdpSocket {
    pub fn bind<A: ToSocketAddrs>(addr: A) -> io::Result<UdpSocket> {
        super::each_addr(addr, |address| {
            with_address(address, |bytes, length, port, scope| {
                result_handle(unsafe { jvm_udp_bind(bytes, length, port, scope) }).map(UdpSocket)
            })
        })
    }

    pub fn peer_addr(&self) -> io::Result<SocketAddr> {
        result_code(unsafe { jvm_udp_address(self.0, true) })?;
        last_address()
    }

    pub fn socket_addr(&self) -> io::Result<SocketAddr> {
        result_code(unsafe { jvm_udp_address(self.0, false) })?;
        last_address()
    }

    pub fn recv_from(&self, buffer: &mut [u8]) -> io::Result<(usize, SocketAddr)> {
        let count = result_count(unsafe {
            jvm_udp_recv_from(self.0, buffer.as_mut_ptr(), buffer.len(), false)
        })?;
        Ok((count, last_address()?))
    }

    pub fn peek_from(&self, buffer: &mut [u8]) -> io::Result<(usize, SocketAddr)> {
        let count = result_count(unsafe {
            jvm_udp_recv_from(self.0, buffer.as_mut_ptr(), buffer.len(), true)
        })?;
        Ok((count, last_address()?))
    }

    pub fn send_to(&self, buffer: &[u8], destination: &SocketAddr) -> io::Result<usize> {
        with_address(destination, |bytes, length, port, scope| {
            result_count(unsafe {
                jvm_udp_send_to(
                    self.0,
                    buffer.as_ptr(),
                    buffer.len(),
                    bytes,
                    length,
                    port,
                    scope,
                )
            })
        })
    }

    pub fn duplicate(&self) -> io::Result<UdpSocket> {
        result_handle(unsafe { jvm_udp_duplicate(self.0) }).map(UdpSocket)
    }

    pub fn set_read_timeout(&self, duration: Option<Duration>) -> io::Result<()> {
        set_udp_timeout(self.0, true, duration)
    }

    pub fn set_write_timeout(&self, duration: Option<Duration>) -> io::Result<()> {
        set_udp_timeout(self.0, false, duration)
    }

    pub fn read_timeout(&self) -> io::Result<Option<Duration>> {
        udp_timeout(self.0, true)
    }

    pub fn write_timeout(&self) -> io::Result<Option<Duration>> {
        udp_timeout(self.0, false)
    }

    pub fn set_broadcast(&self, broadcast: bool) -> io::Result<()> {
        set_udp_boolean(self.0, OPTION_BROADCAST, broadcast)
    }

    pub fn broadcast(&self) -> io::Result<bool> {
        udp_boolean(self.0, OPTION_BROADCAST)
    }

    pub fn set_multicast_loop_v4(&self, value: bool) -> io::Result<()> {
        set_udp_boolean(self.0, OPTION_MULTICAST_LOOP_V4, value)
    }

    pub fn multicast_loop_v4(&self) -> io::Result<bool> {
        udp_boolean(self.0, OPTION_MULTICAST_LOOP_V4)
    }

    pub fn set_multicast_ttl_v4(&self, value: u32) -> io::Result<()> {
        result_code(unsafe { jvm_udp_set_integer_option(self.0, OPTION_MULTICAST_TTL_V4, value) })
    }

    pub fn multicast_ttl_v4(&self) -> io::Result<u32> {
        let value = unsafe { jvm_udp_integer_option(self.0, OPTION_MULTICAST_TTL_V4) };
        if value < 0 {
            Err(last_error())
        } else {
            Ok(value as u32)
        }
    }

    pub fn set_multicast_loop_v6(&self, value: bool) -> io::Result<()> {
        set_udp_boolean(self.0, OPTION_MULTICAST_LOOP_V6, value)
    }

    pub fn multicast_loop_v6(&self) -> io::Result<bool> {
        udp_boolean(self.0, OPTION_MULTICAST_LOOP_V6)
    }

    pub fn join_multicast_v4(&self, multiaddr: &Ipv4Addr, interface: &Ipv4Addr) -> io::Result<()> {
        multicast_v4(self.0, true, multiaddr, interface)
    }

    pub fn join_multicast_v6(&self, multiaddr: &Ipv6Addr, interface: u32) -> io::Result<()> {
        multicast_v6(self.0, true, multiaddr, interface)
    }

    pub fn leave_multicast_v4(&self, multiaddr: &Ipv4Addr, interface: &Ipv4Addr) -> io::Result<()> {
        multicast_v4(self.0, false, multiaddr, interface)
    }

    pub fn leave_multicast_v6(&self, multiaddr: &Ipv6Addr, interface: u32) -> io::Result<()> {
        multicast_v6(self.0, false, multiaddr, interface)
    }

    pub fn set_ttl(&self, _ttl: u32) -> io::Result<()> {
        unsupported("Java does not expose the UDP unicast IP TTL")
    }

    pub fn ttl(&self) -> io::Result<u32> {
        unsupported("Java does not expose the UDP unicast IP TTL")
    }

    pub fn take_error(&self) -> io::Result<Option<io::Error>> {
        take_error(unsafe { jvm_udp_take_error(self.0) })
    }

    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        result_code(unsafe { jvm_udp_set_nonblocking(self.0, nonblocking) })
    }

    pub fn recv(&self, buffer: &mut [u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_udp_recv(self.0, buffer.as_mut_ptr(), buffer.len(), false) })
    }

    pub fn peek(&self, buffer: &mut [u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_udp_recv(self.0, buffer.as_mut_ptr(), buffer.len(), true) })
    }

    pub fn send(&self, buffer: &[u8]) -> io::Result<usize> {
        result_count(unsafe { jvm_udp_send(self.0, buffer.as_ptr(), buffer.len()) })
    }

    pub fn connect<A: ToSocketAddrs>(&self, addr: A) -> io::Result<()> {
        super::each_addr(addr, |address| {
            with_address(address, |bytes, length, port, scope| {
                result_code(unsafe { jvm_udp_connect(self.0, bytes, length, port, scope) })
            })
        })
    }
}

impl Drop for UdpSocket {
    fn drop(&mut self) {
        unsafe { jvm_udp_close(self.0) };
    }
}

impl fmt::Debug for UdpSocket {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = formatter.debug_struct("UdpSocket");
        if let Ok(address) = self.socket_addr() {
            debug.field("addr", &address);
        }
        if let Ok(address) = self.peer_addr() {
            debug.field("peer", &address);
        }
        debug.field("handle", &self.0).finish()
    }
}

pub struct LookupHost {
    handle: u64,
    index: usize,
    count: usize,
}

impl Iterator for LookupHost {
    type Item = SocketAddr;

    fn next(&mut self) -> Option<SocketAddr> {
        if self.index >= self.count {
            return None;
        }
        let index = self.index;
        self.index += 1;
        if unsafe { jvm_lookup_address(self.handle, index) } < 0 {
            return None;
        }
        last_address().ok()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.count - self.index;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for LookupHost {}

impl Drop for LookupHost {
    fn drop(&mut self) {
        unsafe { jvm_close_lookup(self.handle) };
    }
}

pub fn lookup_host(host: &str, port: u16) -> io::Result<LookupHost> {
    if host.as_bytes().contains(&0) {
        return Err(io::const_error!(
            io::ErrorKind::InvalidInput,
            "host name contained a nul byte"
        ));
    }
    let handle = result_handle(unsafe { jvm_lookup_host(host.as_ptr(), host.len(), port) })?;
    Ok(LookupHost {
        handle,
        index: 0,
        count: unsafe { jvm_lookup_count(handle) },
    })
}

fn connect_tcp(address: &SocketAddr, timeout: Option<Duration>) -> io::Result<TcpStream> {
    let (seconds, nanos) = timeout.map(duration_parts).unwrap_or((-1, 0));
    with_address(address, |bytes, length, port, scope| {
        result_handle(unsafe { jvm_tcp_connect(bytes, length, port, scope, seconds, nanos) })
            .map(TcpStream)
    })
}

fn set_tcp_timeout(handle: u64, read: bool, duration: Option<Duration>) -> io::Result<()> {
    let (seconds, nanos) = checked_timeout(duration)?;
    result_code(unsafe { jvm_tcp_set_timeout(handle, read, duration.is_some(), seconds, nanos) })
}

fn tcp_timeout(handle: u64, read: bool) -> io::Result<Option<Duration>> {
    let seconds = unsafe { jvm_tcp_timeout_seconds(handle, read) };
    if seconds < -1 {
        return Err(last_error());
    }
    if seconds == -1 {
        return Ok(None);
    }
    let nanos = unsafe { jvm_tcp_timeout_nanos(handle, read) };
    if nanos < 0 {
        Err(last_error())
    } else {
        Ok(Some(Duration::new(seconds as u64, nanos as u32)))
    }
}

fn set_udp_timeout(handle: u64, read: bool, duration: Option<Duration>) -> io::Result<()> {
    let (seconds, nanos) = checked_timeout(duration)?;
    result_code(unsafe { jvm_udp_set_timeout(handle, read, duration.is_some(), seconds, nanos) })
}

fn udp_timeout(handle: u64, read: bool) -> io::Result<Option<Duration>> {
    let seconds = unsafe { jvm_udp_timeout_seconds(handle, read) };
    if seconds < -1 {
        return Err(last_error());
    }
    if seconds == -1 {
        return Ok(None);
    }
    let nanos = unsafe { jvm_udp_timeout_nanos(handle, read) };
    if nanos < 0 {
        Err(last_error())
    } else {
        Ok(Some(Duration::new(seconds as u64, nanos as u32)))
    }
}

fn checked_timeout(duration: Option<Duration>) -> io::Result<(i64, u32)> {
    match duration {
        Some(duration) if duration.is_zero() => Err(io::Error::ZERO_TIMEOUT),
        Some(duration) => Ok(duration_parts(duration)),
        None => Ok((0, 0)),
    }
}

fn duration_parts(duration: Duration) -> (i64, u32) {
    (
        duration.as_secs().min(i64::MAX as u64) as i64,
        duration.subsec_nanos(),
    )
}

fn set_tcp_boolean(handle: u64, option: i32, value: bool) -> io::Result<()> {
    result_code(unsafe { jvm_tcp_set_boolean_option(handle, option, value) })
}

fn tcp_boolean(handle: u64, option: i32) -> io::Result<bool> {
    result_boolean(unsafe { jvm_tcp_boolean_option(handle, option) })
}

fn set_udp_boolean(handle: u64, option: i32, value: bool) -> io::Result<()> {
    result_code(unsafe { jvm_udp_set_boolean_option(handle, option, value) })
}

fn udp_boolean(handle: u64, option: i32) -> io::Result<bool> {
    result_boolean(unsafe { jvm_udp_boolean_option(handle, option) })
}

fn multicast_v4(
    handle: u64,
    join: bool,
    multiaddr: &Ipv4Addr,
    interface: &Ipv4Addr,
) -> io::Result<()> {
    let group = multiaddr.octets();
    let interface = interface.octets();
    result_code(unsafe {
        jvm_udp_multicast(
            handle,
            join,
            group.as_ptr(),
            group.len(),
            interface.as_ptr(),
            interface.len(),
            0,
        )
    })
}

fn multicast_v6(handle: u64, join: bool, multiaddr: &Ipv6Addr, interface: u32) -> io::Result<()> {
    let group = multiaddr.octets();
    result_code(unsafe {
        jvm_udp_multicast(
            handle,
            join,
            group.as_ptr(),
            group.len(),
            core::ptr::null(),
            0,
            interface,
        )
    })
}

fn with_address<T>(
    address: &SocketAddr,
    operation: impl FnOnce(*const u8, usize, u16, u32) -> T,
) -> T {
    match address {
        SocketAddr::V4(address) => {
            let bytes = address.ip().octets();
            operation(bytes.as_ptr(), bytes.len(), address.port(), 0)
        }
        SocketAddr::V6(address) => {
            let bytes = address.ip().octets();
            operation(
                bytes.as_ptr(),
                bytes.len(),
                address.port(),
                address.scope_id(),
            )
        }
    }
}

fn last_address() -> io::Result<SocketAddr> {
    let length = unsafe { jvm_last_address_length() };
    let port = unsafe { jvm_last_address_port() };
    match length {
        4 => {
            let mut bytes = [0; 4];
            unsafe { jvm_copy_last_address(bytes.as_mut_ptr()) };
            Ok(SocketAddr::V4(SocketAddrV4::new(
                Ipv4Addr::from(bytes),
                port,
            )))
        }
        16 => {
            let mut bytes = [0; 16];
            unsafe { jvm_copy_last_address(bytes.as_mut_ptr()) };
            Ok(SocketAddr::V6(SocketAddrV6::new(
                Ipv6Addr::from(bytes),
                port,
                0,
                unsafe { jvm_last_address_scope_id() },
            )))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "the JVM returned an invalid socket address",
        )),
    }
}

fn create_io_vector(count: usize, read: bool) -> io::Result<IoVector> {
    result_handle(unsafe { jvm_create_io_vector(count, read) }).map(IoVector)
}

fn take_error(result: i32) -> io::Result<Option<io::Error>> {
    match result {
        0 => Ok(None),
        1 => Ok(Some(last_error())),
        _ => Err(last_error()),
    }
}

fn result_code(code: i32) -> io::Result<()> {
    if code < 0 { Err(last_error()) } else { Ok(()) }
}

fn result_boolean(value: i32) -> io::Result<bool> {
    match value {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(last_error()),
    }
}

fn result_count(value: i64) -> io::Result<usize> {
    if value < 0 {
        Err(last_error())
    } else {
        usize::try_from(value)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "JVM byte count overflow"))
    }
}

fn result_handle(value: i64) -> io::Result<u64> {
    if value <= 0 {
        Err(last_error())
    } else {
        Ok(value as u64)
    }
}

fn last_error() -> io::Error {
    let length = unsafe { jvm_last_error_message_length() };
    let mut bytes = vec![0; length];
    unsafe { jvm_copy_last_error_message(bytes.as_mut_ptr()) };
    let message = unsafe { crate::string::String::from_utf8_unchecked(bytes) };
    io::Error::new(error_kind(unsafe { jvm_last_error_kind() }), message)
}

fn error_kind(kind: i32) -> io::ErrorKind {
    match kind {
        1 => io::ErrorKind::NotFound,
        2 => io::ErrorKind::PermissionDenied,
        3 => io::ErrorKind::AlreadyExists,
        4 => io::ErrorKind::InvalidInput,
        6 => io::ErrorKind::WouldBlock,
        12 => io::ErrorKind::Interrupted,
        13 => io::ErrorKind::Unsupported,
        15 => io::ErrorKind::ConnectionRefused,
        16 => io::ErrorKind::ConnectionReset,
        17 => io::ErrorKind::HostUnreachable,
        18 => io::ErrorKind::NetworkUnreachable,
        19 => io::ErrorKind::ConnectionAborted,
        20 => io::ErrorKind::NotConnected,
        21 => io::ErrorKind::AddrInUse,
        22 => io::ErrorKind::AddrNotAvailable,
        23 => io::ErrorKind::NetworkDown,
        24 => io::ErrorKind::BrokenPipe,
        25 => io::ErrorKind::TimedOut,
        _ => io::ErrorKind::Other,
    }
}

fn unsupported<T>(message: &'static str) -> io::Result<T> {
    Err(io::Error::new(io::ErrorKind::Unsupported, message))
}
