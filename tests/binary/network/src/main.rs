#![feature(tcp_keepalive, tcp_linger)]

use std::io::{self, IoSlice, IoSliceMut, Read, Write};
use std::net::{
    Ipv4Addr, Ipv6Addr, Shutdown, SocketAddr, TcpListener, TcpStream, ToSocketAddrs, UdpSocket,
};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

fn main() {
    test_address_lookup();
    test_tcp();
    test_udp();
    test_ipv6_when_available();
    println!("network support ok");
}

fn test_address_lookup() {
    let addresses = ("localhost", 12_345)
        .to_socket_addrs()
        .unwrap()
        .collect::<Vec<_>>();
    assert!(!addresses.is_empty());
    assert!(addresses.iter().all(|address| address.port() == 12_345));
    assert!(addresses.iter().any(|address| address.ip().is_loopback()));

    let error = ("invalid\0host", 80).to_socket_addrs().unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
}

fn test_tcp() {
    let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0)).unwrap();
    let listener_address = listener.local_addr().unwrap();
    assert!(listener_address.ip().is_loopback());
    assert_ne!(listener_address.port(), 0);
    assert!(format!("{listener:?}").contains("TcpListener"));
    assert!(listener.take_error().unwrap().is_none());

    listener.set_nonblocking(true).unwrap();
    assert_eq!(
        listener.accept().unwrap_err().kind(),
        io::ErrorKind::WouldBlock
    );
    listener.set_nonblocking(false).unwrap();

    assert_eq!(
        listener.set_ttl(42).unwrap_err().kind(),
        io::ErrorKind::Unsupported
    );
    assert_eq!(
        listener.ttl().unwrap_err().kind(),
        io::ErrorKind::Unsupported
    );

    let server_listener = listener.try_clone().unwrap();
    let (accepted_tx, accepted_rx) = mpsc::channel();
    let (read_ready_tx, read_ready_rx) = mpsc::channel();
    let server = thread::spawn(move || {
        let (mut stream, peer) = server_listener.accept().unwrap();
        assert!(peer.ip().is_loopback());
        assert_eq!(stream.local_addr().unwrap(), listener_address);
        assert_eq!(stream.peer_addr().unwrap(), peer);
        assert!(format!("{stream:?}").contains("TcpStream"));
        assert!(stream.take_error().unwrap().is_none());

        stream
            .set_read_timeout(Some(Duration::from_millis(125)))
            .unwrap();
        stream
            .set_write_timeout(Some(Duration::from_millis(250)))
            .unwrap();
        assert_eq!(
            stream.read_timeout().unwrap(),
            Some(Duration::from_millis(125))
        );
        assert_eq!(
            stream.write_timeout().unwrap(),
            Some(Duration::from_millis(250))
        );

        stream.set_nodelay(true).unwrap();
        assert!(stream.nodelay().unwrap());
        stream.set_keepalive(true).unwrap();
        assert!(stream.keepalive().unwrap());
        stream.set_linger(Some(Duration::from_secs(1))).unwrap();
        assert_eq!(stream.linger().unwrap(), Some(Duration::from_secs(1)));

        accepted_tx.send(()).unwrap();
        stream.set_nonblocking(true).unwrap();
        let mut empty_read = [0];
        assert_eq!(
            stream.read(&mut empty_read).unwrap_err().kind(),
            io::ErrorKind::WouldBlock
        );
        stream.set_nonblocking(false).unwrap();
        stream
            .set_read_timeout(Some(Duration::from_millis(20)))
            .unwrap();
        assert_eq!(
            stream.read(&mut empty_read).unwrap_err().kind(),
            io::ErrorKind::TimedOut
        );
        stream
            .set_read_timeout(Some(Duration::from_millis(125)))
            .unwrap();
        read_ready_tx.send(()).unwrap();

        let mut peeked = [0; 5];
        assert_eq!(stream.peek(&mut peeked).unwrap(), 5);
        assert_eq!(&peeked, b"hello");

        let mut first = [0; 2];
        let mut second = [0; 3];
        let mut buffers = [IoSliceMut::new(&mut first), IoSliceMut::new(&mut second)];
        assert_eq!(stream.read_vectored(&mut buffers).unwrap(), 5);
        assert_eq!(&first, b"he");
        assert_eq!(&second, b"llo");

        let mut remainder = [0; 6];
        stream.read_exact(&mut remainder).unwrap();
        assert_eq!(&remainder, b" world");

        let response = b"pong";
        let written = stream
            .write_vectored(&[IoSlice::new(&response[..2]), IoSlice::new(&response[2..])])
            .unwrap();
        assert!(written > 0 && written <= response.len());
        if written != response.len() {
            stream.write_all(&response[written..]).unwrap();
        }

        let mut cloned = stream.try_clone().unwrap();
        cloned.write_all(b"!").unwrap();
        stream.shutdown(Shutdown::Write).unwrap();
    });

    let mut client = TcpStream::connect_timeout(&listener_address, Duration::from_secs(2)).unwrap();
    accepted_rx.recv().unwrap();
    read_ready_rx.recv().unwrap();
    assert_eq!(client.peer_addr().unwrap(), listener_address);
    assert!(client.local_addr().unwrap().ip().is_loopback());

    client
        .set_read_timeout(Some(Duration::from_secs(2)))
        .unwrap();
    client
        .set_write_timeout(Some(Duration::from_secs(2)))
        .unwrap();
    client.write_all(b"hello world").unwrap();
    let mut response = [0; 5];
    client.read_exact(&mut response).unwrap();
    assert_eq!(&response, b"pong!");
    assert_eq!(
        client.set_ttl(42).unwrap_err().kind(),
        io::ErrorKind::Unsupported
    );
    client.set_read_timeout(None).unwrap();
    client.set_write_timeout(None).unwrap();
    assert_eq!(client.read_timeout().unwrap(), None);
    assert_eq!(client.write_timeout().unwrap(), None);

    server.join().unwrap();

    let simple_listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0)).unwrap();
    let simple_address = simple_listener.local_addr().unwrap();
    let simple_server = thread::spawn(move || {
        let (mut stream, _) = simple_listener.accept().unwrap();
        stream.write_all(b"x").unwrap();
    });
    let mut simple_client = TcpStream::connect(simple_address).unwrap();
    let mut byte = [0];
    simple_client.read_exact(&mut byte).unwrap();
    assert_eq!(byte, *b"x");
    simple_server.join().unwrap();
}

fn test_udp() {
    let receiver = UdpSocket::bind((Ipv4Addr::LOCALHOST, 0)).unwrap();
    let sender = UdpSocket::bind((Ipv4Addr::LOCALHOST, 0)).unwrap();
    let receiver_address = receiver.local_addr().unwrap();
    let sender_address = sender.local_addr().unwrap();
    assert!(format!("{receiver:?}").contains("UdpSocket"));
    assert!(receiver.take_error().unwrap().is_none());

    receiver
        .set_read_timeout(Some(Duration::from_millis(150)))
        .unwrap();
    receiver
        .set_write_timeout(Some(Duration::from_millis(275)))
        .unwrap();
    assert_eq!(
        receiver.read_timeout().unwrap(),
        Some(Duration::from_millis(150))
    );
    assert_eq!(
        receiver.write_timeout().unwrap(),
        Some(Duration::from_millis(275))
    );

    receiver.set_broadcast(true).unwrap();
    assert!(receiver.broadcast().unwrap());
    receiver.set_broadcast(false).unwrap();
    assert!(!receiver.broadcast().unwrap());
    receiver.set_multicast_loop_v4(false).unwrap();
    assert!(!receiver.multicast_loop_v4().unwrap());
    receiver.set_multicast_loop_v4(true).unwrap();
    assert!(receiver.multicast_loop_v4().unwrap());
    receiver.set_multicast_ttl_v4(7).unwrap();
    assert_eq!(receiver.multicast_ttl_v4().unwrap(), 7);

    let group = Ipv4Addr::new(239, 255, 0, 1);
    receiver
        .join_multicast_v4(&group, &Ipv4Addr::UNSPECIFIED)
        .unwrap();
    receiver
        .leave_multicast_v4(&group, &Ipv4Addr::UNSPECIFIED)
        .unwrap();

    assert_eq!(sender.send_to(b"datagram", receiver_address).unwrap(), 8);
    let mut short = [0; 3];
    let (peeked, source) = receiver.peek_from(&mut short).unwrap();
    assert_eq!(peeked, 3);
    assert_eq!(&short, b"dat");
    assert_eq!(source, sender_address);

    let mut full = [0; 16];
    let (received, source) = receiver.recv_from(&mut full).unwrap();
    assert_eq!(&full[..received], b"datagram");
    assert_eq!(source, sender_address);

    assert_eq!(sender.send_to(&[], receiver_address).unwrap(), 0);
    let (peeked, source) = receiver.peek_from(&mut []).unwrap();
    assert_eq!(peeked, 0);
    assert_eq!(source, sender_address);
    let (received, source) = receiver.recv_from(&mut []).unwrap();
    assert_eq!(received, 0);
    assert_eq!(source, sender_address);

    receiver.set_nonblocking(true).unwrap();
    assert_eq!(
        receiver.recv_from(&mut full).unwrap_err().kind(),
        io::ErrorKind::WouldBlock
    );
    receiver.set_nonblocking(false).unwrap();

    sender.connect(receiver_address).unwrap();
    receiver.connect(sender_address).unwrap();
    assert_eq!(sender.peer_addr().unwrap(), receiver_address);
    assert_eq!(receiver.peer_addr().unwrap(), sender_address);

    assert_eq!(sender.send(b"connected").unwrap(), 9);
    let mut prefix = [0; 4];
    assert_eq!(receiver.peek(&mut prefix).unwrap(), 4);
    assert_eq!(&prefix, b"conn");
    let received = receiver.recv(&mut full).unwrap();
    assert_eq!(&full[..received], b"connected");

    let cloned = sender.try_clone().unwrap();
    assert_eq!(cloned.send(b"clone").unwrap(), 5);
    let received = receiver.recv(&mut full).unwrap();
    assert_eq!(&full[..received], b"clone");

    receiver
        .set_read_timeout(Some(Duration::from_millis(20)))
        .unwrap();
    assert_eq!(
        receiver.recv(&mut full).unwrap_err().kind(),
        io::ErrorKind::TimedOut
    );
    assert_eq!(
        receiver.set_ttl(42).unwrap_err().kind(),
        io::ErrorKind::Unsupported
    );
    assert_eq!(
        receiver.ttl().unwrap_err().kind(),
        io::ErrorKind::Unsupported
    );
    receiver.set_read_timeout(None).unwrap();
    receiver.set_write_timeout(None).unwrap();
    assert_eq!(receiver.read_timeout().unwrap(), None);
    assert_eq!(receiver.write_timeout().unwrap(), None);
}

fn test_ipv6_when_available() {
    let Ok(socket) = UdpSocket::bind((Ipv6Addr::LOCALHOST, 0)) else {
        return;
    };
    assert!(matches!(socket.local_addr().unwrap(), SocketAddr::V6(_)));
    socket.set_multicast_loop_v6(false).unwrap();
    assert!(!socket.multicast_loop_v6().unwrap());
    socket.set_multicast_loop_v6(true).unwrap();
    assert!(socket.multicast_loop_v6().unwrap());

    let Ok(listener) = TcpListener::bind((Ipv6Addr::LOCALHOST, 0)) else {
        return;
    };
    assert!(matches!(listener.local_addr().unwrap(), SocketAddr::V6(_)));
    #[allow(deprecated)]
    {
        assert_eq!(
            listener.set_only_v6(true).unwrap_err().kind(),
            io::ErrorKind::Unsupported
        );
        assert_eq!(
            listener.only_v6().unwrap_err().kind(),
            io::ErrorKind::Unsupported
        );
    }
    let address = listener.local_addr().unwrap();
    let server = thread::spawn(move || {
        let (mut stream, _) = listener.accept().unwrap();
        stream.write_all(b"6").unwrap();
    });
    let mut client = TcpStream::connect_timeout(&address, Duration::from_secs(2)).unwrap();
    let mut byte = [0];
    client.read_exact(&mut byte).unwrap();
    assert_eq!(byte, *b"6");
    server.join().unwrap();
}
