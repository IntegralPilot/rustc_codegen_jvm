package org.rustlang.runtime;

import java.io.IOException;
import java.net.BindException;
import java.net.ConnectException;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.NoRouteToHostException;
import java.net.PortUnreachableException;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.StandardProtocolFamily;
import java.net.StandardSocketOptions;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.AlreadyConnectedException;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.MembershipKey;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.UnresolvedAddressException;
import java.nio.channels.UnsupportedAddressTypeException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/** Java NIO host services used by the JVM standard-library networking PAL. */
public final class NetworkSupport {
    public static final int ERROR_OTHER = 0;
    public static final int ERROR_NOT_FOUND = 1;
    public static final int ERROR_PERMISSION_DENIED = 2;
    public static final int ERROR_ALREADY_EXISTS = 3;
    public static final int ERROR_INVALID_INPUT = 4;
    public static final int ERROR_INVALID_DATA = 5;
    public static final int ERROR_WOULD_BLOCK = 6;
    public static final int ERROR_INTERRUPTED = 12;
    public static final int ERROR_UNSUPPORTED = 13;
    public static final int ERROR_CONNECTION_REFUSED = 15;
    public static final int ERROR_CONNECTION_RESET = 16;
    public static final int ERROR_HOST_UNREACHABLE = 17;
    public static final int ERROR_NETWORK_UNREACHABLE = 18;
    public static final int ERROR_CONNECTION_ABORTED = 19;
    public static final int ERROR_NOT_CONNECTED = 20;
    public static final int ERROR_ADDR_IN_USE = 21;
    public static final int ERROR_ADDR_NOT_AVAILABLE = 22;
    public static final int ERROR_NETWORK_DOWN = 23;
    public static final int ERROR_BROKEN_PIPE = 24;
    public static final int ERROR_TIMED_OUT = 25;

    private static final int OPTION_KEEPALIVE = 1;
    private static final int OPTION_NODELAY = 2;
    private static final int OPTION_BROADCAST = 3;
    private static final int OPTION_MULTICAST_LOOP_V4 = 4;
    private static final int OPTION_MULTICAST_LOOP_V6 = 5;
    private static final int OPTION_MULTICAST_TTL_V4 = 6;
    private static final int MAX_DATAGRAM_SIZE = 65_535;

    private static final AtomicLong NEXT_HANDLE = new AtomicLong(1);
    private static final ConcurrentHashMap<Long, TcpState> TCP_STREAMS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, ListenerState> TCP_LISTENERS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, UdpState> UDP_SOCKETS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, LookupState> LOOKUPS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, IoVector> IO_VECTORS =
            new ConcurrentHashMap<>();

    private static final ThreadLocal<ErrorState> LAST_ERROR =
            new ThreadLocal<ErrorState>() {
                @Override
                protected ErrorState initialValue() {
                    return new ErrorState(ERROR_OTHER, "network operation failed");
                }
            };
    private static final ThreadLocal<AddressState> LAST_ADDRESS =
            new ThreadLocal<AddressState>() {
                @Override
                protected AddressState initialValue() {
                    return AddressState.empty();
                }
            };
    private static final ThreadLocal<byte[]> LAST_HOSTNAME =
            new ThreadLocal<byte[]>() {
                @Override
                protected byte[] initialValue() {
                    return new byte[0];
                }
            };

    private static final class ErrorState {
        private final int kind;
        private final byte[] message;

        private ErrorState(int kind, String message) {
            this.kind = kind;
            this.message = message.getBytes(StandardCharsets.UTF_8);
        }
    }

    private static final class AddressState {
        private final byte[] address;
        private final char port;
        private final int scopeId;

        private AddressState(byte[] address, int port, int scopeId) {
            this.address = address;
            this.port = (char) port;
            this.scopeId = scopeId;
        }

        private static AddressState empty() {
            return new AddressState(new byte[0], 0, 0);
        }
    }

    private static final class Timeout {
        private static final Timeout NONE = new Timeout(false, 0, 0);

        private final boolean enabled;
        private final long seconds;
        private final int nanos;

        private Timeout(boolean enabled, long seconds, int nanos) {
            this.enabled = enabled;
            this.seconds = seconds;
            this.nanos = nanos;
        }

        private long saturatedNanos() {
            if (seconds >= Long.MAX_VALUE / 1_000_000_000L) {
                return Long.MAX_VALUE;
            }
            return seconds * 1_000_000_000L + nanos;
        }
    }

    private static final class WaitBudget {
        private final boolean enabled;
        private final long started;
        private final long durationNanos;

        private WaitBudget(Timeout timeout) {
            enabled = timeout.enabled;
            started = System.nanoTime();
            durationNanos = timeout.saturatedNanos();
        }

        private long remainingMillis() {
            if (!enabled) {
                return -1;
            }
            long elapsed = System.nanoTime() - started;
            if (elapsed < 0) {
                elapsed = Long.MAX_VALUE;
            }
            if (elapsed >= durationNanos) {
                return 0;
            }
            long remaining = durationNanos - elapsed;
            long millis = remaining / 1_000_000L;
            if (remaining % 1_000_000L != 0) {
                millis++;
            }
            return Math.max(1, millis);
        }
    }

    private static final class TcpState {
        private final SocketChannel channel;
        private final AtomicInteger references = new AtomicInteger(1);
        private final Object readLock = new Object();
        private final Object writeLock = new Object();
        private volatile boolean nonblocking;
        private volatile Timeout readTimeout = Timeout.NONE;
        private volatile Timeout writeTimeout = Timeout.NONE;
        private byte[] peeked;
        private int peekOffset;
        private int peekLength;

        private TcpState(SocketChannel channel) {
            this.channel = channel;
        }
    }

    private static final class ListenerState {
        private final ServerSocketChannel channel;
        private final AtomicInteger references = new AtomicInteger(1);
        private final Object acceptLock = new Object();
        private volatile boolean nonblocking;

        private ListenerState(ServerSocketChannel channel) {
            this.channel = channel;
        }
    }

    private static final class PendingDatagram {
        private final byte[] bytes;
        private final InetSocketAddress source;

        private PendingDatagram(byte[] bytes, InetSocketAddress source) {
            this.bytes = bytes;
            this.source = source;
        }
    }

    private static final class UdpState {
        private final DatagramChannel channel;
        private final AtomicInteger references = new AtomicInteger(1);
        private final Object readLock = new Object();
        private final Object writeLock = new Object();
        private final Map<String, MembershipKey> memberships = new HashMap<>();
        private volatile boolean nonblocking;
        private volatile Timeout readTimeout = Timeout.NONE;
        private volatile Timeout writeTimeout = Timeout.NONE;
        private PendingDatagram peeked;

        private UdpState(DatagramChannel channel) {
            this.channel = channel;
        }
    }

    private static final class LookupState {
        private final InetAddress[] addresses;
        private final char port;

        private LookupState(InetAddress[] addresses, char port) {
            this.addresses = addresses;
            this.port = port;
        }
    }

    private static final class IoVector {
        private final boolean read;
        private final Pointer[] pointers;
        private final byte[][] storage;
        private final ByteBuffer[] buffers;
        private int added;

        private IoVector(int count, boolean read) {
            this.read = read;
            pointers = new Pointer[count];
            storage = new byte[count][];
            buffers = new ByteBuffer[count];
        }
    }

    private NetworkSupport() {}

    public static int lastErrorKind() {
        return LAST_ERROR.get().kind;
    }

    public static long lastErrorMessageLength() {
        return LAST_ERROR.get().message.length;
    }

    public static void copyLastErrorMessage(Pointer destination) {
        RuntimeSupport.copyBytes(LAST_ERROR.get().message, destination);
    }

    public static long lastAddressLength() {
        return LAST_ADDRESS.get().address.length;
    }

    public static void copyLastAddress(Pointer destination) {
        RuntimeSupport.copyBytes(LAST_ADDRESS.get().address, destination);
    }

    public static char lastAddressPort() {
        return LAST_ADDRESS.get().port;
    }

    public static int lastAddressScopeId() {
        return LAST_ADDRESS.get().scopeId;
    }

    public static long hostnameLength() {
        try {
            byte[] hostname =
                    InetAddress.getLocalHost().getHostName().getBytes(StandardCharsets.UTF_8);
            LAST_HOSTNAME.set(hostname);
            return hostname.length;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static void copyHostname(Pointer destination) {
        RuntimeSupport.copyBytes(LAST_HOSTNAME.get(), destination);
    }

    public static long createIoVector(long count, boolean read) {
        try {
            int checkedCount = checkedLength(count);
            long handle = nextHandle();
            IO_VECTORS.put(handle, new IoVector(checkedCount, read));
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int addIoVectorBuffer(long handle, Pointer pointer, long length) {
        try {
            IoVector vector = ioVector(handle);
            if (vector.added >= vector.buffers.length) {
                throw new IllegalArgumentException("too many vectored I/O buffers");
            }
            int checkedLength = checkedLength(length);
            byte[] bytes =
                    vector.read
                            ? new byte[checkedLength]
                            : RuntimeSupport.copyFromPointer(pointer, checkedLength);
            vector.pointers[vector.added] = pointer;
            vector.storage[vector.added] = bytes;
            vector.buffers[vector.added] = ByteBuffer.wrap(bytes);
            vector.added++;
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static void closeIoVector(long handle) {
        IO_VECTORS.remove(handle);
    }

    public static long tcpConnect(
            Pointer addressBytes,
            long addressLength,
            char port,
            int scopeId,
            long timeoutSeconds,
            int timeoutNanos) {
        SocketChannel channel = null;
        try {
            InetSocketAddress address =
                    socketAddress(addressBytes, addressLength, port, scopeId);
            Timeout timeout =
                    timeoutSeconds < 0
                            ? Timeout.NONE
                            : checkedTimeout(true, timeoutSeconds, timeoutNanos);
            channel = SocketChannel.open();
            channel.configureBlocking(false);
            WaitBudget budget = new WaitBudget(timeout);
            if (!channel.connect(address)) {
                while (!channel.finishConnect()) {
                    if (!await(channel, SelectionKey.OP_CONNECT, budget)) {
                        throw new SocketTimeoutException("connection timed out");
                    }
                }
            }
            long handle = nextHandle();
            TCP_STREAMS.put(handle, new TcpState(channel));
            return handle;
        } catch (Exception error) {
            closeQuietly(channel);
            return fail(error);
        }
    }

    public static void tcpClose(long handle) {
        TcpState state = TCP_STREAMS.get(handle);
        if (state == null) {
            return;
        }
        synchronized (state) {
            if (state.references.decrementAndGet() == 0) {
                TCP_STREAMS.remove(handle, state);
                closeQuietly(state.channel);
            }
        }
    }

    public static long tcpDuplicate(long handle) {
        try {
            TcpState state = tcp(handle);
            synchronized (state) {
                if (!state.channel.isOpen()) {
                    throw new ClosedChannelException();
                }
                state.references.incrementAndGet();
            }
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long tcpRead(long handle, Pointer destination, long length) {
        try {
            TcpState state = tcp(handle);
            int checkedLength = checkedLength(length);
            if (checkedLength == 0) {
                return 0;
            }
            byte[] bytes = new byte[checkedLength];
            int count;
            synchronized (state.readLock) {
                ByteBuffer buffer = ByteBuffer.wrap(bytes);
                count = copyPeeked(state, buffer, true);
                if (count == 0) {
                    count = readTcp(state, buffer);
                }
            }
            RuntimeSupport.copyBytes(bytes, count, destination);
            return count;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long tcpReadVectored(long handle, long vectorHandle) {
        try {
            TcpState state = tcp(handle);
            IoVector vector = completeIoVector(vectorHandle, true);
            if (remaining(vector.buffers) == 0) {
                return 0;
            }
            long count;
            synchronized (state.readLock) {
                count = copyPeeked(state, vector.buffers, true);
                if (count == 0) {
                    count = readTcp(state, vector.buffers);
                }
            }
            copyReadVector(vector);
            return count;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long tcpPeek(long handle, Pointer destination, long length) {
        try {
            TcpState state = tcp(handle);
            int checkedLength = checkedLength(length);
            if (checkedLength == 0) {
                return 0;
            }
            byte[] result = new byte[checkedLength];
            int count;
            synchronized (state.readLock) {
                if (state.peekLength == 0) {
                    byte[] peeked = new byte[checkedLength];
                    ByteBuffer buffer = ByteBuffer.wrap(peeked);
                    int read = readTcp(state, buffer);
                    if (read == 0) {
                        return 0;
                    }
                    state.peeked = peeked;
                    state.peekOffset = 0;
                    state.peekLength = read;
                }
                count = Math.min(checkedLength, state.peekLength);
                System.arraycopy(state.peeked, state.peekOffset, result, 0, count);
            }
            RuntimeSupport.copyBytes(result, count, destination);
            return count;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long tcpWrite(long handle, Pointer source, long length) {
        try {
            TcpState state = tcp(handle);
            byte[] bytes = RuntimeSupport.copyFromPointer(source, length);
            if (bytes.length == 0) {
                return 0;
            }
            synchronized (state.writeLock) {
                return writeTcp(state, ByteBuffer.wrap(bytes));
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long tcpWriteVectored(long handle, long vectorHandle) {
        try {
            TcpState state = tcp(handle);
            IoVector vector = completeIoVector(vectorHandle, false);
            if (remaining(vector.buffers) == 0) {
                return 0;
            }
            synchronized (state.writeLock) {
                return writeTcp(state, vector.buffers);
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int tcpAddress(long handle, boolean peer) {
        try {
            TcpState state = tcp(handle);
            storeAddress(peer ? state.channel.getRemoteAddress() : state.channel.getLocalAddress());
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int tcpShutdown(long handle, int how) {
        try {
            TcpState state = tcp(handle);
            if (how == 0 || how == 2) {
                state.channel.socket().shutdownInput();
            }
            if (how == 1 || how == 2) {
                state.channel.socket().shutdownOutput();
            }
            if (how < 0 || how > 2) {
                throw new IllegalArgumentException("invalid socket shutdown mode " + how);
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int tcpSetTimeout(
            long handle,
            boolean read,
            boolean enabled,
            long seconds,
            int nanos) {
        try {
            TcpState state = tcp(handle);
            Timeout timeout = checkedTimeout(enabled, seconds, nanos);
            if (read) {
                state.readTimeout = timeout;
            } else {
                state.writeTimeout = timeout;
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long tcpTimeoutSeconds(long handle, boolean read) {
        try {
            Timeout timeout = read ? tcp(handle).readTimeout : tcp(handle).writeTimeout;
            return timeout.enabled ? timeout.seconds : -1;
        } catch (Exception error) {
            return fail(error) - 1;
        }
    }

    public static int tcpTimeoutNanos(long handle, boolean read) {
        try {
            Timeout timeout = read ? tcp(handle).readTimeout : tcp(handle).writeTimeout;
            return timeout.enabled ? timeout.nanos : 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int tcpSetBooleanOption(long handle, int option, boolean value) {
        try {
            TcpState state = tcp(handle);
            if (option == OPTION_KEEPALIVE) {
                state.channel.setOption(StandardSocketOptions.SO_KEEPALIVE, value);
            } else if (option == OPTION_NODELAY) {
                state.channel.setOption(StandardSocketOptions.TCP_NODELAY, value);
            } else {
                throw new IllegalArgumentException("invalid TCP boolean option " + option);
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int tcpBooleanOption(long handle, int option) {
        try {
            TcpState state = tcp(handle);
            if (option == OPTION_KEEPALIVE) {
                return state.channel.getOption(StandardSocketOptions.SO_KEEPALIVE) ? 1 : 0;
            }
            if (option == OPTION_NODELAY) {
                return state.channel.getOption(StandardSocketOptions.TCP_NODELAY) ? 1 : 0;
            }
            throw new IllegalArgumentException("invalid TCP boolean option " + option);
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int tcpSetLinger(long handle, boolean enabled, int seconds) {
        try {
            if (enabled && seconds < 0) {
                throw new IllegalArgumentException("negative TCP linger duration");
            }
            tcp(handle)
                    .channel
                    .setOption(StandardSocketOptions.SO_LINGER, enabled ? seconds : -1);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long tcpLinger(long handle) {
        try {
            return tcp(handle).channel.getOption(StandardSocketOptions.SO_LINGER);
        } catch (Exception error) {
            return fail(error) - 1;
        }
    }

    public static int tcpTakeError(long handle) {
        try {
            tcp(handle);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int tcpSetNonblocking(long handle, boolean nonblocking) {
        try {
            tcp(handle).nonblocking = nonblocking;
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long listenerBind(
            Pointer addressBytes, long addressLength, char port, int scopeId) {
        ServerSocketChannel channel = null;
        try {
            InetSocketAddress address =
                    socketAddress(addressBytes, addressLength, port, scopeId);
            channel = ServerSocketChannel.open();
            channel.configureBlocking(false);
            if (java.io.File.separatorChar != '\\') {
                channel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
            }
            channel.bind(address, 128);
            long handle = nextHandle();
            TCP_LISTENERS.put(handle, new ListenerState(channel));
            return handle;
        } catch (Exception error) {
            closeQuietly(channel);
            return fail(error);
        }
    }

    public static void listenerClose(long handle) {
        ListenerState state = TCP_LISTENERS.get(handle);
        if (state == null) {
            return;
        }
        synchronized (state) {
            if (state.references.decrementAndGet() == 0) {
                TCP_LISTENERS.remove(handle, state);
                closeQuietly(state.channel);
            }
        }
    }

    public static long listenerDuplicate(long handle) {
        try {
            ListenerState state = listener(handle);
            synchronized (state) {
                if (!state.channel.isOpen()) {
                    throw new ClosedChannelException();
                }
                state.references.incrementAndGet();
            }
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int listenerAddress(long handle) {
        try {
            storeAddress(listener(handle).channel.getLocalAddress());
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long listenerAccept(long handle) {
        SocketChannel accepted = null;
        try {
            ListenerState state = listener(handle);
            synchronized (state.acceptLock) {
                while ((accepted = state.channel.accept()) == null) {
                    if (state.nonblocking) {
                        return failWouldBlock("TCP accept would block");
                    }
                    if (!await(
                            state.channel,
                            SelectionKey.OP_ACCEPT,
                            new WaitBudget(Timeout.NONE))) {
                        continue;
                    }
                }
            }
            accepted.configureBlocking(false);
            storeAddress(accepted.getRemoteAddress());
            long streamHandle = nextHandle();
            TCP_STREAMS.put(streamHandle, new TcpState(accepted));
            return streamHandle;
        } catch (Exception error) {
            closeQuietly(accepted);
            return fail(error);
        }
    }

    public static int listenerTakeError(long handle) {
        try {
            listener(handle);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int listenerSetNonblocking(long handle, boolean nonblocking) {
        try {
            listener(handle).nonblocking = nonblocking;
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long udpBind(
            Pointer addressBytes, long addressLength, char port, int scopeId) {
        DatagramChannel channel = null;
        try {
            InetSocketAddress address =
                    socketAddress(addressBytes, addressLength, port, scopeId);
            channel =
                    DatagramChannel.open(
                            address.getAddress() instanceof Inet6Address
                                    ? StandardProtocolFamily.INET6
                                    : StandardProtocolFamily.INET);
            channel.configureBlocking(false);
            channel.bind(address);
            long handle = nextHandle();
            UDP_SOCKETS.put(handle, new UdpState(channel));
            return handle;
        } catch (Exception error) {
            closeQuietly(channel);
            return fail(error);
        }
    }

    public static void udpClose(long handle) {
        UdpState state = UDP_SOCKETS.get(handle);
        if (state == null) {
            return;
        }
        synchronized (state) {
            if (state.references.decrementAndGet() == 0) {
                UDP_SOCKETS.remove(handle, state);
                synchronized (state.memberships) {
                    for (MembershipKey key : state.memberships.values()) {
                        key.drop();
                    }
                    state.memberships.clear();
                }
                closeQuietly(state.channel);
            }
        }
    }

    public static long udpDuplicate(long handle) {
        try {
            UdpState state = udp(handle);
            synchronized (state) {
                if (!state.channel.isOpen()) {
                    throw new ClosedChannelException();
                }
                state.references.incrementAndGet();
            }
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int udpAddress(long handle, boolean peer) {
        try {
            UdpState state = udp(handle);
            SocketAddress address =
                    peer ? state.channel.getRemoteAddress() : state.channel.getLocalAddress();
            if (address == null) {
                throw new NotYetConnectedException();
            }
            storeAddress(address);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long udpRecvFrom(
            long handle, Pointer destination, long length, boolean peek) {
        try {
            UdpState state = udp(handle);
            int checkedLength = checkedLength(length);
            byte[] result;
            InetSocketAddress source;
            synchronized (state.readLock) {
                PendingDatagram datagram = receiveDatagram(state, peek);
                source = datagram.source;
                int count = Math.min(checkedLength, datagram.bytes.length);
                result = Arrays.copyOf(datagram.bytes, count);
                if (!peek) {
                    state.peeked = null;
                }
            }
            RuntimeSupport.copyBytes(result, destination);
            storeAddress(source);
            return result.length;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long udpSendTo(
            long handle,
            Pointer source,
            long length,
            Pointer addressBytes,
            long addressLength,
            char port,
            int scopeId) {
        try {
            UdpState state = udp(handle);
            byte[] bytes = RuntimeSupport.copyFromPointer(source, length);
            InetSocketAddress address =
                    socketAddress(addressBytes, addressLength, port, scopeId);
            synchronized (state.writeLock) {
                return sendDatagram(state, ByteBuffer.wrap(bytes), address);
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int udpSetTimeout(
            long handle,
            boolean read,
            boolean enabled,
            long seconds,
            int nanos) {
        try {
            UdpState state = udp(handle);
            Timeout timeout = checkedTimeout(enabled, seconds, nanos);
            if (read) {
                state.readTimeout = timeout;
            } else {
                state.writeTimeout = timeout;
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long udpTimeoutSeconds(long handle, boolean read) {
        try {
            Timeout timeout = read ? udp(handle).readTimeout : udp(handle).writeTimeout;
            return timeout.enabled ? timeout.seconds : -1;
        } catch (Exception error) {
            return fail(error) - 1;
        }
    }

    public static int udpTimeoutNanos(long handle, boolean read) {
        try {
            Timeout timeout = read ? udp(handle).readTimeout : udp(handle).writeTimeout;
            return timeout.enabled ? timeout.nanos : 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int udpSetBooleanOption(long handle, int option, boolean value) {
        try {
            UdpState state = udp(handle);
            if (option == OPTION_BROADCAST) {
                state.channel.setOption(StandardSocketOptions.SO_BROADCAST, value);
            } else if (option == OPTION_MULTICAST_LOOP_V4
                    || option == OPTION_MULTICAST_LOOP_V6) {
                state.channel.setOption(StandardSocketOptions.IP_MULTICAST_LOOP, value);
            } else {
                throw new IllegalArgumentException("invalid UDP boolean option " + option);
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int udpBooleanOption(long handle, int option) {
        try {
            UdpState state = udp(handle);
            if (option == OPTION_BROADCAST) {
                return state.channel.getOption(StandardSocketOptions.SO_BROADCAST) ? 1 : 0;
            }
            if (option == OPTION_MULTICAST_LOOP_V4
                    || option == OPTION_MULTICAST_LOOP_V6) {
                return state.channel.getOption(StandardSocketOptions.IP_MULTICAST_LOOP) ? 1 : 0;
            }
            throw new IllegalArgumentException("invalid UDP boolean option " + option);
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int udpSetIntegerOption(long handle, int option, int value) {
        try {
            if (option != OPTION_MULTICAST_TTL_V4) {
                throw new IllegalArgumentException("invalid UDP integer option " + option);
            }
            if (value < 0 || value > 255) {
                throw new IllegalArgumentException("multicast TTL must be between 0 and 255");
            }
            udp(handle).channel.setOption(StandardSocketOptions.IP_MULTICAST_TTL, value);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long udpIntegerOption(long handle, int option) {
        try {
            if (option != OPTION_MULTICAST_TTL_V4) {
                throw new IllegalArgumentException("invalid UDP integer option " + option);
            }
            return udp(handle).channel.getOption(StandardSocketOptions.IP_MULTICAST_TTL);
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int udpMulticast(
            long handle,
            boolean join,
            Pointer addressBytes,
            long addressLength,
            Pointer interfaceBytes,
            long interfaceLength,
            int interfaceIndex) {
        try {
            UdpState state = udp(handle);
            InetAddress group = inetAddress(addressBytes, addressLength, 0);
            NetworkInterface networkInterface =
                    multicastInterface(
                            group, interfaceBytes, interfaceLength, interfaceIndex);
            String key = membershipKey(group, networkInterface);
            synchronized (state.memberships) {
                if (join) {
                    MembershipKey existing = state.memberships.get(key);
                    if (existing != null && existing.isValid()) {
                        return 0;
                    }
                    MembershipKey membership = state.channel.join(group, networkInterface);
                    state.memberships.put(key, membership);
                } else {
                    MembershipKey membership = state.memberships.remove(key);
                    if (membership == null || !membership.isValid()) {
                        throw new IllegalArgumentException(
                                "socket has not joined the requested multicast group");
                    }
                    membership.drop();
                }
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int udpTakeError(long handle) {
        try {
            udp(handle);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int udpSetNonblocking(long handle, boolean nonblocking) {
        try {
            udp(handle).nonblocking = nonblocking;
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int udpConnect(
            long handle,
            Pointer addressBytes,
            long addressLength,
            char port,
            int scopeId) {
        try {
            UdpState state = udp(handle);
            InetSocketAddress address =
                    socketAddress(addressBytes, addressLength, port, scopeId);
            synchronized (state.readLock) {
                synchronized (state.writeLock) {
                    if (state.channel.isConnected()) {
                        state.channel.disconnect();
                    }
                    state.channel.connect(address);
                    state.peeked = null;
                }
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long udpRecv(long handle, Pointer destination, long length, boolean peek) {
        try {
            UdpState state = udp(handle);
            if (!state.channel.isConnected()) {
                throw new NotYetConnectedException();
            }
            return udpRecvFrom(handle, destination, length, peek);
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long udpSend(long handle, Pointer source, long length) {
        try {
            UdpState state = udp(handle);
            if (!state.channel.isConnected()) {
                throw new NotYetConnectedException();
            }
            byte[] bytes = RuntimeSupport.copyFromPointer(source, length);
            synchronized (state.writeLock) {
                return sendDatagram(state, ByteBuffer.wrap(bytes), null);
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long lookupHost(Pointer hostBytes, long hostLength, char port) {
        try {
            String host =
                    new String(
                            RuntimeSupport.copyFromPointer(hostBytes, hostLength),
                            StandardCharsets.UTF_8);
            InetAddress[] addresses = InetAddress.getAllByName(host);
            long handle = nextHandle();
            LOOKUPS.put(handle, new LookupState(addresses, port));
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long lookupCount(long handle) {
        try {
            return lookup(handle).addresses.length;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int lookupAddress(long handle, long index) {
        try {
            LookupState lookup = lookup(handle);
            int checkedIndex = checkedLength(index);
            if (checkedIndex >= lookup.addresses.length) {
                throw new IndexOutOfBoundsException("network lookup index " + index);
            }
            storeAddress(
                    new InetSocketAddress(
                            lookup.addresses[checkedIndex], lookup.port & 0xffff));
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static void closeLookup(long handle) {
        LOOKUPS.remove(handle);
    }

    private static int readTcp(TcpState state, ByteBuffer buffer) throws IOException {
        WaitBudget budget = new WaitBudget(state.readTimeout);
        while (true) {
            int read = state.channel.read(buffer);
            if (read < 0) {
                return 0;
            }
            if (read != 0) {
                return read;
            }
            if (state.nonblocking) {
                throw wouldBlock("TCP read would block");
            }
            if (!await(state.channel, SelectionKey.OP_READ, budget)) {
                throw new SocketTimeoutException("TCP read timed out");
            }
        }
    }

    private static long readTcp(TcpState state, ByteBuffer[] buffers) throws IOException {
        WaitBudget budget = new WaitBudget(state.readTimeout);
        while (true) {
            long read = state.channel.read(buffers);
            if (read < 0) {
                return 0;
            }
            if (read != 0) {
                return read;
            }
            if (state.nonblocking) {
                throw wouldBlock("TCP vectored read would block");
            }
            if (!await(state.channel, SelectionKey.OP_READ, budget)) {
                throw new SocketTimeoutException("TCP vectored read timed out");
            }
        }
    }

    private static int writeTcp(TcpState state, ByteBuffer buffer) throws IOException {
        WaitBudget budget = new WaitBudget(state.writeTimeout);
        while (true) {
            int written = state.channel.write(buffer);
            if (written != 0) {
                return written;
            }
            if (state.nonblocking) {
                throw wouldBlock("TCP write would block");
            }
            if (!await(state.channel, SelectionKey.OP_WRITE, budget)) {
                throw new SocketTimeoutException("TCP write timed out");
            }
        }
    }

    private static long writeTcp(TcpState state, ByteBuffer[] buffers) throws IOException {
        WaitBudget budget = new WaitBudget(state.writeTimeout);
        while (true) {
            long written = state.channel.write(buffers);
            if (written != 0) {
                return written;
            }
            if (state.nonblocking) {
                throw wouldBlock("TCP vectored write would block");
            }
            if (!await(state.channel, SelectionKey.OP_WRITE, budget)) {
                throw new SocketTimeoutException("TCP vectored write timed out");
            }
        }
    }

    private static int copyPeeked(TcpState state, ByteBuffer destination, boolean consume) {
        if (state.peekLength == 0) {
            return 0;
        }
        int count = Math.min(destination.remaining(), state.peekLength);
        destination.put(state.peeked, state.peekOffset, count);
        if (consume) {
            state.peekOffset += count;
            state.peekLength -= count;
            if (state.peekLength == 0) {
                state.peeked = null;
                state.peekOffset = 0;
            }
        }
        return count;
    }

    private static long copyPeeked(
            TcpState state, ByteBuffer[] destinations, boolean consume) {
        long count = 0;
        for (ByteBuffer destination : destinations) {
            count += copyPeeked(state, destination, consume);
            if (state.peekLength == 0) {
                break;
            }
        }
        return count;
    }

    private static PendingDatagram receiveDatagram(UdpState state, boolean peek)
            throws IOException {
        if (state.peeked != null) {
            return state.peeked;
        }
        ByteBuffer buffer = ByteBuffer.allocate(MAX_DATAGRAM_SIZE);
        WaitBudget budget = new WaitBudget(state.readTimeout);
        while (true) {
            SocketAddress source = state.channel.receive(buffer);
            if (source != null) {
                PendingDatagram datagram =
                        new PendingDatagram(
                                Arrays.copyOf(buffer.array(), buffer.position()),
                                checkedSocketAddress(source));
                if (peek) {
                    state.peeked = datagram;
                }
                return datagram;
            }
            if (state.nonblocking) {
                throw wouldBlock("UDP receive would block");
            }
            if (!await(state.channel, SelectionKey.OP_READ, budget)) {
                throw new SocketTimeoutException("UDP receive timed out");
            }
        }
    }

    private static int sendDatagram(
            UdpState state, ByteBuffer buffer, InetSocketAddress destination)
            throws IOException {
        WaitBudget budget = new WaitBudget(state.writeTimeout);
        while (true) {
            int written =
                    destination == null
                            ? state.channel.write(buffer)
                            : state.channel.send(buffer, destination);
            if (written != 0 || !buffer.hasRemaining()) {
                return written;
            }
            if (state.nonblocking) {
                throw wouldBlock("UDP send would block");
            }
            if (!await(state.channel, SelectionKey.OP_WRITE, budget)) {
                throw new SocketTimeoutException("UDP send timed out");
            }
        }
    }

    private static boolean await(
            SelectableChannel channel, int operation, WaitBudget budget) throws IOException {
        try (Selector selector = Selector.open()) {
            channel.register(selector, operation);
            while (true) {
                long millis = budget.remainingMillis();
                if (millis == 0) {
                    return false;
                }
                int selected = millis < 0 ? selector.select() : selector.select(millis);
                if (selected != 0) {
                    return true;
                }
                if (millis < 0) {
                    continue;
                }
            }
        }
    }

    private static Timeout checkedTimeout(boolean enabled, long seconds, int nanos) {
        if (!enabled) {
            return Timeout.NONE;
        }
        if (seconds < 0 || nanos < 0 || nanos >= 1_000_000_000) {
            throw new IllegalArgumentException("invalid network timeout");
        }
        if (seconds == 0 && nanos == 0) {
            throw new IllegalArgumentException("network timeout must not be zero");
        }
        return new Timeout(true, seconds, nanos);
    }

    private static InetSocketAddress socketAddress(
            Pointer bytes, long length, char port, int scopeId) throws UnknownHostException {
        return new InetSocketAddress(inetAddress(bytes, length, scopeId), port & 0xffff);
    }

    private static InetAddress inetAddress(Pointer pointer, long length, int scopeId)
            throws UnknownHostException {
        byte[] bytes = RuntimeSupport.copyFromPointer(pointer, length);
        if (bytes.length == 4) {
            return InetAddress.getByAddress(bytes);
        }
        if (bytes.length == 16) {
            return Inet6Address.getByAddress(null, bytes, scopeId);
        }
        throw new IllegalArgumentException("IP address must contain 4 or 16 bytes");
    }

    private static void storeAddress(SocketAddress address) {
        InetSocketAddress inet = checkedSocketAddress(address);
        InetAddress ip = inet.getAddress();
        if (ip == null) {
            throw new UnresolvedAddressException();
        }
        int scopeId = ip instanceof Inet6Address ? ((Inet6Address) ip).getScopeId() : 0;
        LAST_ADDRESS.set(new AddressState(ip.getAddress(), inet.getPort(), scopeId));
    }

    private static InetSocketAddress checkedSocketAddress(SocketAddress address) {
        if (!(address instanceof InetSocketAddress)) {
            throw new UnsupportedAddressTypeException();
        }
        return (InetSocketAddress) address;
    }

    private static NetworkInterface multicastInterface(
            InetAddress group,
            Pointer interfaceBytes,
            long interfaceLength,
            int interfaceIndex)
            throws IOException {
        NetworkInterface result = null;
        if (interfaceLength != 0) {
            InetAddress address = inetAddress(interfaceBytes, interfaceLength, 0);
            if (!address.isAnyLocalAddress()) {
                result = NetworkInterface.getByInetAddress(address);
            }
        } else if (interfaceIndex != 0) {
            if (interfaceIndex < 0) {
                throw new IllegalArgumentException("multicast interface index is out of range");
            }
            result = NetworkInterface.getByIndex(interfaceIndex);
        }
        if (result == null) {
            result = defaultMulticastInterface(group instanceof Inet6Address);
        }
        if (result == null) {
            throw new SocketException("no multicast-capable network interface is available");
        }
        return result;
    }

    private static NetworkInterface defaultMulticastInterface(boolean ipv6)
            throws SocketException {
        NetworkInterface loopback = null;
        Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
        while (interfaces != null && interfaces.hasMoreElements()) {
            NetworkInterface candidate = interfaces.nextElement();
            if (!candidate.isUp() || !candidate.supportsMulticast()) {
                continue;
            }
            boolean matchingAddress = false;
            Enumeration<InetAddress> addresses = candidate.getInetAddresses();
            while (addresses.hasMoreElements()) {
                InetAddress address = addresses.nextElement();
                if ((ipv6 && address instanceof Inet6Address)
                        || (!ipv6 && address instanceof Inet4Address)) {
                    matchingAddress = true;
                    break;
                }
            }
            if (!matchingAddress) {
                continue;
            }
            if (!candidate.isLoopback()) {
                return candidate;
            }
            loopback = candidate;
        }
        return loopback;
    }

    private static String membershipKey(InetAddress group, NetworkInterface networkInterface) {
        return Arrays.toString(group.getAddress()) + "%" + networkInterface.getIndex();
    }

    private static IoVector completeIoVector(long handle, boolean read) {
        IoVector vector = ioVector(handle);
        if (vector.read != read) {
            throw new IllegalArgumentException("vectored I/O direction mismatch");
        }
        if (vector.added != vector.buffers.length) {
            throw new IllegalArgumentException("incomplete vectored I/O buffer");
        }
        return vector;
    }

    private static void copyReadVector(IoVector vector) {
        for (int index = 0; index < vector.buffers.length; index++) {
            RuntimeSupport.copyBytes(
                    vector.storage[index],
                    vector.buffers[index].position(),
                    vector.pointers[index]);
        }
    }

    private static long remaining(ByteBuffer[] buffers) {
        long remaining = 0;
        for (ByteBuffer buffer : buffers) {
            remaining += buffer.remaining();
        }
        return remaining;
    }

    private static int checkedLength(long value) {
        int checked = Math.toIntExact(value);
        if (checked < 0) {
            throw new IllegalArgumentException("negative network buffer length");
        }
        return checked;
    }

    private static long nextHandle() {
        long handle = NEXT_HANDLE.getAndIncrement();
        if (handle <= 0) {
            throw new IllegalStateException("JVM network handle space exhausted");
        }
        return handle;
    }

    private static TcpState tcp(long handle) {
        TcpState state = TCP_STREAMS.get(handle);
        if (state == null) {
            throw new IllegalStateException("unknown JVM TCP stream " + handle);
        }
        return state;
    }

    private static ListenerState listener(long handle) {
        ListenerState state = TCP_LISTENERS.get(handle);
        if (state == null) {
            throw new IllegalStateException("unknown JVM TCP listener " + handle);
        }
        return state;
    }

    private static UdpState udp(long handle) {
        UdpState state = UDP_SOCKETS.get(handle);
        if (state == null) {
            throw new IllegalStateException("unknown JVM UDP socket " + handle);
        }
        return state;
    }

    private static LookupState lookup(long handle) {
        LookupState state = LOOKUPS.get(handle);
        if (state == null) {
            throw new IllegalStateException("unknown JVM DNS lookup " + handle);
        }
        return state;
    }

    private static IoVector ioVector(long handle) {
        IoVector vector = IO_VECTORS.get(handle);
        if (vector == null) {
            throw new IllegalStateException("unknown JVM network I/O vector " + handle);
        }
        return vector;
    }

    private static IOException wouldBlock(String message) {
        return new WouldBlockException(message);
    }

    private static long failWouldBlock(String message) {
        setError(ERROR_WOULD_BLOCK, message);
        return -1;
    }

    private static long fail(Exception error) {
        setError(errorKind(error), errorMessage(error));
        return -1;
    }

    private static int failInt(Exception error) {
        setError(errorKind(error), errorMessage(error));
        return -1;
    }

    private static void setError(int kind, String message) {
        LAST_ERROR.set(new ErrorState(kind, message));
    }

    private static String errorMessage(Exception error) {
        String message = error.getMessage();
        return message == null || message.isEmpty()
                ? error.getClass().getSimpleName()
                : message;
    }

    private static int errorKind(Exception error) {
        if (error instanceof WouldBlockException) {
            return ERROR_WOULD_BLOCK;
        }
        if (error instanceof SecurityException) {
            return ERROR_PERMISSION_DENIED;
        }
        if (error instanceof SocketTimeoutException) {
            return ERROR_TIMED_OUT;
        }
        if (error instanceof ClosedByInterruptException
                || error instanceof InterruptedException) {
            return ERROR_INTERRUPTED;
        }
        if (error instanceof BindException) {
            String message = errorMessage(error).toLowerCase(Locale.ROOT);
            if (message.contains("permission") || message.contains("access denied")) {
                return ERROR_PERMISSION_DENIED;
            }
            return message.contains("in use") ? ERROR_ADDR_IN_USE : ERROR_ADDR_NOT_AVAILABLE;
        }
        if (error instanceof ConnectException || error instanceof PortUnreachableException) {
            return ERROR_CONNECTION_REFUSED;
        }
        if (error instanceof NoRouteToHostException) {
            return ERROR_HOST_UNREACHABLE;
        }
        if (error instanceof UnknownHostException) {
            return ERROR_ADDR_NOT_AVAILABLE;
        }
        if (error instanceof NotYetConnectedException) {
            return ERROR_NOT_CONNECTED;
        }
        if (error instanceof AlreadyConnectedException) {
            return ERROR_ALREADY_EXISTS;
        }
        if (error instanceof ClosedChannelException) {
            return ERROR_NOT_CONNECTED;
        }
        if (error instanceof UnsupportedOperationException) {
            return ERROR_UNSUPPORTED;
        }
        if (error instanceof IllegalArgumentException
                || error instanceof UnresolvedAddressException
                || error instanceof UnsupportedAddressTypeException
                || error instanceof IndexOutOfBoundsException) {
            return ERROR_INVALID_INPUT;
        }
        if (error instanceof SocketException) {
            String message = errorMessage(error).toLowerCase(Locale.ROOT);
            if (message.contains("permission") || message.contains("access denied")) {
                return ERROR_PERMISSION_DENIED;
            }
            if (message.contains("reset")) {
                return ERROR_CONNECTION_RESET;
            }
            if (message.contains("broken pipe")) {
                return ERROR_BROKEN_PIPE;
            }
            if (message.contains("network is unreachable")) {
                return ERROR_NETWORK_UNREACHABLE;
            }
            if (message.contains("network is down")) {
                return ERROR_NETWORK_DOWN;
            }
            if (message.contains("not connected") || message.contains("closed")) {
                return ERROR_NOT_CONNECTED;
            }
        }
        return ERROR_OTHER;
    }

    private static void closeQuietly(java.nio.channels.Channel channel) {
        if (channel == null) {
            return;
        }
        try {
            channel.close();
        } catch (IOException ignored) {
            // Rust Drop cannot report close failures.
        }
    }

    private static final class WouldBlockException extends IOException {
        private static final long serialVersionUID = 1L;

        private WouldBlockException(String message) {
            super(message);
        }
    }
}
