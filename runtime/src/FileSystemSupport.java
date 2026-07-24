package org.rustlang.runtime;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.NonReadableChannelException;
import java.nio.channels.NonWritableChannelException;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystemException;
import java.nio.file.FileSystemLoopException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.NotDirectoryException;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.ReadOnlyFileSystemException;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributeView;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.DosFileAttributeView;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/** Java NIO host services used by the JVM standard-library filesystem PAL. */
public final class FileSystemSupport {
    public static final int ERROR_OTHER = 0;
    public static final int ERROR_NOT_FOUND = 1;
    public static final int ERROR_PERMISSION_DENIED = 2;
    public static final int ERROR_ALREADY_EXISTS = 3;
    public static final int ERROR_INVALID_INPUT = 4;
    public static final int ERROR_INVALID_DATA = 5;
    public static final int ERROR_WOULD_BLOCK = 6;
    public static final int ERROR_NOT_A_DIRECTORY = 7;
    public static final int ERROR_IS_A_DIRECTORY = 8;
    public static final int ERROR_DIRECTORY_NOT_EMPTY = 9;
    public static final int ERROR_READ_ONLY_FILESYSTEM = 10;
    public static final int ERROR_FILESYSTEM_LOOP = 11;
    public static final int ERROR_INTERRUPTED = 12;
    public static final int ERROR_UNSUPPORTED = 13;
    public static final int ERROR_CROSSES_DEVICES = 14;

    private static final int OPTION_READ = 1;
    private static final int OPTION_WRITE = 1 << 1;
    private static final int OPTION_APPEND = 1 << 2;
    private static final int OPTION_TRUNCATE = 1 << 3;
    private static final int OPTION_CREATE = 1 << 4;
    private static final int OPTION_CREATE_NEW = 1 << 5;

    private static final int TYPE_FILE = 1;
    private static final int TYPE_DIRECTORY = 2;
    private static final int TYPE_SYMLINK = 3;
    private static final int TYPE_OTHER = 4;

    private static final AtomicLong NEXT_HANDLE = new AtomicLong(1);
    private static final ConcurrentHashMap<Long, OpenFile> FILES = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, MetadataSnapshot> METADATA =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, Object> FILE_KEYS = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, DirectorySnapshot> DIRECTORIES =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Long, IoVector> IO_VECTORS = new ConcurrentHashMap<>();
    private static final ThreadLocal<ErrorState> LAST_ERROR = new ThreadLocal<ErrorState>() {
        @Override
        protected ErrorState initialValue() {
            return new ErrorState(ERROR_OTHER, "filesystem operation failed");
        }
    };
    private static final ThreadLocal<byte[]> LAST_PATH = new ThreadLocal<byte[]>() {
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

    private static final class OpenFile {
        private final FileChannel channel;
        private final Path path;
        private final boolean append;
        private final AtomicInteger references = new AtomicInteger(1);
        private FileLock lock;

        private OpenFile(FileChannel channel, Path path, boolean append) {
            this.channel = channel;
            this.path = path;
            this.append = append;
        }
    }

    private static final class MetadataSnapshot {
        private final long size;
        private final int type;
        private final boolean readonly;
        private final boolean posix;
        private final int permissionMode;
        private final Object fileKey;
        private final long modifiedSeconds;
        private final int modifiedNanos;
        private final long accessedSeconds;
        private final int accessedNanos;
        private final long createdSeconds;
        private final int createdNanos;

        private MetadataSnapshot(
                long size,
                int type,
                boolean readonly,
                boolean posix,
                int permissionMode,
                Object fileKey,
                Instant modified,
                Instant accessed,
                Instant created) {
            this.size = size;
            this.type = type;
            this.readonly = readonly;
            this.posix = posix;
            this.permissionMode = permissionMode;
            this.fileKey = fileKey;
            this.modifiedSeconds = modified.getEpochSecond();
            this.modifiedNanos = modified.getNano();
            this.accessedSeconds = accessed.getEpochSecond();
            this.accessedNanos = accessed.getNano();
            this.createdSeconds = created.getEpochSecond();
            this.createdNanos = created.getNano();
        }
    }

    private static final class DirectorySnapshot {
        private final byte[][] names;

        private DirectorySnapshot(byte[][] names) {
            this.names = names;
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
            this.pointers = new Pointer[count];
            this.storage = new byte[count][];
            this.buffers = new ByteBuffer[count];
        }
    }

    private FileSystemSupport() {}

    public static int lastErrorKind() {
        return LAST_ERROR.get().kind;
    }

    public static long lastErrorMessageLength() {
        return LAST_ERROR.get().message.length;
    }

    public static void copyLastErrorMessage(Pointer destination) {
        RuntimeSupport.copyBytes(LAST_ERROR.get().message, destination);
    }

    public static long open(
            Pointer pathBytes, long pathLength, int options, int creationMode) {
        try {
            Path path = path(pathBytes, pathLength);
            boolean read = (options & OPTION_READ) != 0;
            boolean write = (options & OPTION_WRITE) != 0;
            boolean append = (options & OPTION_APPEND) != 0;
            boolean truncate = (options & OPTION_TRUNCATE) != 0;
            boolean create = (options & OPTION_CREATE) != 0;
            boolean createNew = (options & OPTION_CREATE_NEW) != 0;

            Set<OpenOption> nioOptions = new HashSet<>();
            if (read) {
                nioOptions.add(StandardOpenOption.READ);
            }
            if (write || append) {
                nioOptions.add(StandardOpenOption.WRITE);
            }
            if (truncate && !createNew) {
                nioOptions.add(StandardOpenOption.TRUNCATE_EXISTING);
            }
            if (createNew) {
                nioOptions.add(StandardOpenOption.CREATE_NEW);
            } else if (create) {
                nioOptions.add(StandardOpenOption.CREATE);
            }

            FileChannel channel;
            if (creationMode >= 0 && (create || createNew)) {
                if ((creationMode & ~0777) != 0) {
                    throw new IllegalArgumentException(
                            "JVM POSIX creation mode contains unsupported bits");
                }
                if (path.getFileSystem().supportedFileAttributeViews().contains("posix")) {
                    FileAttribute<Set<PosixFilePermission>> permissions =
                            PosixFilePermissions.asFileAttribute(
                                    posixPermissions(creationMode));
                    channel = FileChannel.open(path, nioOptions, permissions);
                } else if (create && !createNew) {
                    Set<OpenOption> existingOptions = new HashSet<>(nioOptions);
                    existingOptions.remove(StandardOpenOption.CREATE);
                    try {
                        channel = FileChannel.open(path, existingOptions);
                    } catch (NoSuchFileException error) {
                        throw unsupportedCreationMode(path);
                    }
                } else {
                    throw unsupportedCreationMode(path);
                }
            } else {
                channel = FileChannel.open(path, nioOptions);
            }
            long handle = nextHandle();
            FILES.put(handle, new OpenFile(channel, path, append));
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long duplicate(long handle) {
        try {
            OpenFile file = file(handle);
            file.references.incrementAndGet();
            return handle;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static void close(long handle) {
        OpenFile file = FILES.get(handle);
        if (file == null) {
            return;
        }
        if (file.references.decrementAndGet() != 0) {
            return;
        }
        FILES.remove(handle, file);
        synchronized (file) {
            try {
                if (file.lock != null && file.lock.isValid()) {
                    file.lock.release();
                }
            } catch (IOException ignored) {
                // Drop cannot report close failures.
            }
            try {
                file.channel.close();
            } catch (IOException ignored) {
                // Drop cannot report close failures.
            }
        }
    }

    public static long read(long handle, Pointer destination, long length) {
        try {
            int checkedLength = checkedLength(length);
            if (checkedLength == 0) {
                return 0;
            }
            byte[] bytes = new byte[checkedLength];
            int read = file(handle).channel.read(ByteBuffer.wrap(bytes));
            if (read < 0) {
                return 0;
            }
            RuntimeSupport.copyBytes(bytes, read, destination);
            return read;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long readAt(long handle, Pointer destination, long length, long offset) {
        try {
            int checkedLength = checkedLength(length);
            checkedOffset(offset);
            if (checkedLength == 0) {
                return 0;
            }
            byte[] bytes = new byte[checkedLength];
            int read = file(handle).channel.read(ByteBuffer.wrap(bytes), offset);
            if (read < 0) {
                return 0;
            }
            RuntimeSupport.copyBytes(bytes, read, destination);
            return read;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long write(long handle, Pointer source, long length) {
        try {
            OpenFile file = file(handle);
            byte[] bytes = RuntimeSupport.copyFromPointer(source, length);
            synchronized (file) {
                if (file.append) {
                    file.channel.position(file.channel.size());
                }
                return file.channel.write(ByteBuffer.wrap(bytes));
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long writeAt(long handle, Pointer source, long length, long offset) {
        try {
            checkedOffset(offset);
            byte[] bytes = RuntimeSupport.copyFromPointer(source, length);
            return file(handle).channel.write(ByteBuffer.wrap(bytes), offset);
        } catch (Exception error) {
            return fail(error);
        }
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
            byte[] bytes = vector.read
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

    public static long readVectored(long fileHandle, long vectorHandle) {
        try {
            IoVector vector = completeIoVector(vectorHandle, true);
            long read = file(fileHandle).channel.read(vector.buffers);
            if (read < 0) {
                return 0;
            }
            for (int index = 0; index < vector.buffers.length; index++) {
                RuntimeSupport.copyBytes(
                        vector.storage[index],
                        vector.buffers[index].position(),
                        vector.pointers[index]);
            }
            return read;
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long writeVectored(long fileHandle, long vectorHandle) {
        try {
            IoVector vector = completeIoVector(vectorHandle, false);
            OpenFile file = file(fileHandle);
            synchronized (file) {
                if (file.append) {
                    file.channel.position(file.channel.size());
                }
                return file.channel.write(vector.buffers);
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static void closeIoVector(long handle) {
        IO_VECTORS.remove(handle);
    }

    public static long seek(long handle, int origin, long offset) {
        try {
            OpenFile file = file(handle);
            synchronized (file) {
                long base;
                if (origin == 0) {
                    base = 0;
                } else if (origin == 1) {
                    base = file.channel.position();
                } else if (origin == 2) {
                    base = file.channel.size();
                } else {
                    throw new IllegalArgumentException("invalid seek origin " + origin);
                }
                long position = Math.addExact(base, offset);
                if (position < 0) {
                    throw new IllegalArgumentException("invalid seek to a negative position");
                }
                file.channel.position(position);
                return position;
            }
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long tell(long handle) {
        try {
            return file(handle).channel.position();
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long size(long handle) {
        try {
            return file(handle).channel.size();
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int truncate(long handle, long size) {
        try {
            if (size < 0) {
                throw new IllegalArgumentException("file size exceeds the JVM signed range");
            }
            OpenFile file = file(handle);
            synchronized (file) {
                long oldSize = file.channel.size();
                if (size < oldSize) {
                    file.channel.truncate(size);
                } else if (size > oldSize) {
                    file.channel.write(ByteBuffer.wrap(new byte[] {0}), size - 1);
                }
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int sync(long handle, boolean metadata) {
        try {
            file(handle).channel.force(metadata);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    /** Returns 0 when acquired, 1 when a nonblocking lock would block, and -1 on error. */
    public static int lock(long handle, boolean shared, boolean blocking) {
        try {
            OpenFile file = file(handle);
            synchronized (file) {
                if (file.lock != null && file.lock.isValid()) {
                    return blocking ? failInt(new OverlappingFileLockException()) : 1;
                }
                try {
                    file.lock = blocking
                            ? file.channel.lock(0, Long.MAX_VALUE, shared)
                            : file.channel.tryLock(0, Long.MAX_VALUE, shared);
                } catch (OverlappingFileLockException error) {
                    if (!blocking) {
                        return 1;
                    }
                    throw error;
                }
                return file.lock == null ? 1 : 0;
            }
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int unlock(long handle) {
        try {
            OpenFile file = file(handle);
            synchronized (file) {
                if (file.lock != null) {
                    file.lock.release();
                    file.lock = null;
                }
            }
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long metadata(Pointer pathBytes, long pathLength, boolean followLinks) {
        try {
            return storeMetadata(snapshot(path(pathBytes, pathLength), followLinks));
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long fileMetadata(long fileHandle) {
        try {
            return storeMetadata(snapshot(file(fileHandle).path, true));
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long metadataSize(long handle) {
        return metadata(handle).size;
    }

    public static int metadataType(long handle) {
        return metadata(handle).type;
    }

    public static boolean metadataReadonly(long handle) {
        return metadata(handle).readonly;
    }

    public static boolean metadataHasPosixPermissions(long handle) {
        return metadata(handle).posix;
    }

    public static int metadataPermissionMode(long handle) {
        return metadata(handle).permissionMode;
    }

    public static long metadataFileKey(long handle) {
        Object key = metadata(handle).fileKey;
        return key == null ? 0 : storeFileKey(key);
    }

    public static long duplicateFileKey(long handle) {
        return storeFileKey(fileKey(handle));
    }

    public static boolean fileKeyEquals(long first, long second) {
        return fileKey(first).equals(fileKey(second));
    }

    public static int fileKeyHash(long handle) {
        return fileKey(handle).hashCode();
    }

    public static void closeFileKey(long handle) {
        FILE_KEYS.remove(handle);
    }

    public static long metadataModifiedSeconds(long handle) {
        return metadata(handle).modifiedSeconds;
    }

    public static int metadataModifiedNanos(long handle) {
        return metadata(handle).modifiedNanos;
    }

    public static long metadataAccessedSeconds(long handle) {
        return metadata(handle).accessedSeconds;
    }

    public static int metadataAccessedNanos(long handle) {
        return metadata(handle).accessedNanos;
    }

    public static long metadataCreatedSeconds(long handle) {
        return metadata(handle).createdSeconds;
    }

    public static int metadataCreatedNanos(long handle) {
        return metadata(handle).createdNanos;
    }

    public static void closeMetadata(long handle) {
        METADATA.remove(handle);
    }

    public static int setPermissions(
            Pointer pathBytes,
            long pathLength,
            boolean readonly,
            boolean hasPosixMode,
            int permissionMode) {
        try {
            setPermissions(path(pathBytes, pathLength), readonly, hasPosixMode, permissionMode);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int setFilePermissions(
            long handle, boolean readonly, boolean hasPosixMode, int permissionMode) {
        try {
            setPermissions(file(handle).path, readonly, hasPosixMode, permissionMode);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int setTimes(
            Pointer pathBytes,
            long pathLength,
            boolean hasAccessed,
            long accessedSeconds,
            int accessedNanos,
            boolean hasModified,
            long modifiedSeconds,
            int modifiedNanos,
            boolean followLinks) {
        try {
            setTimes(
                    path(pathBytes, pathLength),
                    hasAccessed,
                    accessedSeconds,
                    accessedNanos,
                    hasModified,
                    modifiedSeconds,
                    modifiedNanos,
                    followLinks);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int setFileTimes(
            long handle,
            boolean hasAccessed,
            long accessedSeconds,
            int accessedNanos,
            boolean hasModified,
            long modifiedSeconds,
            int modifiedNanos) {
        try {
            setTimes(
                    file(handle).path,
                    hasAccessed,
                    accessedSeconds,
                    accessedNanos,
                    hasModified,
                    modifiedSeconds,
                    modifiedNanos,
                    true);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long openDirectory(Pointer pathBytes, long pathLength) {
        try {
            Path path = path(pathBytes, pathLength);
            List<byte[]> names = new ArrayList<>();
            try (DirectoryStream<Path> entries = Files.newDirectoryStream(path)) {
                for (Path entry : entries) {
                    names.add(pathBytes(entry.getFileName()));
                }
            }
            long handle = nextHandle();
            DIRECTORIES.put(
                    handle, new DirectorySnapshot(names.toArray(new byte[names.size()][])));
            return handle;
        } catch (DirectoryIteratorException error) {
            return fail(error.getCause() == null ? error : error.getCause());
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long directoryCount(long handle) {
        return directory(handle).names.length;
    }

    public static long directoryNameLength(long handle, long index) {
        return directoryEntry(handle, index).length;
    }

    public static void copyDirectoryName(long handle, long index, Pointer destination) {
        RuntimeSupport.copyBytes(directoryEntry(handle, index), destination);
    }

    public static void closeDirectory(long handle) {
        DIRECTORIES.remove(handle);
    }

    public static int createDirectory(Pointer pathBytes, long pathLength) {
        try {
            Files.createDirectory(path(pathBytes, pathLength));
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int removeFile(Pointer pathBytes, long pathLength) {
        try {
            Path path = path(pathBytes, pathLength);
            if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
                return failInt(ERROR_IS_A_DIRECTORY, path + ": is a directory");
            }
            Files.delete(path);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int removeDirectory(Pointer pathBytes, long pathLength) {
        try {
            Path path = path(pathBytes, pathLength);
            if (!Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
                if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) {
                    throw new NoSuchFileException(path.toString());
                }
                throw new NotDirectoryException(path.toString());
            }
            Files.delete(path);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int rename(
            Pointer oldBytes, long oldLength, Pointer newBytes, long newLength) {
        try {
            Files.move(
                    path(oldBytes, oldLength),
                    path(newBytes, newLength),
                    StandardCopyOption.REPLACE_EXISTING);
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long copy(
            Pointer fromBytes, long fromLength, Pointer toBytes, long toLength) {
        try {
            Path from = path(fromBytes, fromLength);
            Path to = path(toBytes, toLength);
            BasicFileAttributes attributes =
                    Files.readAttributes(from, BasicFileAttributes.class);
            if (!attributes.isRegularFile()) {
                if (attributes.isDirectory()) {
                    return fail(ERROR_IS_A_DIRECTORY, from + ": is a directory");
                }
                return fail(new FileSystemException(
                        from.toString(), null, "source is not a regular file"));
            }
            Files.copy(
                    from,
                    to,
                    StandardCopyOption.REPLACE_EXISTING,
                    StandardCopyOption.COPY_ATTRIBUTES);
            return Files.size(to);
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static int hardLink(
            Pointer sourceBytes, long sourceLength, Pointer linkBytes, long linkLength) {
        try {
            Files.createLink(path(linkBytes, linkLength), path(sourceBytes, sourceLength));
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static int symbolicLink(
            Pointer sourceBytes, long sourceLength, Pointer linkBytes, long linkLength) {
        try {
            Files.createSymbolicLink(path(linkBytes, linkLength), path(sourceBytes, sourceLength));
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    public static long readLink(Pointer pathBytes, long pathLength) {
        try {
            return storePath(Files.readSymbolicLink(path(pathBytes, pathLength)));
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static long canonicalize(Pointer pathBytes, long pathLength) {
        try {
            return storePath(path(pathBytes, pathLength).toRealPath());
        } catch (Exception error) {
            return fail(error);
        }
    }

    public static void copyPathResult(Pointer destination) {
        RuntimeSupport.copyBytes(LAST_PATH.get(), destination);
    }

    public static int exists(Pointer pathBytes, long pathLength) {
        try {
            Files.readAttributes(path(pathBytes, pathLength), BasicFileAttributes.class);
            return 1;
        } catch (NoSuchFileException error) {
            return 0;
        } catch (Exception error) {
            return failInt(error);
        }
    }

    private static OpenFile file(long handle) {
        OpenFile file = FILES.get(handle);
        if (file == null) {
            throw new IllegalStateException("unknown JVM file handle " + handle);
        }
        return file;
    }

    private static MetadataSnapshot metadata(long handle) {
        MetadataSnapshot snapshot = METADATA.get(handle);
        if (snapshot == null) {
            throw new IllegalStateException("unknown JVM metadata handle " + handle);
        }
        return snapshot;
    }

    private static DirectorySnapshot directory(long handle) {
        DirectorySnapshot snapshot = DIRECTORIES.get(handle);
        if (snapshot == null) {
            throw new IllegalStateException("unknown JVM directory handle " + handle);
        }
        return snapshot;
    }

    private static IoVector ioVector(long handle) {
        IoVector vector = IO_VECTORS.get(handle);
        if (vector == null) {
            throw new IllegalStateException("unknown JVM vectored I/O handle " + handle);
        }
        return vector;
    }

    private static IoVector completeIoVector(long handle, boolean read) {
        IoVector vector = ioVector(handle);
        if (vector.read != read) {
            throw new IllegalArgumentException("vectored I/O direction mismatch");
        }
        if (vector.added != vector.buffers.length) {
            throw new IllegalArgumentException("incomplete vectored I/O buffer list");
        }
        return vector;
    }

    private static byte[] directoryEntry(long handle, long index) {
        byte[][] names = directory(handle).names;
        int checkedIndex = Math.toIntExact(index);
        if (checkedIndex < 0 || checkedIndex >= names.length) {
            throw new IndexOutOfBoundsException("directory entry index " + index);
        }
        return names[checkedIndex];
    }

    private static MetadataSnapshot snapshot(Path path, boolean followLinks) throws IOException {
        LinkOption[] options = followLinks
                ? new LinkOption[0]
                : new LinkOption[] {LinkOption.NOFOLLOW_LINKS};
        BasicFileAttributes attributes =
                Files.readAttributes(path, BasicFileAttributes.class, options);
        int type = fileType(attributes);

        PosixFileAttributeView posixView =
                Files.getFileAttributeView(path, PosixFileAttributeView.class, options);
        boolean hasPosix = posixView != null;
        int permissionMode = 0;
        boolean readonly;
        if (hasPosix) {
            Set<PosixFilePermission> permissions = posixView.readAttributes().permissions();
            permissionMode = permissionMode(permissions);
            readonly = (permissionMode & 0222) == 0;
        } else {
            DosFileAttributeView dosView =
                    Files.getFileAttributeView(path, DosFileAttributeView.class, options);
            if (dosView != null) {
                readonly = dosView.readAttributes().isReadOnly();
            } else {
                readonly = !Files.isWritable(path);
            }
        }

        return new MetadataSnapshot(
                attributes.size(),
                type,
                readonly,
                hasPosix,
                permissionMode,
                attributes.fileKey(),
                attributes.lastModifiedTime().toInstant(),
                attributes.lastAccessTime().toInstant(),
                attributes.creationTime().toInstant());
    }

    private static int fileType(BasicFileAttributes attributes) {
        return attributes.isSymbolicLink()
                ? TYPE_SYMLINK
                : attributes.isRegularFile()
                        ? TYPE_FILE
                        : attributes.isDirectory() ? TYPE_DIRECTORY : TYPE_OTHER;
    }

    private static void setPermissions(
            Path path, boolean readonly, boolean hasPosixMode, int permissionMode)
            throws IOException {
        PosixFileAttributeView posixView =
                Files.getFileAttributeView(path, PosixFileAttributeView.class);
        if (hasPosixMode && posixView != null) {
            posixView.setPermissions(posixPermissions(permissionMode));
            return;
        }

        DosFileAttributeView dosView = Files.getFileAttributeView(path, DosFileAttributeView.class);
        if (dosView != null) {
            dosView.setReadOnly(readonly);
            return;
        }

        File file = path.toFile();
        if (!file.setWritable(!readonly, false)) {
            throw new AccessDeniedException(path.toString());
        }
    }

    private static void setTimes(
            Path path,
            boolean hasAccessed,
            long accessedSeconds,
            int accessedNanos,
            boolean hasModified,
            long modifiedSeconds,
            int modifiedNanos,
            boolean followLinks)
            throws IOException {
        LinkOption[] options = followLinks
                ? new LinkOption[0]
                : new LinkOption[] {LinkOption.NOFOLLOW_LINKS};
        BasicFileAttributeView view =
                Files.getFileAttributeView(path, BasicFileAttributeView.class, options);
        if (view == null) {
            throw new UnsupportedOperationException("basic file timestamps are unavailable");
        }
        view.setTimes(
                hasModified
                        ? FileTime.from(Instant.ofEpochSecond(modifiedSeconds, modifiedNanos))
                        : null,
                hasAccessed
                        ? FileTime.from(Instant.ofEpochSecond(accessedSeconds, accessedNanos))
                        : null,
                null);
    }

    private static int permissionMode(Set<PosixFilePermission> permissions) {
        int mode = 0;
        if (permissions.contains(PosixFilePermission.OWNER_READ)) mode |= 0400;
        if (permissions.contains(PosixFilePermission.OWNER_WRITE)) mode |= 0200;
        if (permissions.contains(PosixFilePermission.OWNER_EXECUTE)) mode |= 0100;
        if (permissions.contains(PosixFilePermission.GROUP_READ)) mode |= 0040;
        if (permissions.contains(PosixFilePermission.GROUP_WRITE)) mode |= 0020;
        if (permissions.contains(PosixFilePermission.GROUP_EXECUTE)) mode |= 0010;
        if (permissions.contains(PosixFilePermission.OTHERS_READ)) mode |= 0004;
        if (permissions.contains(PosixFilePermission.OTHERS_WRITE)) mode |= 0002;
        if (permissions.contains(PosixFilePermission.OTHERS_EXECUTE)) mode |= 0001;
        return mode;
    }

    private static Set<PosixFilePermission> posixPermissions(int mode) {
        Set<PosixFilePermission> permissions = EnumSet.noneOf(PosixFilePermission.class);
        if ((mode & 0400) != 0) permissions.add(PosixFilePermission.OWNER_READ);
        if ((mode & 0200) != 0) permissions.add(PosixFilePermission.OWNER_WRITE);
        if ((mode & 0100) != 0) permissions.add(PosixFilePermission.OWNER_EXECUTE);
        if ((mode & 0040) != 0) permissions.add(PosixFilePermission.GROUP_READ);
        if ((mode & 0020) != 0) permissions.add(PosixFilePermission.GROUP_WRITE);
        if ((mode & 0010) != 0) permissions.add(PosixFilePermission.GROUP_EXECUTE);
        if ((mode & 0004) != 0) permissions.add(PosixFilePermission.OTHERS_READ);
        if ((mode & 0002) != 0) permissions.add(PosixFilePermission.OTHERS_WRITE);
        if ((mode & 0001) != 0) permissions.add(PosixFilePermission.OTHERS_EXECUTE);
        return permissions;
    }

    private static UnsupportedOperationException unsupportedCreationMode(Path path) {
        return new UnsupportedOperationException(
                path + ": filesystem does not support atomic POSIX creation permissions");
    }

    private static Path path(Pointer bytes, long length)
            throws CharacterCodingException, InvalidPathException, NoSuchFileException {
        ByteBuffer encoded = ByteBuffer.wrap(RuntimeSupport.copyFromPointer(bytes, length));
        String decoded = StandardCharsets.UTF_8
                .newDecoder()
                .onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT)
                .decode(encoded)
                .toString();
        if (decoded.isEmpty()) {
            throw new NoSuchFileException(decoded);
        }
        if (File.separatorChar == '\\'
                && decoded.length() >= 4
                && decoded.charAt(0) == '/'
                && Character.isLetter(decoded.charAt(1))
                && decoded.charAt(2) == ':'
                && decoded.charAt(3) == '/') {
            decoded = decoded.substring(1);
        }
        return Paths.get(decoded);
    }

    private static byte[] pathBytes(Path path) {
        String portable = path.toString().replace(File.separatorChar, '/');
        if (File.separatorChar == '\\'
                && portable.length() >= 3
                && Character.isLetter(portable.charAt(0))
                && portable.charAt(1) == ':'
                && portable.charAt(2) == '/') {
            portable = "/" + portable;
        }
        return portable.getBytes(StandardCharsets.UTF_8);
    }

    private static long storePath(Path path) {
        byte[] bytes = pathBytes(path);
        LAST_PATH.set(bytes);
        return bytes.length;
    }

    private static long storeMetadata(MetadataSnapshot snapshot) {
        long handle = nextHandle();
        METADATA.put(handle, snapshot);
        return handle;
    }

    private static long storeFileKey(Object key) {
        long handle = nextHandle();
        FILE_KEYS.put(handle, key);
        return handle;
    }

    private static Object fileKey(long handle) {
        Object key = FILE_KEYS.get(handle);
        if (key == null) {
            throw new IllegalStateException("unknown JVM file-key handle " + handle);
        }
        return key;
    }

    private static long nextHandle() {
        long handle = NEXT_HANDLE.getAndIncrement();
        if (handle <= 0) {
            throw new IllegalStateException("JVM filesystem handle space exhausted");
        }
        return handle;
    }

    private static int checkedLength(long length) {
        if (length < 0) {
            throw new IllegalArgumentException("negative buffer length");
        }
        return Math.toIntExact(length);
    }

    private static void checkedOffset(long offset) {
        if (offset < 0) {
            throw new IllegalArgumentException("file offset exceeds the JVM signed range");
        }
    }

    private static long fail(Throwable error) {
        setError(error);
        return -1;
    }

    private static int failInt(Throwable error) {
        setError(error);
        return -1;
    }

    private static long fail(int kind, String message) {
        LAST_ERROR.set(new ErrorState(kind, message));
        return -1;
    }

    private static int failInt(int kind, String message) {
        LAST_ERROR.set(new ErrorState(kind, message));
        return -1;
    }

    private static void setError(Throwable error) {
        int kind = errorKind(error);
        String message = error.getMessage();
        if (message == null || message.isEmpty()) {
            message = error.getClass().getSimpleName();
        }
        LAST_ERROR.set(new ErrorState(kind, message));
    }

    private static int errorKind(Throwable error) {
        if (error instanceof NoSuchFileException) return ERROR_NOT_FOUND;
        if (error instanceof AccessDeniedException || error instanceof SecurityException) {
            return ERROR_PERMISSION_DENIED;
        }
        if (error instanceof FileAlreadyExistsException) return ERROR_ALREADY_EXISTS;
        if (error instanceof NotDirectoryException) return ERROR_NOT_A_DIRECTORY;
        if (error instanceof DirectoryNotEmptyException) return ERROR_DIRECTORY_NOT_EMPTY;
        if (error instanceof ReadOnlyFileSystemException) return ERROR_READ_ONLY_FILESYSTEM;
        if (error instanceof FileSystemLoopException) return ERROR_FILESYSTEM_LOOP;
        if (error instanceof ClosedByInterruptException) return ERROR_INTERRUPTED;
        if (error instanceof OverlappingFileLockException) return ERROR_WOULD_BLOCK;
        if (error instanceof UnsupportedOperationException) return ERROR_UNSUPPORTED;
        if (error instanceof CharacterCodingException) return ERROR_INVALID_DATA;
        if (error instanceof InvalidPathException
                || error instanceof IllegalArgumentException
                || error instanceof NonReadableChannelException
                || error instanceof NonWritableChannelException) {
            return ERROR_INVALID_INPUT;
        }
        if (error instanceof FileSystemException) {
            String reason = ((FileSystemException) error).getReason();
            if (reason != null) {
                String normalized = reason.toLowerCase();
                if (normalized.contains("is a directory")) return ERROR_IS_A_DIRECTORY;
                if (normalized.contains("cross-device")
                        || normalized.contains("different file system")) {
                    return ERROR_CROSSES_DEVICES;
                }
            }
        }
        return ERROR_OTHER;
    }
}
