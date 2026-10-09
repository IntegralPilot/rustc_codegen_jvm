import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import org.rustlang.runtime.Pointer;
import org.rustlang.runtime.RustField;
import org.rustlang.runtime.SliceView;

/** Physical borrowed fields and references to them survive parent replacement. */
public final class BorrowedFields {
    private static final String VIEW = "@slice-pointer\norg/rustlang/runtime/SliceView\n1\n";
    private static final String ADDRESS = "@raw-pointer\n4\n\n";
    private static Class<?> fixture;

    public static final class Codec {
        public static void w$slots(Object value, byte[] bytes, int start) throws Exception {
            Pointer.encodeFatPointerMemory(RustField.find(fixture, "view").get(value), bytes, start, 16, VIEW);
            Pointer.array(bytes, 0, 1).byte_offset(start + 16).retype(8, ADDRESS)
                    .set(RustField.find(fixture, "address").get(value));
        }
        public static Object a$slots(byte[] bytes, int start) throws Exception {
            Object value = fixture.getConstructor().newInstance();
            RustField.find(fixture, "view").set(value, Pointer.decodeFatPointerMemory(bytes, start, 16, VIEW));
            RustField.find(fixture, "address").set(value,
                    Pointer.array(bytes, 0, 1).byte_offset(start + 16).retype(8, ADDRESS).getObject());
            return value;
        }
    }
    public static class Slots {
        public Object view;
        public int $rust$s$view;
        public long $rust$l$view;
        public Object address;
        public long $rust$4$address;
        public Object typed;
        public long $rust$a8$typed;
        public static final String $rust$c$typed = "MemoryViews$PairCodec#pair#LMemoryViews$Pair;#8";
    }

    // javac cannot declare synthetic fields. Set the flags that the backend emits.
    public static Class<?> slotsClass() throws Exception {
        byte[] data;
        try (InputStream input = BorrowedFields.class.getResourceAsStream("BorrowedFields$Slots.class")) {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            byte[] buffer = new byte[4096];
            for (int count; (count = input.read(buffer)) >= 0;) output.write(buffer, 0, count);
            data = output.toByteArray();
        }
        ByteBuffer bytes = ByteBuffer.wrap(data);
        bytes.position(8);
        int count = Short.toUnsignedInt(bytes.getShort());
        String[] names = new String[count];
        for (int i = 1; i < count; i++) {
            switch (Byte.toUnsignedInt(bytes.get())) {
                case 1:
                    byte[] text = new byte[Short.toUnsignedInt(bytes.getShort())];
                    bytes.get(text);
                    names[i] = new String(text, "UTF-8");
                    break;
                case 3: case 4: case 9: case 10: case 11: case 12: case 17: case 18:
                    bytes.position(bytes.position() + 4); break;
                case 5: case 6: bytes.position(bytes.position() + 8); i++; break;
                case 7: case 8: case 16: case 19: case 20:
                    bytes.position(bytes.position() + 2); break;
                case 15: bytes.position(bytes.position() + 3); break;
                default: throw new AssertionError("unknown fixture constant");
            }
        }
        bytes.position(bytes.position() + 6);
        int interfaces = Short.toUnsignedInt(bytes.getShort());
        bytes.position(bytes.position() + interfaces * 2);
        int fields = Short.toUnsignedInt(bytes.getShort());
        for (int i = 0; i < fields; i++) {
            int position = bytes.position();
            short flags = bytes.getShort();
            String name = names[Short.toUnsignedInt(bytes.getShort())];
            if (name.startsWith("$rust$")) bytes.putShort(position, (short) (flags | 0x1000));
            bytes.getShort();
            int attributes = Short.toUnsignedInt(bytes.getShort());
            for (int j = 0; j < attributes; j++) {
                bytes.getShort();
                int length = bytes.getInt();
                bytes.position(bytes.position() + length);
            }
        }
        return new ClassLoader(BorrowedFields.class.getClassLoader()) {
            Class<?> define(byte[] bytes) { return defineClass(null, bytes, 0, bytes.length); }
        }.define(data);
    }

    public static void check() throws Exception {
        Class<?> type = slotsClass();
        fixture = type;
        Object first = type.getConstructor().newInstance();
        Object second = type.getConstructor().newInstance();
        Field view = type.getField("view");
        Field address = type.getField("address");
        byte[] bytes = {3, 5, 7, 11};
        int[] words = {13, 17, 19};
        view.set(first, bytes);
        type.getField("$rust$s$view").setInt(first, 1);
        type.getField("$rust$l$view").setLong(first, 3);
        address.set(first, words);
        type.getField("$rust$4$address").setLong(first, 4);
        byte[] typedBytes = new byte[24];
        org.rustlang.runtime.MemoryBytes.write(typedBytes, 8, 4, 101);
        org.rustlang.runtime.MemoryBytes.write(typedBytes, 12, 4, 103);
        org.rustlang.runtime.MemoryBytes.write(typedBytes, 16, 4, 107);
        type.getField("typed").set(first, typedBytes);
        type.getField("$rust$a8$typed").setLong(first, 8);
        Pointer typed = (Pointer) RustField.find(type, "typed").get(first);
        MemoryViews.Pair pair = (MemoryViews.Pair) typed.getObjectCopyAs(MemoryViews.Pair.class.getName());
        MemoryViews.Pair next = (MemoryViews.Pair) typed.add(1).getObjectCopyAs(MemoryViews.Pair.class.getName());
        if (pair.first != 101 || pair.second != 103 || next.first != 107)
            throw new AssertionError("stored exact layout lost its stride or codec");
        RustField.find(type, "typed").set(second, typed.add(1));
        if (((MemoryViews.Pair) ((Pointer) RustField.find(type, "typed").get(second))
                .getObjectCopyAs(MemoryViews.Pair.class.getName())).first != 107)
            throw new AssertionError("reflective exact-layout store lost its location");
        Pointer owner = Pointer.cell(first, 24, null);
        // Supply the exact Class from the child loader.
        Field allocation = Pointer.class.getDeclaredField("allocation");
        allocation.setAccessible(true);
        java.lang.reflect.Method project = Pointer.class.getDeclaredMethod("rootField",
                Object.class, Class.class, String.class, long.class, String.class);
        project.setAccessible(true);
        Pointer slice = (Pointer) project.invoke(null, allocation.get(owner), type, "view", 16L, null);
        Pointer pointer = (Pointer) project.invoke(null, allocation.get(owner), type, "address", 8L, null);
        long[] metadata = new long[2];
        if (Pointer.loadBorrowedView(slice, 0, metadata) != bytes || metadata[0] != 1 || metadata[1] != 3)
            throw new AssertionError("view components were boxed or changed");
        if (Pointer.loadBorrowedAddress(pointer, 0, metadata) != words || metadata[0] != 4)
            throw new AssertionError("address components were boxed or changed");
        owner.set(second);
        Pointer.storeBorrowedView(slice, 0, bytes, 2, 2);
        Pointer.storeBorrowedAddress(pointer, 0, words, 8, 4);
        if (view.get(second) != bytes || address.get(second) != words
                || type.getField("$rust$s$view").getInt(second) != 2
                || type.getField("$rust$4$address").getLong(second) != 8
                || type.getField("$rust$s$view").getInt(first) != 1)
            throw new AssertionError("borrowed store detached after parent replacement");
        if (Pointer.loadBorrowedAddress(pointer, 0, metadata) != words || metadata[0] != 8)
            throw new AssertionError("borrowed read retained replaced parent");
        Pointer.storeBorrowedAddress(pointer, 0, null, 0, 4);
        if (Pointer.loadBorrowedAddress(pointer, 0, metadata) != null || metadata[0] != 0)
            throw new AssertionError("nullable borrow changed");

        String codec = "BorrowedFields$Codec#slots#Ljava/lang/Object;#24";
        Object storage = Pointer.storageAligned(first, 24, codec, 8,
                "0,16,view,v:4\n" + VIEW + "\n16,8,address,p:4\n" + ADDRESS);
        Object projected = Pointer.storageBorrowedFieldRoot(storage, 0, type.getName(), "view", 0, 16, VIEW);
        if (projected != storage || Pointer.loadBorrowedView(projected, 0, metadata) != bytes)
            throw new AssertionError("borrowed field address materialized a carrier");
        projected = Pointer.storageBorrowedFieldRoot(storage, 0, type.getName(), "address", 16, 8, ADDRESS);
        if (projected != storage || Pointer.loadBorrowedAddress(projected, 16, metadata) != words)
            throw new AssertionError("stored pointer address materialized a carrier");
        Pointer.storeStorageLocation(storage, 0, second);
        Pointer.storeBorrowedView(storage, 0, bytes, 1, 3);
        Pointer.storeBorrowedAddress(storage, 16, words, 4, 4);
        if (Pointer.loadBorrowedView(storage, 0, metadata) != bytes || metadata[0] != 1 || metadata[1] != 3
                || Pointer.loadBorrowedAddress(storage, 16, metadata) != words || metadata[0] != 4)
            throw new AssertionError("typed borrowed fields detached after replacement");
        Field boundary = storage.getClass().getDeclaredField("boundary");
        boundary.setAccessible(true);
        if (boundary.get(storage) != null) throw new AssertionError("typed borrows allocated Pointer");

        Pointer escaped = Pointer.addressFromParts(storage, 0, 64);
        Pointer raw = Pointer.fromStorageLocation(storage, 0);
        // The metadata word is shared with byte aliases of the enclosing owner.
        raw.byte_offset(8).retype(8, null).set(2L);
        SliceView sliceValue = (SliceView) escaped.getObject();
        if (sliceValue.rustLength != 2) throw new AssertionError("byte alias did not update stored borrow");
        Pointer.storeBorrowedView(storage, 0, bytes, 2, 1);
        if (((SliceView) escaped.getObject()).rustLength != 1 || raw.byte_offset(8).retype(8, null).getI64() != 1)
            throw new AssertionError("stored borrow did not update byte alias");
        Pointer.storeStorageLocation(storage, 0, first);
        Pointer.storeBorrowedView(storage, 0, bytes, 0, 4);
        if (((SliceView) escaped.getObject()).rustLength != 4)
            throw new AssertionError("escaped borrow retained replaced owner");
    }
}
