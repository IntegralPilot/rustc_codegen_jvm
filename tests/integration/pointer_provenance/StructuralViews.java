import org.rustlang.runtime.Pointer;
import org.rustlang.runtime.TraitObjectCarrier;

/** Trait metadata belongs to the innermost unsized tail, through every wrapper. */
public final class StructuralViews {
    public interface Value { int get(); }

    public static final class Carrier implements Value, TraitObjectCarrier {
        public int get() { return 42; }
        public Object rustTraitObjectPayload() { return Integer.valueOf(42); }
        public long rustTraitObjectSize() { return 4; }
        public long rustTraitObjectAlignment() { return 4; }
    }

    public static final class Inner {
        public int tag;
        public int value;
        public Inner(int tag, int value) { this.tag = tag; this.value = value; }
    }

    public static final class Outer {
        public int tag;
        public Inner tail;
        public Outer(int tag, Inner tail) { this.tag = tag; this.tail = tail; }
    }

    public static final class InnerDyn {
        public int tag;
        public Value value;
        public InnerDyn(int tag, Value value) { this.tag = tag; this.value = value; }
    }

    public static final class OuterDyn {
        public int tag;
        public InnerDyn tail;
        public OuterDyn(int tag, InnerDyn tail) { this.tag = tag; this.tail = tail; }
    }

    public static void main(String[] args) { check(); }

    public static void check() {
        Pointer source = Pointer.cell(new Outer(7, new Inner(11, 42)), 12, null);
        Pointer view = Pointer.unsizeStruct(source, 8, OuterDyn.class.getName());
        Carrier carrier = new Carrier();
        Pointer.attachStructTailTraitMetadata(view, Pointer.fromTraitObjectReference(carrier));
        OuterDyn value = (OuterDyn) view.getObjectAs(OuterDyn.class.getName());
        if (value.tag != 7 || value.tail.tag != 11
                || value.tail.value != carrier || value.tail.value.get() != 42) {
            throw new AssertionError("nested structural view lost its trait tail metadata");
        }
    }
}
