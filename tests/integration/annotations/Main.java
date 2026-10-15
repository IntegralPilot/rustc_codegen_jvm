import annotations.Annotated;
import annotations.Repeated;
import annotations.annotations;
import java.lang.annotation.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class Main {
    public enum Mode { FIRST, SECOND }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE)
    @Repeatable(Tags.class)
    public @interface Tag { String value(); }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE)
    public @interface Tags { Tag[] value(); }

    @Retention(RetentionPolicy.CLASS)
    public @interface ClassTag {}

    @Retention(RetentionPolicy.SOURCE)
    public @interface SourceTag {}

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE)
    public @interface AllValues {
        boolean flag(); byte small(); short medium(); int count(); long big();
        float float_value(); double double_value(); char letter(); String text();
        Mode kind(); Class<?> type(); Class<?>[] types(); Tag nested();
        int[] numbers(); String[] empty(); String retention();
        int omitted() default 17;
    }

    public static void main(String[] args) throws Exception {
        AllValues a = Annotated.class.getAnnotation(AllValues.class);
        if (a == null || !a.flag() || a.small() != Byte.MIN_VALUE || a.medium() != Short.MIN_VALUE
                || a.count() != Integer.MIN_VALUE || a.big() != Long.MIN_VALUE
                || Float.floatToIntBits(a.float_value()) != Float.floatToIntBits(-0.0f)
                || a.double_value() != 1.25 || a.letter() != 'λ' || !a.text().equals("é\0😀")
                || a.kind() != Mode.SECOND || a.type() != String[].class
                || !Arrays.equals(a.types(), new Class<?>[] { int.class, void.class, Object.class })
                || !a.nested().value().equals("nested") || !Arrays.equals(a.numbers(), new int[] { 1, -2, 3 })
                || a.empty().length != 0 || !a.retention().equals("an annotation element") || a.omitted() != 17)
            throw new AssertionError("annotation element values");
        if (!Annotated.class.isAnnotationPresent(Deprecated.class)
                || Annotated.class.getDeclaredAnnotations().length != 2)
            throw new AssertionError("annotation retention");
        try (var input = Annotated.class.getResourceAsStream("Annotated.class")) {
            String data = new String(input.readAllBytes(), StandardCharsets.ISO_8859_1);
            if (!data.contains("RuntimeInvisibleAnnotations") || !data.contains("LMain$ClassTag;")
                    || data.contains("LMain$SourceTag;"))
                throw new AssertionError("class/source retention encoding");
        }
        Tag[] repeated = Repeated.class.getAnnotationsByType(Tag.class);
        if (repeated.length != 2 || !repeated[0].value().equals("first") || !repeated[1].value().equals("second"))
            throw new AssertionError("repeatable annotation container");
        for (Object value : new Object[] { annotations.generic(), annotations.generic_long() }) {
            Tag tag = value.getClass().getAnnotation(Tag.class);
            if (tag == null || !tag.value().equals("upstream") || value.getClass().getDeclaredAnnotations().length != 1)
                throw new AssertionError("cross-crate generic annotation");
        }
        Annotated value = new Annotated(4);
        ((Runnable) value).run();
        if (value.value != 5) throw new AssertionError("interface implementation with annotations");
    }
}
