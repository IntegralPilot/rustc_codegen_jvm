use crate::test_support::run_jar;
use crate::*;
use jvm_compiler_core::classfile::summary;

#[test]
fn enum_sharing_retains_live_helper_unions_and_distinct_payloads_and_tags() {
    let temp = tempfile::tempdir().unwrap();
    for live_equality in [false, true] {
        let source = r#"
public class EnumRun {
    public interface A {
        long $rust$tag();
        static long _unionDiscriminant(A a) { return a == null ? 0 : a.$rust$tag(); }
        static boolean eq(A a, A b) { return ((AV)a).value == ((AV)b).value; }
    }
    public interface B {
        long $rust$tag();
        static long _unionDiscriminant(B a) { return a == null ? 0 : a.$rust$tag(); }
        static boolean eq(B a, B b) { return ((BV)a).value == ((BV)b).value; }
    }
    public interface C {
        long $rust$tag();
        static long _unionDiscriminant(C a) { return a == null ? 0 : a.$rust$tag(); }
        static boolean eq(C a, C b) { return ((CV)a).value == ((CV)b).value; }
    }
    public static class AV implements A {
        public int value;
        public AV(int v) { value = v; }
        public long $rust$tag() { return 7; }
    }
    public static class BV implements B {
        public long value;
        public BV(long v) { value = v; }
        public long $rust$tag() { return -11; }
    }
    public static class CV implements C {
        public int value;
        public CV(int v) { value = v; }
        public long $rust$tag() { return 7; }
    }
    public static void main(String[] args) {
        AV a = new AV(19); BV b = new BV(23); CV c = new CV(29);
        if (a.value != 19 || b.value != 23 || c.value != 29) throw new AssertionError("payload");
        if (A._unionDiscriminant(a) != 7 || B._unionDiscriminant(b) != -11
            || C._unionDiscriminant(c) != 7 || A._unionDiscriminant(null) != 0)
            throw new AssertionError("tag dispatch");
        /* equality */
    }
}
"#.replace("/* equality */", if live_equality {
            "if (!C.eq(c, new CV(29)) || C.eq(c, new CV(31))) throw new AssertionError(\"equality\");"
        } else { "" });
        fs::write(temp.path().join("EnumRun.java"), source).unwrap();
        let compile = std::process::Command::new("javac")
            .args(["--release", "8", "EnumRun.java"])
            .current_dir(temp.path())
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "{}",
            String::from_utf8_lossy(&compile.stderr)
        );
        let mut paths = Vec::new();
        for suffix in ["", "$A", "$B", "$C", "$AV", "$BV", "$CV"] {
            let name = format!("EnumRun{suffix}");
            let path = temp.path().join(format!("{name}.class"));
            let mut class = class_file_from_data(&fs::read(&path).unwrap()).unwrap();
            if !suffix.is_empty() {
                let (owner, variant, scalar, tag) = match suffix {
                    "$A" | "$AV" => ("EnumRun$A", "EnumRun$AV", "I", 7),
                    "$B" | "$BV" => ("EnumRun$B", "EnumRun$BV", "J", -11),
                    _ => ("EnumRun$C", "EnumRun$CV", "I", 7),
                };
                let recipe = if name == owner {
                    format!(
                        "carrier-v2;enum-v1;method=$rust$tag;abstract-tag;method=_unionDiscriminant;dispatch-tag;0:L{owner};;method=eq;eq;0:L{owner};;0:L{variant};;"
                    )
                } else {
                    format!(
                        "carrier-v2;variant-v1;V0;{tag};0:L{owner};;carrier-v2;plain;scalar;5:value{scalar};5:value{scalar};"
                    )
                };
                for (attribute, info) in [
                    (summary::PRIVATE_ATTRIBUTE, Vec::new()),
                    (summary::CARRIER_ATTRIBUTE, recipe.into_bytes()),
                ] {
                    class.attributes.push(Attribute::Unknown {
                        name_index: class.constant_pool.add_utf8(attribute).unwrap(),
                        info,
                    });
                }
                fs::write(&path, serialize_class_file(&class).unwrap()).unwrap();
            }
            paths.push(path.to_string_lossy().into_owned());
        }
        let output = temp.path().join("linked.jar");
        pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
        run_jar(&output);
        let mut jar = ZipArchive::new(fs::File::open(output).unwrap()).unwrap();
        let mut interfaces = 0;
        for i in 0..jar.len() {
            let mut entry = jar.by_index(i).unwrap();
            if !entry.name().ends_with(".class") {
                continue;
            }
            let mut data = Vec::new();
            entry.read_to_end(&mut data).unwrap();
            let class = class_file_from_data(&data).unwrap();
            interfaces += usize::from(class.access_flags.contains(ClassAccessFlags::INTERFACE));
        }
        assert_eq!(interfaces, if live_equality { 2 } else { 1 });
    }
}
