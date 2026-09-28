use crate::*;
use jvm_compiler_core::classfile::{constant_pool::InternedConstantPool, names::*};
use namespaces::Namespaces;
use ristretto_classfile::{Method, MethodAccessFlags};

const OLD: &str = "versioned$crate1111111111111111$";
const NEW: &str = "versioned$crate2222222222222222$";
const APP: &str = "app$crate3333333333333333$";

#[test]
fn only_unambiguous_crate_names_are_shortened() {
    let old = format!("{OLD}/Node.class");
    let new = format!("{NEW}/Node.class");
    let app = format!("{APP}/app.class");
    let names = Namespaces::collect([old.as_str(), new.as_str(), app.as_str()].into_iter());
    assert_eq!(names.name(&old), old);
    assert_eq!(names.name(&new), new);
    assert_eq!(names.name(&app), "app/app.class");
    assert_eq!(
        names.name(&format!("core/Vec_{APP}_Item")),
        "core/Vec_app_Item"
    );
    // An existing unqualified Java class also occupies its namespace.
    let names = Namespaces::collect([app.as_str(), "app/Foreign.class"].into_iter());
    assert_eq!(names.name(&app), app);
}

#[test]
fn names_and_reflection_strings_relocate_without_changing_literals_or_code() {
    let owner = format!("{APP}/Node");
    let descriptor = format!("(L{owner};)[L{owner};");
    let mut pool = InternedConstantPool::default();
    let this_class = pool.add_class(&owner).unwrap();
    let super_class = pool.add_class("java/lang/Object").unwrap();
    let literal = pool.add_string(&owner).unwrap(); // shares UTF8 with CONSTANT_Class
    let reflection = pool.add_name_string(&owner).unwrap();
    let escaped_value = format!("{NAME_STRING}{owner}");
    let escaped = pool.add_string(&escaped_value).unwrap();
    let unicode_value = "λ\0😀";
    let unicode = pool.add_string(unicode_value).unwrap();
    let source = format!("{APP}.rs");
    let source_index = pool.add_utf8(&source).unwrap();
    let method = Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: pool.add_utf8("name").unwrap(),
        descriptor_index: pool.add_utf8("()Ljava/lang/String;").unwrap(),
        attributes: vec![Attribute::Code {
            name_index: pool.add_utf8("Code").unwrap(),
            max_stack: 1,
            max_locals: 0,
            code: vec![
                Instruction::Ldc(literal.try_into().unwrap()),
                Instruction::Areturn,
            ],
            exception_table: vec![],
            attributes: vec![],
        }],
    };
    let class = ClassFile {
        constant_pool: {
            pool.add_method_type(&descriptor).unwrap();
            pool.into_inner()
        },
        this_class,
        super_class,
        methods: vec![method.clone()],
        attributes: vec![Attribute::SourceFile {
            name_index: 0, // Filled below, independently of the name being relocated.
            source_file_index: source_index,
        }],
        ..Default::default()
    };
    let mut class = class;
    let source_attr = class.constant_pool.add_utf8("SourceFile").unwrap();
    let Attribute::SourceFile { name_index, .. } = &mut class.attributes[0] else {
        unreachable!()
    };
    *name_index = source_attr;
    let data = serialize_class_file(&class).unwrap();
    let names = Namespaces::collect([owner.as_str()].into_iter());
    let relocated = names
        .class(ClassInfo {
            jar_entry_name: format!("{owner}.class"),
            data,
        })
        .unwrap();
    assert_eq!(relocated.jar_entry_name, "app/Node.class");
    let class = class_file_from_data(&relocated.data).unwrap();
    assert_eq!(class.methods, [method]);
    assert_eq!(class.class_name().unwrap(), "app/Node");
    assert_eq!(
        class.constant_pool.try_get_string(literal).unwrap(),
        owner.as_str()
    );
    assert_eq!(
        class.constant_pool.try_get_string(reflection).unwrap(),
        "app/Node"
    );
    assert_eq!(
        class.constant_pool.try_get_string(escaped).unwrap(),
        escaped_value.as_str()
    );
    assert_eq!(
        class.constant_pool.try_get_string(unicode).unwrap(),
        unicode_value
    );
    assert_eq!(
        class.constant_pool.try_get_utf8(source_index).unwrap(),
        source.as_str()
    );
    assert!(
        class
            .constant_pool
            .iter()
            .any(|constant| matches!(constant, Constant::MethodType(index)
        if class.constant_pool.try_get_utf8(*index).unwrap() == "(Lapp/Node;)[Lapp/Node;"))
    );
}

#[test]
fn crate_markers_are_not_nesting_separators() {
    let name = format!("{APP}/Outer_{OLD}_Item$Inner");
    let separators = nesting_separators(&name).collect::<Vec<_>>();
    assert_eq!(separators, [name.len() - "$Inner".len()]);
    assert_eq!(inner_name(&name), "Inner");
}
