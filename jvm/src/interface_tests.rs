use super::*;

#[test]
fn interface_statics_inherit_the_owner_kind_without_affecting_other_owners() {
    let item: syn::ItemImpl = syn::parse_quote! {
        impl Measure {
            #[jvm::static_method]
            fn inferred() -> i32 {}
            #[jvm::static_method("renamed")]
            fn shorthand() -> i32 {}
            #[jvm::static_method(class = "example.Measure", name = "value")]
            fn own_interface() -> i32 {}
            #[jvm::static_method(class = "other.Class", name = "value")]
            fn other_class() -> i32 {}
            #[jvm::static_method("other.Class", "value")]
            fn positional_class() -> i32 {}
            #[jvm::static_method(class = "other.Interface", name = "value", interface = true)]
            fn other_interface() -> i32 {}
        }
    };
    let (expanded, _) = expand_class_impl(
        item,
        Some("example/Measure".to_string()),
        RenameRule::None,
        true,
    )
    .unwrap();
    let links: Vec<_> = expanded
        .items
        .iter()
        .map(|item| {
            let ImplItem::Fn(function) = item else {
                panic!("expected method")
            };
            let syn::Stmt::Item(syn::Item::ForeignMod(block)) = &function.block.stmts[0] else {
                panic!("expected generated extern block")
            };
            let syn::ForeignItem::Fn(function) = &block.items[0] else {
                panic!("expected import")
            };
            let Meta::NameValue(attribute) = &function.attrs[0].meta else {
                panic!("expected link name")
            };
            let syn::Expr::Lit(value) = &attribute.value else {
                panic!("expected literal")
            };
            let syn::Lit::Str(value) = &value.lit else {
                panic!("expected string")
            };
            value.value()
        })
        .collect();
    assert_eq!(
        links,
        [
            "jvm:static-interface:example/Measure:inferred",
            "jvm:static-interface:example/Measure:renamed",
            "jvm:static-interface:example/Measure:value",
            "jvm:static:other/Class:value",
            "jvm:static:other/Class:value",
            "jvm:static-interface:other/Interface:value",
        ]
    );
}

#[test]
fn interface_declarations_reject_constructors_and_instance_fields() {
    for item in [
        syn::parse_quote! { impl Invalid { #[jvm::constructor] fn new() -> *mut Self {} } },
        syn::parse_quote! { impl Invalid { #[jvm::field] fn value(&self) -> i32 {} } },
    ] {
        let error = expand_class_impl(item, Some("example/Invalid".into()), RenameRule::None, true)
            .err()
            .expect("interface instance members must be rejected");
        assert!(
            error
                .to_string()
                .contains("no constructors or instance fields")
        );
    }
}

#[test]
fn static_interface_descriptors_are_preserved_and_options_are_checked() {
    let args = syn::parse_str::<Args>(
        r#"class = "example.Factory", name = "create", descriptor = "()I", interface = true"#,
    )
    .unwrap();
    let function: ItemFn = syn::parse_quote! { fn create() -> i32 {} };
    assert_eq!(
        binding_link(
            BindingKind::StaticMethod,
            &args,
            &function.sig,
            None,
            RenameRule::None
        )
        .unwrap(),
        "jvm:static-interface:example/Factory:create:()I"
    );
    assert!(syn::parse_str::<Args>("interface = true, interface = false").is_err());
    assert!(syn::parse_str::<Args>(r#"interface = "true""#).is_err());
    assert!(
        binding_link(
            BindingKind::Method,
            &args,
            &function.sig,
            None,
            RenameRule::None
        )
        .is_err()
    );
}
