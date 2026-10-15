use super::*;

#[test]
fn rejects_invalid_values_and_targets() {
    for args in [
        quote!("", value = 1),
        quote!("bad;Name"),
        quote!("p.Tag", retention = "unknown"),
        quote!("p.Tag", retention = "class", retention = "runtime"),
        quote!("p.Tag", value = 1, values(value = 2)),
        quote!("p.Tag", value = 128_i8),
        quote!("p.Tag", value = -129_i8),
        quote!("p.Tag", value = 2147483648),
        quote!("p.Tag", value = 1_usize),
        quote!("p.Tag", value = '😀'),
        quote!("p.Tag", value = [1, "two"]),
        quote!("p.Tag", value = [[1]]),
        quote!("p.Tag", value = class("void[]")),
        quote!("p.Tag", value = arbitrary()),
        quote!("p.Tag", value = SOME_CONST),
    ] {
        assert!(
            expand(
                args.clone(),
                quote!(
                    struct Example;
                )
            )
            .is_err(),
            "{args}"
        );
    }
    for item in [
        quote!(
            enum Example {
                A,
            }
        ),
        quote!(
            fn example() {}
        ),
        quote!(impl Example {}),
    ] {
        assert!(expand(quote!("p.Tag"), item).is_err());
    }
}

#[test]
fn preserves_struct_and_encodes_retention_and_literals() {
    let item = expand(
        quote!(
            "p.Tag",
            retention = "class",
            values(retention = "element"),
            smallest = -9223372036854775808_i64,
            bits = -0.0_f32,
            kind = enum_value("p.Mode", "A"),
            nested = annotation("p.Nested", value = "é\0😀"),
            types = [class("int"), class("java.lang.String[]")]
        ),
        quote!(
            #[derive(Clone)]
            pub struct Example<T> {
                pub value: T,
            }
        ),
    )
    .unwrap();
    let item: syn::ItemStruct = syn::parse2(item).unwrap();
    assert_eq!(item.generics.params.len(), 1);
    assert_eq!(item.fields.len(), 1);
    assert!(item.attrs[0].path().is_ident("derive"));
    let Meta::NameValue(attr) = &item.attrs[1].meta else {
        panic!()
    };
    let Expr::Lit(ExprLit {
        lit: Lit::Str(payload),
        ..
    }) = &attr.value
    else {
        panic!()
    };
    let payload: Value = serde_json::from_str(&payload.value()).unwrap();
    assert_eq!(payload["retention"], "class");
    assert_eq!(payload["annotation"]["class"], "Lp/Tag;");
    let elements = payload["annotation"]["elements"].as_array().unwrap();
    let get = |name| &elements.iter().find(|v| v[0] == name).unwrap()[1]["value"];
    assert_eq!(get("retention"), "element");
    assert_eq!(get("smallest"), i64::MIN);
    assert_eq!(get("bits"), (-0.0_f32).to_bits());
    assert_eq!(get("nested")["elements"][0][1]["value"], "é\0😀");
    assert_eq!(get("types")[1]["value"], "[Ljava/lang/String;");
}
