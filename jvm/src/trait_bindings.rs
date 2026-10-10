use super::*;

pub(super) fn expand(args: &Args, mut item: syn::ItemTrait) -> syn::Result<syn::ItemTrait> {
    let (class, rename) = class_config(args, true)?;
    if !item.generics.params.is_empty() || item.generics.where_clause.is_some() {
        return Err(syn::Error::new_spanned(
            &item.generics,
            "foreign interface traits cannot be generic; declare the erased Java signature",
        ));
    }
    let class = class.unwrap();
    item.attrs
        .push(syn::parse_quote!(#[jvm_codegen::interface = #class]));
    let mut names = std::collections::HashSet::new();
    for member in &mut item.items {
        let syn::TraitItem::Fn(method) = member else {
            return Err(syn::Error::new_spanned(
                member,
                "foreign interface traits support only instance methods",
            ));
        };
        let sig = &method.sig;
        if !sig.generics.params.is_empty()
            || sig.generics.where_clause.is_some()
            || sig.asyncness.is_some()
            || sig.constness.is_some()
            || sig.abi.is_some()
            || sig.variadic.is_some()
            || !sig
                .receiver()
                .is_some_and(|r| r.reference.is_some() && r.colon_token.is_none())
        {
            return Err(syn::Error::new_spanned(
                sig,
                "foreign interface methods require &self or &mut self and a non-generic, synchronous Rust signature",
            ));
        }
        struct UnsupportedType(bool);
        impl<'ast> Visit<'ast> for UnsupportedType {
            fn visit_type_path(&mut self, ty: &'ast syn::TypePath) {
                self.0 |= ty
                    .path
                    .segments
                    .first()
                    .is_some_and(|segment| segment.ident == "Self");
                syn::visit::visit_type_path(self, ty);
            }
            fn visit_type_impl_trait(&mut self, _: &'ast syn::TypeImplTrait) {
                self.0 = true;
            }
        }
        let mut unsupported = UnsupportedType(false);
        for input in sig.inputs.iter().skip(1) {
            unsupported.visit_fn_arg(input);
        }
        unsupported.visit_return_type(&sig.output);
        if unsupported.0
            || method
                .attrs
                .iter()
                .any(|attr| attr.path().is_ident("track_caller"))
        {
            return Err(syn::Error::new_spanned(
                sig,
                "foreign interface methods need a fixed JVM signature without Self, impl Trait, or track_caller",
            ));
        }
        let mut name = rename.apply(&raw_ident_name(&sig.ident));
        let mut binding_seen = false;
        let mut attrs = Vec::new();
        for attr in std::mem::take(&mut method.attrs) {
            if let Some(kind) = BindingKind::from_attribute(&attr) {
                if binding_seen || kind? != BindingKind::Method {
                    return Err(syn::Error::new_spanned(
                        attr,
                        "foreign interface methods accept one #[jvm::method] attribute",
                    ));
                }
                binding_seen = true;
                let args = attribute_args(&attr)?;
                args.ensure_options(false, true, false, false, false)?;
                if args.positional.len() > 1 {
                    return Err(syn::Error::new_spanned(
                        attr,
                        "only a method name is supported; the Rust signature defines the JVM descriptor",
                    ));
                }
                if let Some(value) = args.name.as_ref().or(args.positional.first()) {
                    name = nonempty(value, "method name")?;
                }
            } else {
                attrs.push(attr);
            }
        }
        if name.contains(['.', ';', '[', '/', '<', '>']) {
            return Err(syn::Error::new_spanned(
                &sig.ident,
                "invalid JVM interface method name",
            ));
        }
        if !names.insert(name.clone()) {
            return Err(syn::Error::new_spanned(
                &sig.ident,
                "overloaded foreign interface methods are not supported",
            ));
        }
        attrs.push(syn::parse_quote!(#[jvm_codegen::method = #name]));
        method.attrs = attrs;
    }
    Ok(item)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_trait_bodies_and_maps_names() {
        let args = syn::parse_str(r#""example.Actions", rename_all = "camelCase""#).unwrap();
        let item = expand(
            &args,
            syn::parse_quote! {
                pub trait Actions {
                    fn do_work(&mut self);
                    #[jvm::method("size")]
                    fn len(&self) -> i32 { 4 }
                }
            },
        )
        .unwrap();
        let tokens = quote!(#item).to_string();
        assert!(tokens.contains("example/Actions"));
        assert!(tokens.contains("doWork"));
        assert!(tokens.contains("size"));
        assert!(tokens.contains("{ 4 }"));
        assert!(!tokens.contains("link_name"));
    }

    #[test]
    fn rejects_signatures_that_cannot_define_interface_methods() {
        let args = syn::parse_str(r#""example.Invalid""#).unwrap();
        for item in [
            quote!(
                trait Invalid<T> {
                    fn call(&self, value: T);
                }
            ),
            quote!(
                trait Invalid {
                    type Output;
                }
            ),
            quote!(
                trait Invalid {
                    fn call();
                }
            ),
            quote!(
                trait Invalid {
                    fn call(self);
                }
            ),
            quote!(
                trait Invalid {
                    fn call<T>(&self, value: T);
                }
            ),
            quote!(
                trait Invalid {
                    async fn call(&self);
                }
            ),
            quote!(
                trait Invalid {
                    fn call(&self) -> Self;
                }
            ),
            quote!(
                trait Invalid {
                    fn call(&self, other: &Self);
                }
            ),
            quote!(
                trait Invalid {
                    fn call(&self) -> impl Copy;
                }
            ),
            quote!(
                trait Invalid {
                    #[track_caller]
                    fn call(&self);
                }
            ),
            quote!(
                trait Invalid {
                    #[jvm::static_method]
                    fn call(&self);
                }
            ),
            quote!(
                trait Invalid {
                    #[jvm::method(descriptor = "()I")]
                    fn call(&self);
                }
            ),
            quote!(
                trait Invalid {
                    #[jvm::method("a")]
                    fn first(&self);
                    #[jvm::method("a")]
                    fn second(&self);
                }
            ),
            quote!(
                trait Invalid {
                    #[jvm::method("<init>")]
                    fn call(&self);
                }
            ),
        ] {
            assert!(
                expand(&args, syn::parse2(item.clone()).unwrap()).is_err(),
                "{item}"
            );
        }
    }
}
