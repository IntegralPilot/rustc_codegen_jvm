#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    Attribute, FnArg, Ident, ImplItem, ImplItemFn, ItemFn, LitStr, Meta, Pat, ReturnType,
    Signature, Token, Type,
    parse::{Parse, ParseStream, Parser},
    visit::Visit,
    visit_mut::VisitMut,
};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum RenameRule {
    #[default]
    None,
    CamelCase,
}

impl RenameRule {
    fn apply(self, name: &str) -> String {
        match self {
            Self::None => name.to_string(),
            Self::CamelCase => lower_camel_case(name),
        }
    }
}

#[derive(Clone, Default)]
struct Args {
    positional: Vec<LitStr>,
    class: Option<LitStr>,
    name: Option<LitStr>,
    descriptor: Option<LitStr>,
    rename_all: Option<LitStr>,
}

impl Parse for Args {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut result = Self::default();
        while !input.is_empty() {
            if input.peek(LitStr) {
                result.positional.push(input.parse()?);
            } else {
                let key: Ident = input.parse()?;
                input.parse::<Token![=]>()?;
                let value: LitStr = input.parse()?;
                let slot = match key.to_string().as_str() {
                    "class" => &mut result.class,
                    "name" => &mut result.name,
                    "descriptor" => &mut result.descriptor,
                    "rename_all" => &mut result.rename_all,
                    _ => {
                        return Err(syn::Error::new_spanned(
                            key,
                            "unknown JVM option; expected `class`, `name`, `descriptor`, or `rename_all`",
                        ));
                    }
                };
                if slot.replace(value).is_some() {
                    return Err(syn::Error::new_spanned(key, "duplicate JVM option"));
                }
            }

            if input.is_empty() {
                break;
            }
            input.parse::<Token![,]>()?;
        }
        Ok(result)
    }
}

impl Args {
    fn parse_tokens(tokens: TokenStream2) -> syn::Result<Self> {
        Self::parse.parse2(tokens)
    }

    fn parse_macro(tokens: TokenStream) -> syn::Result<Self> {
        Self::parse_tokens(tokens.into())
    }

    fn ensure_options(
        &self,
        class: bool,
        name: bool,
        descriptor: bool,
        rename_all: bool,
    ) -> syn::Result<()> {
        for (allowed, option, spelling) in [
            (class, self.class.as_ref(), "class"),
            (name, self.name.as_ref(), "name"),
            (descriptor, self.descriptor.as_ref(), "descriptor"),
            (rename_all, self.rename_all.as_ref(), "rename_all"),
        ] {
            if !allowed && let Some(value) = option {
                return Err(syn::Error::new_spanned(
                    value,
                    format!("`{spelling}` is not supported by this JVM attribute"),
                ));
            }
        }
        if !self.positional.is_empty()
            && (self.class.is_some() || self.name.is_some() || self.descriptor.is_some())
        {
            return Err(syn::Error::new_spanned(
                &self.positional[0],
                "do not mix positional binding arguments with `class`, `name`, or `descriptor`",
            ));
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BindingKind {
    StaticMethod,
    Method,
    Constructor,
    Field,
    StaticField,
}

impl BindingKind {
    fn from_attribute(attribute: &Attribute) -> Option<syn::Result<Self>> {
        let path = attribute.path();
        let (qualified, name) = match path.segments.len() {
            1 => (false, path.segments[0].ident.to_string()),
            2 if path.segments[0].ident == "jvm" || path.segments[0].ident == "rcj" => {
                (true, path.segments[1].ident.to_string())
            }
            _ => return None,
        };
        Some(match name.as_str() {
            "static_method" => Ok(Self::StaticMethod),
            "method" => Ok(Self::Method),
            "constructor" => Ok(Self::Constructor),
            "field" => Ok(Self::Field),
            "static_field" => Ok(Self::StaticField),
            "static" if qualified => Err(syn::Error::new_spanned(
                attribute,
                "use `#[jvm::static_method]`; `static` is a Rust keyword",
            )),
            _ if qualified => Err(syn::Error::new_spanned(
                attribute,
                format!(
                    "unknown JVM binding attribute `{}`; expected `static_method`, `method`, `constructor`, `field`, or `static_field`",
                    quote!(#path),
                ),
            )),
            _ => return None,
        })
    }
}

fn attribute_args(attribute: &Attribute) -> syn::Result<Args> {
    match &attribute.meta {
        Meta::Path(_) => Ok(Args::default()),
        Meta::List(list) => Args::parse_tokens(list.tokens.clone()),
        Meta::NameValue(value) => Err(syn::Error::new_spanned(
            value,
            "expected parentheses, for example `#[jvm::method(name = \"getValue\")]`",
        )),
    }
}

fn normalize_class(raw: &str) -> String {
    let raw = raw.strip_prefix("jvm:class:").unwrap_or(raw);
    if let Some((package, class)) = raw.rsplit_once('/') {
        return format!("{}/{}", package.replace('.', "/"), class.replace('.', "$"));
    }

    let segments = raw.split('.').collect::<Vec<_>>();
    let class_start = segments.iter().position(|segment| {
        segment.contains('$') || segment.chars().next().is_some_and(char::is_uppercase)
    });
    let Some(class_start) = class_start else {
        return raw.replace('.', "/");
    };

    let package = segments[..class_start].join("/");
    let class = segments[class_start..].join("$");
    if package.is_empty() {
        class
    } else {
        format!("{package}/{class}")
    }
}

fn nonempty(value: &LitStr, what: &str) -> syn::Result<String> {
    let value_string = value.value();
    if value_string.is_empty() {
        Err(syn::Error::new_spanned(
            value,
            format!("JVM {what} cannot be empty"),
        ))
    } else {
        Ok(value_string)
    }
}

fn raw_ident_name(ident: &Ident) -> String {
    let name = ident.to_string();
    name.strip_prefix("r#").unwrap_or(&name).to_string()
}

fn lower_camel_case(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    let mut uppercase_next = false;
    for character in name.chars() {
        if character == '_' {
            uppercase_next = !result.is_empty();
        } else if uppercase_next {
            result.extend(character.to_uppercase());
            uppercase_next = false;
        } else {
            result.push(character);
        }
    }
    result
}

fn returns_unit(signature: &Signature) -> bool {
    match &signature.output {
        ReturnType::Default => true,
        ReturnType::Type(_, ty) => {
            matches!(ty.as_ref(), Type::Tuple(tuple) if tuple.elems.is_empty())
        }
    }
}

fn inferred_name(signature: &Signature, kind: BindingKind, rename: RenameRule) -> String {
    let rust_name = raw_ident_name(&signature.ident);
    let base = match kind {
        BindingKind::Field | BindingKind::StaticField if returns_unit(signature) => {
            rust_name.strip_prefix("set_").unwrap_or(&rust_name)
        }
        BindingKind::Field | BindingKind::StaticField => {
            rust_name.strip_prefix("get_").unwrap_or(&rust_name)
        }
        _ => &rust_name,
    };
    rename.apply(base)
}

fn parse_rename_rule(args: &Args) -> syn::Result<RenameRule> {
    let Some(value) = &args.rename_all else {
        return Ok(RenameRule::None);
    };
    match value.value().as_str() {
        "camelCase" => Ok(RenameRule::CamelCase),
        _ => Err(syn::Error::new_spanned(
            value,
            "unsupported rename rule; the available rule is `camelCase`",
        )),
    }
}

fn class_config(args: &Args, required: bool) -> syn::Result<(Option<String>, RenameRule)> {
    args.ensure_options(true, false, false, true)?;
    if args.positional.len() > 1 {
        return Err(syn::Error::new_spanned(
            &args.positional[1],
            "expected at most one JVM class name",
        ));
    }
    let class = args
        .class
        .as_ref()
        .or_else(|| args.positional.first())
        .map(|value| nonempty(value, "class name"))
        .transpose()?
        .map(|value| normalize_class(&value));
    if required && class.is_none() {
        return Err(syn::Error::new(
            Span::call_site(),
            "a JVM class name is required, for example `#[jvm::class(\"java.lang.String\")]`",
        ));
    }
    Ok((class, parse_rename_rule(args)?))
}

fn named_value(value: Option<&LitStr>, what: &str) -> syn::Result<Option<String>> {
    value.map(|value| nonempty(value, what)).transpose()
}

fn positional_value(value: Option<&LitStr>, what: &str) -> syn::Result<Option<String>> {
    value.map(|value| nonempty(value, what)).transpose()
}

fn binding_link(
    kind: BindingKind,
    args: &Args,
    signature: &Signature,
    outer_class: Option<&str>,
    rename: RenameRule,
) -> syn::Result<String> {
    match kind {
        BindingKind::StaticMethod => {
            args.ensure_options(true, true, true, false)?;
            let inferred = inferred_name(signature, kind, rename);
            let (class, name, descriptor) = if args.positional.is_empty() {
                (
                    named_value(args.class.as_ref(), "class name")?
                        .or_else(|| outer_class.map(str::to_string)),
                    named_value(args.name.as_ref(), "method name")?.unwrap_or(inferred),
                    named_value(args.descriptor.as_ref(), "method descriptor")?,
                )
            } else {
                let values = &args.positional;
                match (outer_class, values.len()) {
                    (Some(class), 1) => (
                        Some(class.to_string()),
                        nonempty(&values[0], "method name")?,
                        None,
                    ),
                    (Some(_), 2) | (None, 2) => (
                        Some(nonempty(&values[0], "class name")?),
                        nonempty(&values[1], "method name")?,
                        None,
                    ),
                    (Some(_), 3) | (None, 3) => (
                        Some(nonempty(&values[0], "class name")?),
                        nonempty(&values[1], "method name")?,
                        Some(nonempty(&values[2], "method descriptor")?),
                    ),
                    (None, 1) => (Some(nonempty(&values[0], "class name")?), inferred, None),
                    _ => {
                        return Err(syn::Error::new_spanned(
                            values.last().unwrap(),
                            "expected a class, optional method name, and optional descriptor",
                        ));
                    }
                }
            };
            let class = class.ok_or_else(|| {
                syn::Error::new(
                    Span::call_site(),
                    "no JVM class is known; add `class = \"java.lang.Class\"` or put the method in a named `#[jvm::class]` impl",
                )
            })?;
            let class = normalize_class(&class);
            Ok(match descriptor {
                Some(descriptor) => format!("jvm:static:{class}:{name}:{descriptor}"),
                None => format!("jvm:static:{class}:{name}"),
            })
        }
        BindingKind::Method => {
            args.ensure_options(false, true, true, false)?;
            let (name, descriptor) = if args.positional.is_empty() {
                (
                    named_value(args.name.as_ref(), "method name")?
                        .unwrap_or_else(|| inferred_name(signature, kind, rename)),
                    named_value(args.descriptor.as_ref(), "method descriptor")?,
                )
            } else {
                match args.positional.len() {
                    1 => (nonempty(&args.positional[0], "method name")?, None),
                    2 => (
                        nonempty(&args.positional[0], "method name")?,
                        Some(nonempty(&args.positional[1], "method descriptor")?),
                    ),
                    _ => {
                        return Err(syn::Error::new_spanned(
                            args.positional.last().unwrap(),
                            "expected an optional method name and descriptor",
                        ));
                    }
                }
            };
            Ok(match descriptor {
                Some(descriptor) => format!("jvm:virtual:{name}:{descriptor}"),
                None => format!("jvm:virtual:{name}"),
            })
        }
        BindingKind::Constructor => {
            args.ensure_options(true, false, false, false)?;
            if args.positional.len() > 1 {
                return Err(syn::Error::new_spanned(
                    &args.positional[1],
                    "expected at most one JVM class name",
                ));
            }
            let class = named_value(args.class.as_ref(), "class name")?
                .or(positional_value(
                    args.positional.first(),
                    "class name",
                )?)
                .or_else(|| outer_class.map(str::to_string))
                .ok_or_else(|| {
                    syn::Error::new(
                        Span::call_site(),
                        "no JVM class is known; add `class = \"java.lang.Class\"` or put the constructor in a named `#[jvm::class]` impl",
                    )
                })?;
            Ok(format!("jvm:new:{}", normalize_class(&class)))
        }
        BindingKind::Field => {
            args.ensure_options(false, true, false, false)?;
            if args.positional.len() > 1 {
                return Err(syn::Error::new_spanned(
                    &args.positional[1],
                    "expected at most one JVM field name",
                ));
            }
            let name = named_value(args.name.as_ref(), "field name")?
                .or(positional_value(args.positional.first(), "field name")?)
                .unwrap_or_else(|| inferred_name(signature, kind, rename));
            Ok(format!("jvm:field:{name}"))
        }
        BindingKind::StaticField => {
            args.ensure_options(true, true, false, false)?;
            let inferred = inferred_name(signature, kind, rename);
            let (class, name) = if args.positional.is_empty() {
                (
                    named_value(args.class.as_ref(), "class name")?
                        .or_else(|| outer_class.map(str::to_string)),
                    named_value(args.name.as_ref(), "field name")?.unwrap_or(inferred),
                )
            } else {
                match (outer_class, args.positional.len()) {
                    (Some(class), 1) => (
                        Some(class.to_string()),
                        nonempty(&args.positional[0], "field name")?,
                    ),
                    (Some(_), 2) | (None, 2) => (
                        Some(nonempty(&args.positional[0], "class name")?),
                        nonempty(&args.positional[1], "field name")?,
                    ),
                    (None, 1) => (Some(nonempty(&args.positional[0], "class name")?), inferred),
                    _ => {
                        return Err(syn::Error::new_spanned(
                            args.positional.last().unwrap(),
                            "expected a class and optional field name",
                        ));
                    }
                }
            };
            let class = class.ok_or_else(|| {
                syn::Error::new(
                    Span::call_site(),
                    "no JVM class is known; add `class = \"java.lang.Class\"` or put the field in a named `#[jvm::class]` impl",
                )
            })?;
            Ok(format!(
                "jvm:static-field:{}:{name}",
                normalize_class(&class)
            ))
        }
    }
}

fn validate_receiver(receiver: &syn::Receiver) -> syn::Result<()> {
    if receiver.reference.is_none() || receiver.colon_token.is_some() {
        Err(syn::Error::new_spanned(
            receiver,
            "JVM instance wrappers require a shorthand `&self` or `&mut self` receiver",
        ))
    } else {
        Ok(())
    }
}

fn validate_wrapper_signature(signature: &Signature) -> syn::Result<()> {
    if let Some(asyncness) = &signature.asyncness {
        return Err(syn::Error::new_spanned(
            asyncness,
            "JVM wrapper functions cannot be `async`",
        ));
    }
    if let Some(constness) = &signature.constness {
        return Err(syn::Error::new_spanned(
            constness,
            "JVM wrapper functions cannot be `const`",
        ));
    }
    if !signature.generics.params.is_empty() || signature.generics.where_clause.is_some() {
        return Err(syn::Error::new_spanned(
            &signature.generics,
            "JVM wrapper functions cannot be generic",
        ));
    }
    if let Some(variadic) = &signature.variadic {
        return Err(syn::Error::new_spanned(
            variadic,
            "JVM wrapper functions cannot be variadic",
        ));
    }
    for input in &signature.inputs {
        match input {
            FnArg::Receiver(receiver) => validate_receiver(receiver)?,
            FnArg::Typed(typed) => match typed.pat.as_ref() {
                Pat::Ident(ident) if ident.by_ref.is_none() && ident.subpat.is_none() => {}
                _ => {
                    return Err(syn::Error::new_spanned(
                        &typed.pat,
                        "JVM wrapper parameters must be simple bindings such as `value: i32`",
                    ));
                }
            },
        }
    }
    Ok(())
}

fn validate_binding_shape(
    kind: BindingKind,
    signature: &Signature,
    in_class_impl: bool,
) -> syn::Result<()> {
    let receiver = signature.inputs.iter().find_map(|input| match input {
        FnArg::Receiver(receiver) => Some(receiver),
        FnArg::Typed(_) => None,
    });
    let typed_count = signature
        .inputs
        .iter()
        .filter(|input| matches!(input, FnArg::Typed(_)))
        .count();

    if in_class_impl {
        match kind {
            BindingKind::Method | BindingKind::Field if receiver.is_none() => {
                return Err(syn::Error::new_spanned(
                    &signature.ident,
                    "an instance JVM binding in a `#[jvm::class]` impl requires `&self` or `&mut self`",
                ));
            }
            BindingKind::StaticMethod | BindingKind::Constructor | BindingKind::StaticField
                if receiver.is_some() =>
            {
                return Err(syn::Error::new_spanned(
                    receiver.unwrap(),
                    "this JVM binding is static and cannot take `self`",
                ));
            }
            _ => {}
        }
    } else if receiver.is_some() {
        return Err(syn::Error::new_spanned(
            receiver.unwrap(),
            "methods using `self` must be inside a `#[jvm::class]` impl block",
        ));
    }

    match kind {
        BindingKind::Method if !in_class_impl && typed_count == 0 => Err(syn::Error::new_spanned(
            &signature.ident,
            "an instance JVM method requires the JVM receiver as its first parameter",
        )),
        BindingKind::Constructor if returns_unit(signature) => Err(syn::Error::new_spanned(
            &signature.output,
            "a JVM constructor binding must return the constructed object",
        )),
        BindingKind::Field => {
            let expected_getter = if in_class_impl { 0 } else { 1 };
            let expected_setter = expected_getter + 1;
            if in_class_impl
                && returns_unit(signature)
                && receiver.is_some_and(|receiver| receiver.mutability.is_none())
            {
                return Err(syn::Error::new_spanned(
                    receiver.unwrap(),
                    "an instance field setter requires `&mut self`",
                ));
            }
            if (!returns_unit(signature) && typed_count != expected_getter)
                || (returns_unit(signature) && typed_count != expected_setter)
            {
                Err(syn::Error::new_spanned(
                    &signature.inputs,
                    if in_class_impl {
                        "an instance field getter takes only `&self`; a setter takes `&mut self` and one value"
                    } else {
                        "an instance field getter takes one receiver; a setter takes a receiver and one value"
                    },
                ))
            } else {
                Ok(())
            }
        }
        BindingKind::StaticField => {
            if (!returns_unit(signature) && typed_count != 0)
                || (returns_unit(signature) && typed_count != 1)
            {
                Err(syn::Error::new_spanned(
                    &signature.inputs,
                    "a static field getter takes no parameters; a setter takes exactly one value",
                ))
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}

struct SelfReplacer {
    concrete: Type,
}

impl VisitMut for SelfReplacer {
    fn visit_type_mut(&mut self, ty: &mut Type) {
        if let Type::Path(path) = ty
            && path.qself.is_none()
            && path.path.is_ident("Self")
        {
            *ty = self.concrete.clone();
            return;
        }
        syn::visit_mut::visit_type_mut(self, ty);
    }

    fn visit_path_mut(&mut self, path: &mut syn::Path) {
        if path.leading_colon.is_none()
            && !path.segments.is_empty()
            && path.segments[0].ident == "Self"
        {
            let Type::Path(concrete) = &self.concrete else {
                unreachable!("concrete JVM impl types are paths")
            };
            let tail = path.segments.iter().skip(1).cloned().collect::<Vec<_>>();
            path.leading_colon = concrete.path.leading_colon;
            path.segments = concrete.path.segments.clone();
            path.segments.extend(tail);
        }
        syn::visit_mut::visit_path_mut(self, path);
    }
}

struct FindsSelf(bool);

impl<'ast> Visit<'ast> for FindsSelf {
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        if node.qself.is_none() && node.path.is_ident("Self") {
            self.0 = true;
        }
        syn::visit::visit_type_path(self, node);
    }

    fn visit_path(&mut self, node: &'ast syn::Path) {
        if !node.segments.is_empty() && node.segments[0].ident == "Self" {
            self.0 = true;
        }
        syn::visit::visit_path(self, node);
    }
}

fn wrapper_body(
    signature: &Signature,
    link: &str,
    concrete: Option<&Type>,
) -> syn::Result<syn::Block> {
    validate_wrapper_signature(signature)?;
    let hidden = format_ident!("__jvm_{}", signature.ident);
    let mut replacer = concrete.map(|concrete| SelfReplacer {
        concrete: concrete.clone(),
    });
    let mut hidden_inputs = Vec::new();
    let mut call_args = Vec::new();

    for input in &signature.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                let concrete = concrete.ok_or_else(|| {
                    syn::Error::new_spanned(
                        receiver,
                        "methods using `self` must be inside a `#[jvm::class]` impl block",
                    )
                })?;
                let mutability = &receiver.mutability;
                hidden_inputs.push(quote! { __this: &#mutability #concrete });
                call_args.push(quote! { self });
            }
            FnArg::Typed(typed) => {
                let Pat::Ident(pattern) = typed.pat.as_ref() else {
                    unreachable!("validated by validate_wrapper_signature")
                };
                let ident = &pattern.ident;
                let mut ty = (*typed.ty).clone();
                if let Some(replacer) = &mut replacer {
                    replacer.visit_type_mut(&mut ty);
                }
                hidden_inputs.push(quote! { #ident: #ty });
                call_args.push(quote! { #ident });
            }
        }
    }

    let mut output = signature.output.clone();
    if let Some(replacer) = &mut replacer {
        replacer.visit_return_type_mut(&mut output);
    }
    syn::parse2(quote! {{
        unsafe extern "C" {
            #[link_name = #link]
            fn #hidden(#(#hidden_inputs),*) #output;
        }
        unsafe { #hidden(#(#call_args),*) }
    }})
}

fn wrap_impl_function(
    mut function: ImplItemFn,
    link: &str,
    concrete: Option<&Type>,
) -> syn::Result<ImplItemFn> {
    if concrete.is_none() {
        let mut finder = FindsSelf(false);
        finder.visit_signature(&function.sig);
        if finder.0 {
            return Err(syn::Error::new_spanned(
                &function.sig,
                "methods using `Self` must be inside a `#[jvm::class]` impl block",
            ));
        }
    }
    function.block = wrapper_body(&function.sig, link, concrete)?;
    Ok(function)
}

fn wrap_item_function(mut function: ItemFn, link: &str) -> syn::Result<ItemFn> {
    function.block = Box::new(wrapper_body(&function.sig, link, None)?);
    Ok(function)
}

fn concrete_impl_type(impl_block: &syn::ItemImpl) -> syn::Result<Type> {
    if impl_block.trait_.is_some() {
        return Err(syn::Error::new_spanned(
            impl_block,
            "`#[jvm::class]` cannot wrap a trait impl",
        ));
    }
    if !impl_block.generics.params.is_empty() || impl_block.generics.where_clause.is_some() {
        return Err(syn::Error::new_spanned(
            &impl_block.generics,
            "a `#[jvm::class]` impl cannot be generic",
        ));
    }
    match impl_block.self_ty.as_ref() {
        Type::Path(path) if path.qself.is_none() => {
            if path
                .path
                .segments
                .iter()
                .any(|segment| !segment.arguments.is_none())
            {
                Err(syn::Error::new_spanned(
                    &impl_block.self_ty,
                    "a `#[jvm::class]` impl must use a concrete non-generic type",
                ))
            } else {
                Ok((*impl_block.self_ty).clone())
            }
        }
        _ => Err(syn::Error::new_spanned(
            &impl_block.self_ty,
            "a `#[jvm::class]` impl must use a concrete path type such as `impl JavaString`",
        )),
    }
}

fn declared_type_ident(impl_block: &syn::ItemImpl) -> syn::Result<Ident> {
    match impl_block.self_ty.as_ref() {
        Type::Path(path)
            if path.qself.is_none()
                && path.path.leading_colon.is_none()
                && path.path.segments.len() == 1
                && path.path.segments[0].arguments.is_none() =>
        {
            Ok(path.path.segments[0].ident.clone())
        }
        _ => Err(syn::Error::new_spanned(
            &impl_block.self_ty,
            "a declaring `#[jvm::class]` impl must use a new unqualified type name such as `impl JavaString`; use `#[jvm::bindings]` for an existing or qualified type",
        )),
    }
}

fn expand_class_impl(
    mut impl_block: syn::ItemImpl,
    outer_class: Option<String>,
    rename: RenameRule,
) -> syn::Result<(syn::ItemImpl, Vec<Ident>)> {
    let concrete = concrete_impl_type(&impl_block)?;
    let mut expanded = Vec::with_capacity(impl_block.items.len());
    let mut direct_imports = Vec::new();

    for item in impl_block.items {
        let ImplItem::Fn(mut function) = item else {
            expanded.push(item);
            continue;
        };
        let mut binding = None;
        let mut kept_attributes = Vec::new();
        for attribute in function.attrs {
            if let Some(kind) = BindingKind::from_attribute(&attribute) {
                if binding.is_some() {
                    return Err(syn::Error::new_spanned(
                        attribute,
                        "only one `#[jvm::...]` binding attribute is allowed per method",
                    ));
                }
                if attribute.path().segments.len() == 1 {
                    let ident = attribute.path().segments[0].ident.clone();
                    if !direct_imports.contains(&ident) {
                        direct_imports.push(ident);
                    }
                }
                binding = Some((kind?, attribute_args(&attribute)?));
            } else {
                kept_attributes.push(attribute);
            }
        }
        function.attrs = kept_attributes;

        let Some((kind, args)) = binding else {
            if function.block.stmts.is_empty() {
                return Err(syn::Error::new_spanned(
                    &function.sig,
                    "empty methods in a JVM binding impl need a `#[jvm::method]`, `#[jvm::static_method]`, `#[jvm::constructor]`, `#[jvm::field]`, or `#[jvm::static_field]` attribute",
                ));
            }
            expanded.push(ImplItem::Fn(function));
            continue;
        };
        validate_binding_shape(kind, &function.sig, true)?;
        let link = binding_link(kind, &args, &function.sig, outer_class.as_deref(), rename)?;
        expanded.push(ImplItem::Fn(wrap_impl_function(
            function,
            &link,
            Some(&concrete),
        )?));
    }
    impl_block.items = expanded;
    Ok((impl_block, direct_imports))
}

fn direct_import_uses(imports: &[Ident]) -> TokenStream2 {
    quote! {
        #(
            #[allow(unused_imports)]
            use #imports as _;
        )*
    }
}

fn expand_binding(args: Args, item: TokenStream, kind: BindingKind) -> syn::Result<TokenStream2> {
    let tokens = TokenStream2::from(item);

    if let Ok(mut function) = syn::parse2::<syn::ForeignItemFn>(tokens.clone()) {
        validate_binding_shape(kind, &function.sig, false)?;
        let link = binding_link(kind, &args, &function.sig, None, RenameRule::None)?;
        let link_attribute: Attribute = syn::parse_quote!(#[link_name = #link]);
        function.attrs.push(link_attribute);
        return Ok(quote! { #function });
    }
    if let Ok(function) = syn::parse2::<ItemFn>(tokens.clone()) {
        validate_binding_shape(kind, &function.sig, false)?;
        let link = binding_link(kind, &args, &function.sig, None, RenameRule::None)?;
        let wrapped = wrap_item_function(function, &link)?;
        return Ok(quote! { #wrapped });
    }
    if let Ok(function) = syn::parse2::<ImplItemFn>(tokens.clone()) {
        validate_binding_shape(kind, &function.sig, false)?;
        let link = binding_link(kind, &args, &function.sig, None, RenameRule::None)?;
        let wrapped = wrap_impl_function(function, &link, None)?;
        return Ok(quote! { #wrapped });
    }

    Err(syn::Error::new_spanned(
        tokens,
        "this JVM binding attribute can only be used on a function",
    ))
}

fn run_binding_macro(args: TokenStream, item: TokenStream, kind: BindingKind) -> TokenStream {
    let result = Args::parse_macro(args).and_then(|args| expand_binding(args, item, kind));
    match result {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

/// Declares an opaque JVM class and its Rust binding methods in one `impl`.
///
/// Class names may use dots or slashes. On an `impl`, `rename_all = "camelCase"`
/// converts inferred Rust member names while leaving explicit names unchanged.
#[proc_macro_attribute]
pub fn class(args: TokenStream, item: TokenStream) -> TokenStream {
    let args = match Args::parse_macro(args) {
        Ok(args) => args,
        Err(error) => return error.to_compile_error().into(),
    };
    let tokens = TokenStream2::from(item);

    if let Ok(impl_block) = syn::parse2::<syn::ItemImpl>(tokens.clone()) {
        let (class, rename) = match class_config(&args, true) {
            Ok(config) => config,
            Err(error) => return error.to_compile_error().into(),
        };
        let ident = match declared_type_ident(&impl_block) {
            Ok(ident) => ident,
            Err(error) => return error.to_compile_error().into(),
        };
        let class = class.unwrap();
        return match expand_class_impl(impl_block, Some(class.clone()), rename) {
            Ok((expanded, imports)) => {
                let imports = direct_import_uses(&imports);
                quote! {
                    unsafe extern "C" {
                        #[link_name = #class]
                        pub type #ident;
                    }

                    #imports
                    #expanded
                }
                .into()
            }
            Err(error) => error.to_compile_error().into(),
        };
    }

    let (class, rename) = match class_config(&args, true) {
        Ok(config) => config,
        Err(error) => return error.to_compile_error().into(),
    };
    if rename != RenameRule::None {
        return syn::Error::new_spanned(
            args.rename_all.unwrap(),
            "`rename_all` belongs on the `#[jvm::class]` impl block",
        )
        .to_compile_error()
        .into();
    }
    let class = class.unwrap();

    if let Ok(mut foreign_type) = syn::parse2::<syn::ForeignItemType>(tokens.clone()) {
        let link_attribute: Attribute = syn::parse_quote!(#[link_name = #class]);
        foreign_type.attrs.push(link_attribute);
        return quote! { #foreign_type }.into();
    }
    if let Ok(struct_item) = syn::parse2::<syn::ItemStruct>(tokens.clone()) {
        if !matches!(struct_item.fields, syn::Fields::Unit) {
            return syn::Error::new_spanned(
                struct_item.fields,
                "a JVM class declaration must be an opaque unit struct such as `pub struct JavaString;`",
            )
            .to_compile_error()
            .into();
        }
        if !struct_item.generics.params.is_empty() || struct_item.generics.where_clause.is_some() {
            return syn::Error::new_spanned(
                struct_item.generics,
                "a JVM class declaration cannot be generic",
            )
            .to_compile_error()
            .into();
        }
        let attributes = &struct_item.attrs;
        let visibility = &struct_item.vis;
        let ident = &struct_item.ident;
        return quote! {
            unsafe extern "C" {
                #(#attributes)*
                #[link_name = #class]
                #visibility type #ident;
            }
        }
        .into();
    }

    syn::Error::new_spanned(
        tokens,
        "`#[jvm::class]` can only be used on an opaque unit struct, foreign type, or inherent impl",
    )
    .to_compile_error()
    .into()
}

/// Adds JVM binding methods to a type that has already been declared.
///
/// This is mainly useful for additional impl blocks. An optional class name is
/// required only by constructors and static members that do not name a class
/// themselves.
#[proc_macro_attribute]
pub fn bindings(args: TokenStream, item: TokenStream) -> TokenStream {
    let args = match Args::parse_macro(args) {
        Ok(args) => args,
        Err(error) => return error.to_compile_error().into(),
    };
    let (class, rename) = match class_config(&args, false) {
        Ok(config) => config,
        Err(error) => return error.to_compile_error().into(),
    };
    let tokens = TokenStream2::from(item);
    let impl_block = match syn::parse2::<syn::ItemImpl>(tokens.clone()) {
        Ok(impl_block) => impl_block,
        Err(_) => {
            return syn::Error::new_spanned(
                tokens,
                "`#[jvm::bindings]` can only be used on an inherent impl block",
            )
            .to_compile_error()
            .into();
        }
    };
    match expand_class_impl(impl_block, class, rename) {
        Ok((expanded, imports)) => {
            let imports = direct_import_uses(&imports);
            quote! {
                #imports
                #expanded
            }
            .into()
        }
        Err(error) => error.to_compile_error().into(),
    }
}

/// Binds a JVM static method.
///
/// Use `class = "..."`, `name = "..."`, and `descriptor = "..."` when the
/// corresponding value cannot be inferred from the enclosing class or Rust name.
#[proc_macro_attribute]
pub fn static_method(args: TokenStream, item: TokenStream) -> TokenStream {
    run_binding_macro(args, item, BindingKind::StaticMethod)
}

/// Binds a JVM virtual/interface method.
#[proc_macro_attribute]
pub fn method(args: TokenStream, item: TokenStream) -> TokenStream {
    run_binding_macro(args, item, BindingKind::Method)
}

/// Binds a JVM constructor.
#[proc_macro_attribute]
pub fn constructor(args: TokenStream, item: TokenStream) -> TokenStream {
    run_binding_macro(args, item, BindingKind::Constructor)
}

/// Binds an instance-field getter or setter.
///
/// The field name is inferred from `value`, `get_value`, or `set_value` when it
/// is omitted. A getter returns a value; a setter returns `()`.
#[proc_macro_attribute]
pub fn field(args: TokenStream, item: TokenStream) -> TokenStream {
    run_binding_macro(args, item, BindingKind::Field)
}

/// Binds a static-field getter or setter.
///
/// The class must be supplied unless the function is in a named
/// `#[jvm::class]` impl.
#[proc_macro_attribute]
pub fn static_field(args: TokenStream, item: TokenStream) -> TokenStream {
    run_binding_macro(args, item, BindingKind::StaticField)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(source: &str) -> Args {
        syn::parse_str(source).unwrap()
    }

    fn signature(source: &str) -> Signature {
        syn::parse_str::<syn::ItemFn>(source).unwrap().sig
    }

    #[test]
    fn binding_attributes_accept_qualified_and_directly_imported_forms() {
        for attribute in [
            syn::parse_quote!(#[jvm::method]),
            syn::parse_quote!(#[rcj::method]),
            syn::parse_quote!(#[method]),
        ] {
            assert_eq!(
                BindingKind::from_attribute(&attribute).unwrap().unwrap(),
                BindingKind::Method
            );
        }

        let unrelated: Attribute = syn::parse_quote!(#[inline]);
        assert!(BindingKind::from_attribute(&unrelated).is_none());
    }

    #[test]
    fn class_names_recognise_packages_and_nested_classes() {
        assert_eq!(
            normalize_class("java.time.LocalDate"),
            "java/time/LocalDate"
        );
        assert_eq!(normalize_class("Main.Counter"), "Main$Counter");
        assert_eq!(
            normalize_class("java.util.Map.Entry"),
            "java/util/Map$Entry"
        );
        assert_eq!(
            normalize_class("java.lang.Thread.State"),
            "java/lang/Thread$State"
        );
        assert_eq!(
            normalize_class("java.util.Map$Entry"),
            "java/util/Map$Entry"
        );
        assert_eq!(
            normalize_class("java/util/Map.Entry"),
            "java/util/Map$Entry"
        );
        assert_eq!(
            normalize_class("java.util/Map.Entry"),
            "java/util/Map$Entry"
        );
        assert_eq!(
            normalize_class("java/util/Outer.Inner.Deep"),
            "java/util/Outer$Inner$Deep"
        );
        assert_eq!(
            normalize_class("Unconventional/package/lowercase.inner"),
            "Unconventional/package/lowercase$inner"
        );
        assert_eq!(
            normalize_class("unconventional.package.lowercase"),
            "unconventional/package/lowercase"
        );
    }

    #[test]
    fn static_member_shorthand_depends_only_on_whether_a_class_is_supplied() {
        let sig = signature("fn rust_name() {}");
        assert_eq!(
            binding_link(
                BindingKind::StaticMethod,
                &args("\"javaName\""),
                &sig,
                Some("example/Owner"),
                RenameRule::None,
            )
            .unwrap(),
            "jvm:static:example/Owner:javaName"
        );
        assert_eq!(
            binding_link(
                BindingKind::StaticMethod,
                &args("class = \"other.Owner\", name = \"javaName\""),
                &sig,
                Some("example/Owner"),
                RenameRule::None,
            )
            .unwrap(),
            "jvm:static:other/Owner:javaName"
        );
    }

    #[test]
    fn field_accessors_and_camel_case_are_inferred() {
        let getter = signature("fn get_shared_state(&self) -> i32 { 0 }");
        let setter = signature("fn set_shared_state(&mut self, value: i32) {}");
        assert_eq!(
            inferred_name(&getter, BindingKind::Field, RenameRule::CamelCase),
            "sharedState"
        );
        assert_eq!(
            inferred_name(&setter, BindingKind::Field, RenameRule::CamelCase),
            "sharedState"
        );
    }

    #[test]
    fn named_and_positional_binding_arguments_do_not_mix() {
        let sig = signature("fn value() -> i32 { 0 }");
        let error = binding_link(
            BindingKind::StaticField,
            &args("\"Owner\", name = \"value\""),
            &sig,
            None,
            RenameRule::None,
        )
        .unwrap_err();
        assert!(error.to_string().contains("do not mix"));
    }

    #[test]
    fn unsafe_wrapper_shapes_are_rejected_early() {
        assert!(
            validate_wrapper_signature(&signature("async fn call() {}"))
                .unwrap_err()
                .to_string()
                .contains("cannot be `async`")
        );
        assert!(
            validate_wrapper_signature(&signature("const fn call() {}"))
                .unwrap_err()
                .to_string()
                .contains("cannot be `const`")
        );
        assert!(
            validate_wrapper_signature(&signature("fn call<T>(value: T) {}"))
                .unwrap_err()
                .to_string()
                .contains("cannot be generic")
        );
        assert!(
            validate_binding_shape(
                BindingKind::Field,
                &signature("fn set_value(&self, value: i32) {}"),
                true,
            )
            .unwrap_err()
            .to_string()
            .contains("requires `&mut self`")
        );
    }

    #[test]
    fn trait_impls_are_not_treated_as_jvm_classes() {
        let item: syn::ItemImpl = syn::parse_quote! {
            impl Display for JavaString {}
        };
        assert!(
            concrete_impl_type(&item)
                .err()
                .unwrap()
                .to_string()
                .contains("trait impl")
        );
    }

    #[test]
    fn unannotated_empty_methods_are_not_silent_noops() {
        let item: syn::ItemImpl = syn::parse_quote! {
            impl JavaString {
                fn accidentally_unbound(&self) {}
            }
        };
        assert!(
            expand_class_impl(item, Some("java/lang/String".to_string()), RenameRule::None)
                .err()
                .unwrap()
                .to_string()
                .contains("need a `#[jvm::method]`")
        );
    }
}
