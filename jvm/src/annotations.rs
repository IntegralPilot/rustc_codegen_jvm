use super::*;
use serde_json::{Value, json};
use syn::{Expr, ExprLit, Lit, UnOp, punctuated::Punctuated, spanned::Spanned};

struct AnnotationArgs {
    annotation: Value,
    retention: String,
}

impl Parse for AnnotationArgs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let class = object_descriptor(&input.parse::<LitStr>()?)?;
        let mut elements = Vec::new();
        let mut retention = None;
        while !input.is_empty() {
            input.parse::<Token![,]>()?;
            if input.is_empty() {
                break;
            }
            let name: Ident = input.parse()?;
            if name == "values" && input.peek(syn::token::Paren) {
                let content;
                syn::parenthesized!(content in input);
                let args = Punctuated::<syn::MetaNameValue, Token![,]>::parse_terminated(&content)?;
                for arg in args {
                    let name = arg.path.get_ident().ok_or_else(|| {
                        syn::Error::new_spanned(&arg.path, "expected an annotation element name")
                    })?;
                    add_element(&mut elements, name, arg.value)?;
                }
            } else {
                input.parse::<Token![=]>()?;
                if name == "retention" {
                    let value: LitStr = input.parse()?;
                    if retention.replace(value.value()).is_some() {
                        return Err(syn::Error::new_spanned(name, "duplicate retention option"));
                    }
                    if !matches!(value.value().as_str(), "runtime" | "class" | "source") {
                        return Err(syn::Error::new_spanned(
                            value,
                            "expected runtime, class, or source",
                        ));
                    }
                } else {
                    add_element(&mut elements, &name, input.parse()?)?;
                }
            }
        }
        elements.sort_by(|a, b| a.0.cmp(&b.0));
        Ok(Self {
            annotation: json!({"class": class, "elements": elements}),
            retention: retention.unwrap_or_else(|| "runtime".into()),
        })
    }
}

fn add_element(elements: &mut Vec<(String, Value)>, name: &Ident, expr: Expr) -> syn::Result<()> {
    let key = raw_ident_name(name);
    if elements.iter().any(|(existing, _)| *existing == key) {
        return Err(syn::Error::new_spanned(
            name,
            "duplicate annotation element",
        ));
    }
    elements.push((key, value(&expr)?));
    Ok(())
}

fn object_descriptor(name: &LitStr) -> syn::Result<String> {
    let class = normalize_class(&nonempty(name, "annotation class")?);
    if class.split('/').any(str::is_empty)
        || class
            .chars()
            .any(|c| c.is_whitespace() || matches!(c, ';' | '[' | '(' | ')' | '\0'))
    {
        return Err(syn::Error::new_spanned(name, "expected a Java class name"));
    }
    Ok(format!("L{class};"))
}

fn class_descriptor(name: &LitStr) -> syn::Result<String> {
    let mut raw = name.value();
    let mut dimensions = 0;
    while let Some(element) = raw.strip_suffix("[]") {
        raw = element.to_owned();
        dimensions += 1;
    }
    if dimensions > 255 || (dimensions != 0 && raw == "void") {
        return Err(syn::Error::new_spanned(name, "invalid Java array class"));
    }
    let scalar = match raw.as_str() {
        "boolean" => "Z".into(),
        "byte" => "B".into(),
        "short" => "S".into(),
        "char" => "C".into(),
        "int" => "I".into(),
        "long" => "J".into(),
        "float" => "F".into(),
        "double" => "D".into(),
        "void" => "V".into(),
        _ => object_descriptor(&LitStr::new(&raw, name.span()))?,
    };
    Ok(format!("{}{scalar}", "[".repeat(dimensions)))
}

fn tagged(kind: &str, value: impl Into<Value>) -> Value {
    json!({"kind": kind, "value": value.into()})
}

fn value(expr: &Expr) -> syn::Result<Value> {
    let invalid = || {
        syn::Error::new_spanned(
            expr,
            "expected a Java annotation literal, array, class(...), enum_value(...), or annotation(...)",
        )
    };
    match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Str(s), ..
        }) => Ok(tagged("String", s.value())),
        Expr::Lit(ExprLit {
            lit: Lit::Bool(b), ..
        }) => Ok(tagged("Boolean", b.value)),
        Expr::Lit(ExprLit {
            lit: Lit::Char(c), ..
        }) => {
            let v = u16::try_from(c.value() as u32).map_err(|_| {
                syn::Error::new_spanned(c, "Java char values must fit one UTF-16 code unit")
            })?;
            Ok(tagged("Char", v))
        }
        Expr::Lit(ExprLit {
            lit: Lit::Int(_) | Lit::Float(_),
            ..
        }) => number(expr, false),
        Expr::Unary(unary) if matches!(unary.op, UnOp::Neg(_)) => number(&unary.expr, true),
        Expr::Array(array) => {
            let values = array
                .elems
                .iter()
                .map(value)
                .collect::<syn::Result<Vec<_>>>()?;
            let identity = |v: &Value| (v["kind"].clone(), v["value"].get("class").cloned());
            if values.iter().any(|v| {
                v["kind"] == "Array"
                    || values
                        .first()
                        .is_some_and(|first| identity(first) != identity(v))
            }) {
                return Err(syn::Error::new_spanned(
                    expr,
                    "annotation arrays need one element type and cannot contain arrays",
                ));
            }
            Ok(tagged("Array", values))
        }
        Expr::Call(call) => {
            let Expr::Path(path) = &*call.func else {
                return Err(invalid());
            };
            let name = path
                .path
                .get_ident()
                .map(ToString::to_string)
                .unwrap_or_default();
            let string = |i: usize| match call.args.iter().nth(i) {
                Some(Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                })) => Ok(s),
                _ => Err(invalid()),
            };
            match name.as_str() {
                "class" if call.args.len() == 1 => {
                    Ok(tagged("Class", class_descriptor(string(0)?)?))
                }
                "enum_value" if call.args.len() == 2 => {
                    let constant = nonempty(string(1)?, "enum constant")?;
                    if constant.contains(['.', ';', '[', '/', '\0']) {
                        return Err(invalid());
                    }
                    Ok(tagged(
                        "Enum",
                        json!({"class": object_descriptor(string(0)?)?, "constant": constant}),
                    ))
                }
                "annotation" => {
                    let args = &call.args;
                    let nested: AnnotationArgs = syn::parse2(quote!(#args))?;
                    if nested.retention != "runtime" {
                        return Err(syn::Error::new_spanned(
                            expr,
                            "nested annotations do not have a retention option",
                        ));
                    }
                    Ok(tagged("Annotation", nested.annotation))
                }
                _ => Err(invalid()),
            }
        }
        _ => Err(invalid()),
    }
}

fn number(expr: &Expr, negative: bool) -> syn::Result<Value> {
    let Expr::Lit(ExprLit { lit, .. }) = expr else {
        return Err(syn::Error::new_spanned(expr, "expected a numeric literal"));
    };
    let (digits, suffix, float) = match lit {
        Lit::Int(v) => (v.base10_digits(), v.suffix(), false),
        Lit::Float(v) => (v.base10_digits(), v.suffix(), true),
        _ => return Err(syn::Error::new_spanned(expr, "expected a numeric literal")),
    };
    let text = format!("{}{digits}", if negative { "-" } else { "" });
    let invalid = || syn::Error::new_spanned(expr, "invalid Java numeric value or Rust suffix");
    if float || matches!(suffix, "f32" | "f64") {
        return match suffix {
            "f32" => text
                .parse::<f32>()
                .ok()
                .filter(|v| v.is_finite())
                .map(|v| tagged("Float", v.to_bits())),
            "" | "f64" => text
                .parse::<f64>()
                .ok()
                .filter(|v| v.is_finite())
                .map(|v| tagged("Double", v.to_bits())),
            _ => None,
        }
        .ok_or_else(invalid);
    }
    let n = text.parse::<i128>().map_err(|_| invalid())?;
    match suffix {
        "i8" => i8::try_from(n).ok().map(|v| tagged("Byte", v)),
        "i16" => i16::try_from(n).ok().map(|v| tagged("Short", v)),
        "" | "i32" => i32::try_from(n).ok().map(|v| tagged("Int", v)),
        "i64" => i64::try_from(n).ok().map(|v| tagged("Long", v)),
        "u16" => u16::try_from(n).ok().map(|v| tagged("Char", v)),
        _ => None,
    }
    .ok_or_else(invalid)
}

pub(super) fn expand(args: TokenStream2, item: TokenStream2) -> syn::Result<TokenStream2> {
    let args: AnnotationArgs = syn::parse2(args)?;
    let mut item: syn::ItemStruct = syn::parse2(item).map_err(|error| {
        syn::Error::new(error.span(), "#[jvm::annotation] requires a Rust struct")
    })?;
    let payload = json!({"retention": args.retention, "annotation": args.annotation}).to_string();
    let payload = LitStr::new(&payload, item.span());
    item.attrs
        .push(syn::parse_quote!(#[jvm_codegen::annotation = #payload]));
    Ok(quote!(#item))
}

#[cfg(test)]
mod tests;
