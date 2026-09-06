//! Intern representation types once at the semantic-emission boundary.
use super::*;

#[derive(Default)]
pub(super) struct Vocabulary {
    pub types: ir::Types,
    ids: HashMap<oomir::Type, ir::TypeId>,
    descriptors: Vec<String>,
}
impl Vocabulary {
    pub fn add(&mut self, ty: &oomir::Type) -> ir::TypeId {
        if let Some(&id) = self.ids.get(ty) {
            return id;
        }
        use ir::Type as I;
        use oomir::Type as T;
        let repr = match ty {
            T::Void | T::Unit => I::Unit,
            T::Boolean => I::Scalar(ScalarType::Bool),
            T::Char | T::U16 => I::Scalar(ScalarType::U16),
            T::I8 => I::Scalar(ScalarType::I8),
            T::U8 => I::Scalar(ScalarType::U8),
            T::I16 => I::Scalar(ScalarType::I16),
            T::I32 => I::Scalar(ScalarType::I32),
            T::U32 => I::Scalar(ScalarType::U32),
            T::I64 => I::Scalar(ScalarType::I64),
            T::U64 => I::Scalar(ScalarType::U64),
            T::F16 => I::Scalar(ScalarType::F16),
            T::F32 => I::Scalar(ScalarType::F32),
            T::F64 => I::Scalar(ScalarType::F64),
            T::Class(name) => I::Class(self.types.symbol(name)),
            T::Interface(name) => I::Interface(self.types.symbol(name)),
            T::Pointer(inner) => I::Pointer(self.add(inner)),
            T::Slice(inner) => I::Slice(self.add(inner)),
            T::Array(inner) | T::MutableReference(inner) => {
                let inner = if inner.has_jvm_value() {
                    self.add(inner)
                } else {
                    self.add(&object())
                };
                I::Array(inner)
            }
            T::Reference(inner) => {
                let id = self.add(inner);
                self.ids.insert(ty.clone(), id);
                return id;
            }
            T::Str => I::Str,
        };
        let id = self.types.intern(repr);
        if id.index() == self.descriptors.len() {
            self.descriptors.push(ty.to_jvm_descriptor());
        }
        self.ids.insert(ty.clone(), id);
        if let T::Interface(name) = ty {
            self.ids.entry(T::Class(name.clone())).or_insert(id);
        }
        id
    }
    pub fn id(&self, ty: &oomir::Type) -> ir::TypeId {
        *self
            .ids
            .get(ty)
            .unwrap_or_else(|| panic!("unregistered emission type {ty:?}"))
    }
    pub fn same_carrier(&self, a: ir::TypeId, b: ir::TypeId) -> bool {
        self.descriptors[a.index()] == self.descriptors[b.index()]
    }
    pub fn add_wrappers(&mut self, context: &Context) {
        let mut pending = self.ids.keys().cloned().collect::<Vec<_>>();
        let mut seen = HashSet::default();
        while let Some(ty) = pending.pop() {
            let oomir::Type::Class(name) = ty else {
                continue;
            };
            if !seen.insert(name.clone()) {
                continue;
            }
            if let Some(fields) = context.wrappers.get(&name) {
                for (_, ty) in fields {
                    self.add(ty);
                    pending.push(ty.clone());
                }
            }
        }
    }
    pub fn signature(&mut self, signature: &oomir::Signature) {
        for (_, ty) in &signature.params {
            self.add(ty);
        }
        self.add(&signature.ret);
    }
    pub fn instruction(&mut self, instruction: &oomir::Instruction) {
        use oomir::Instruction::*;
        instruction.visit_operands(|operand| {
            self.add(&operand.get_type().unwrap());
        });
        if let InvokeVirtual { class_name, .. } | InvokeInterface { class_name, .. } = instruction {
            self.add(&oomir::Type::Class(class_name.clone()));
        }
        match instruction {
            InvokeStatic { method_ty, .. }
            | InvokeRustStatic { method_ty, .. }
            | InvokeVirtual { method_ty, .. }
            | InvokeInterface { method_ty, .. }
            | CallIndirect {
                signature: method_ty,
                ..
            } => self.signature(method_ty),
            CreateFunctionPointer {
                interface_name,
                signature,
                ..
            } => {
                self.add(&oomir::Type::Interface(interface_name.clone()));
                self.signature(signature);
            }
            NewArray { element_type, .. } => {
                self.add(&oomir::Type::Array(Box::new(element_type.clone())));
            }
            ConstructObject {
                class_name, args, ..
            } => {
                self.add(&oomir::Type::Class(class_name.clone()));
                for (_, ty) in args {
                    self.add(ty);
                }
            }
            SetField {
                owner_class,
                field_ty,
                ..
            }
            | GetField {
                owner_class,
                field_ty,
                ..
            } => {
                self.add(&oomir::Type::Class(owner_class.clone()));
                self.add(field_ty);
            }
            GetJvmField {
                class_name,
                field_ty,
                ..
            }
            | SetJvmField {
                class_name,
                field_ty,
                ..
            }
            | GetStaticField {
                class_name,
                field_ty,
                ..
            }
            | SetStaticField {
                class_name,
                field_ty,
                ..
            } => {
                self.add(&oomir::Type::Class(class_name.clone()));
                self.add(field_ty);
            }
            Cast { ty, .. } => {
                self.add(ty);
            }
            ThrowNewWithMessage {
                exception_class, ..
            } => {
                self.add(&oomir::Type::Class(exception_class.clone()));
            }
            _ => {}
        }
    }
}
pub(super) fn object() -> oomir::Type {
    oomir::Type::Class("java/lang/Object".into())
}

pub(crate) fn source_type(types: &ir::Types, id: ir::TypeId) -> oomir::Type {
    use ir::Type as I;
    use oomir::Type as T;
    match types.get(id).expect("valid representation type") {
        I::Unit | I::Opaque(_) => T::Unit,
        I::Scalar(ty) => match ty {
            ScalarType::Bool => T::Boolean,
            ScalarType::Char | ScalarType::U16 => T::U16,
            ScalarType::I8 => T::I8,
            ScalarType::U8 => T::U8,
            ScalarType::I16 => T::I16,
            ScalarType::I32 => T::I32,
            ScalarType::U32 => T::U32,
            ScalarType::I64 => T::I64,
            ScalarType::U64 => T::U64,
            ScalarType::F16 => T::F16,
            ScalarType::F32 => T::F32,
            ScalarType::F64 => T::F64,
            ScalarType::I128 => T::Class("org/rustlang/runtime/I128".into()),
            ScalarType::U128 => T::Class("org/rustlang/runtime/U128".into()),
        },
        I::Class(id) => T::Class(types.symbol_name(id).unwrap().into()),
        I::Interface(id) => T::Interface(types.symbol_name(id).unwrap().into()),
        I::Pointer(inner) => T::Pointer(Box::new(source_type(types, inner))),
        I::Array(inner) => T::Array(Box::new(source_type(types, inner))),
        I::Slice(inner) => T::Slice(Box::new(source_type(types, inner))),
        I::Str => T::Str,
    }
}
