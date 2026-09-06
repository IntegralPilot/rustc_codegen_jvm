//! Intern representation types once at the semantic-emission boundary.
use super::*;
use std::sync::OnceLock;

pub(super) struct Vocabulary {
    pub types: ir::Types,
    ids: HashMap<oomir::Type, ir::TypeId>,
    descriptors: Vec<String>,
}
const COMMON_DESCRIPTORS: [&str; 16] = [
    "V",
    "Z",
    "B",
    "B",
    "S",
    "C",
    "I",
    "I",
    "J",
    "J",
    "S",
    "F",
    "D",
    "Ljava/lang/Object;",
    "Ljava/lang/String;",
    "Ljava/lang/Throwable;",
];

fn common_types() -> &'static Arc<ir::Types> {
    static TYPES: OnceLock<Arc<ir::Types>> = OnceLock::new();
    TYPES.get_or_init(|| {
        let mut types = ir::Types::default();
        types.intern(ir::Type::Unit);
        for ty in [
            ScalarType::Bool,
            ScalarType::I8,
            ScalarType::U8,
            ScalarType::I16,
            ScalarType::U16,
            ScalarType::I32,
            ScalarType::U32,
            ScalarType::I64,
            ScalarType::U64,
            ScalarType::F16,
            ScalarType::F32,
            ScalarType::F64,
        ] {
            types.scalar(ty);
        }
        for name in [
            "java/lang/Object",
            "java/lang/String",
            "java/lang/Throwable",
        ] {
            let symbol = types.symbol(name);
            types.intern(ir::Type::Class(symbol));
        }
        Arc::new(types)
    })
}

fn common_id(ty: &oomir::Type) -> Option<ir::TypeId> {
    use oomir::Type::*;
    Some(ir::TypeId::new(match ty {
        Void | Unit => 0,
        Boolean => 1,
        I8 => 2,
        U8 => 3,
        I16 => 4,
        Char | U16 => 5,
        I32 => 6,
        U32 => 7,
        I64 => 8,
        U64 => 9,
        F16 => 10,
        F32 => 11,
        F64 => 12,
        Class(name) => match name.as_str() {
            "java/lang/Object" => 13,
            "java/lang/String" => 14,
            "java/lang/Throwable" => 15,
            _ => return None,
        },
        _ => return None,
    }))
}

impl Default for Vocabulary {
    fn default() -> Self {
        Self {
            types: ir::Types::with_base(Arc::clone(common_types())),
            ids: HashMap::default(),
            descriptors: Vec::new(),
        }
    }
}
impl Vocabulary {
    pub fn add(&mut self, ty: &oomir::Type) -> ir::TypeId {
        if let Some(id) = common_id(ty) {
            return id;
        }
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
        if id.index() == COMMON_DESCRIPTORS.len() + self.descriptors.len() {
            self.descriptors.push(ty.to_jvm_descriptor());
        }
        self.ids.insert(ty.clone(), id);
        if let T::Interface(name) = ty {
            self.ids.entry(T::Class(name.clone())).or_insert(id);
        }
        id
    }
    pub fn id(&self, ty: &oomir::Type) -> ir::TypeId {
        if let Some(id) = common_id(ty) {
            return id;
        }
        *self
            .ids
            .get(ty)
            .unwrap_or_else(|| panic!("unregistered emission type {ty:?}"))
    }
    pub fn same_carrier(&self, a: ir::TypeId, b: ir::TypeId) -> bool {
        let descriptor = |id: ir::TypeId| {
            if id.index() < COMMON_DESCRIPTORS.len() {
                COMMON_DESCRIPTORS[id.index()]
            } else {
                self.descriptors[id.index() - COMMON_DESCRIPTORS.len()].as_str()
            }
        };
        descriptor(a) == descriptor(b)
    }
    pub fn into_types(self) -> Arc<ir::Types> {
        if self.types.has_additions() {
            Arc::new(self.types)
        } else {
            Arc::clone(common_types())
        }
    }
    pub fn add_wrappers(&mut self, context: &Context) {
        let mut pending = self.ids.keys().cloned().collect::<Vec<_>>();
        for name in [
            "java/lang/Object",
            "java/lang/String",
            "java/lang/Throwable",
        ] {
            if context.wrappers.contains_key(name) {
                pending.push(oomir::Type::Class(name.into()));
            }
        }
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
        instruction.visit_operands(|operand| match operand {
            oomir::Operand::Variable { ty, .. } => {
                self.add(ty);
            }
            oomir::Operand::Constant(constant) => {
                self.add(&oomir::Type::from_constant(constant));
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_types_preserve_carriers_and_keep_body_symbols_separate() {
        use oomir::Type::*;
        let types = [
            Void,
            Unit,
            Boolean,
            Char,
            I8,
            U8,
            I16,
            U16,
            I32,
            U32,
            I64,
            U64,
            F16,
            F32,
            F64,
            object(),
            Class("java/lang/String".into()),
            Class("java/lang/Throwable".into()),
            Class("A".into()),
            Interface("I".into()),
            Pointer(Box::new(I32)),
            Pointer(Box::new(I64)),
            Array(Box::new(F16)),
            Array(Box::new(I16)),
            Reference(Box::new(U8)),
        ];
        let mut vocabulary = Vocabulary::default();
        for ty in &types {
            vocabulary.add(ty);
        }
        for a in &types {
            for b in &types {
                assert_eq!(
                    vocabulary.same_carrier(vocabulary.id(a), vocabulary.id(b)),
                    a.same_jvm_type(b),
                    "{a:?}, {b:?}"
                );
            }
        }
        let mut other = Vocabulary::default();
        let b = other.add(&Class("B".into()));
        assert_eq!(source_type(&other.types, b), Class("B".into()));
        assert_eq!(
            source_type(&vocabulary.types, vocabulary.id(&Class("A".into()))),
            Class("A".into())
        );
    }
}
