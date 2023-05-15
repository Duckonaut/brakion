use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum TypeRepresentation {
    // Built-in types
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Void,

    // User-defined types

    // use `String` as namespace instead of `Vec<String>` for runtime-relevant stuff
    // since it's a single allocation and therefore a flat memory layout
    Type {
        namespace: String,
        fields: HashMap<String, TypeRepresentation>,
    },
    TraitObject {
        namespace: String,
        underlying_type: Box<TypeRepresentation>,
    },
}

impl std::hash::Hash for TypeRepresentation {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            TypeRepresentation::U8(v) => v.hash(state),
            TypeRepresentation::U16(v) => v.hash(state),
            TypeRepresentation::U32(v) => v.hash(state),
            TypeRepresentation::U64(v) => v.hash(state),
            TypeRepresentation::I8(v) => v.hash(state),
            TypeRepresentation::I16(v) => v.hash(state),
            TypeRepresentation::I32(v) => v.hash(state),
            TypeRepresentation::I64(v) => v.hash(state),
            TypeRepresentation::F32(v) => v.to_bits().hash(state),
            TypeRepresentation::F64(v) => v.to_bits().hash(state),
            TypeRepresentation::Bool(v) => v.hash(state),
            TypeRepresentation::Char(v) => v.hash(state),
            TypeRepresentation::Str(v) => v.hash(state),
            TypeRepresentation::Void => {}
            TypeRepresentation::Type { namespace, fields } => {
                namespace.hash(state);
                for (name, ty) in fields {
                    name.hash(state);
                    ty.hash(state);
                }
            }
            TypeRepresentation::TraitObject { namespace, underlying_type } => {
                namespace.hash(state);
                underlying_type.hash(state);
            }
        }
    }
}
