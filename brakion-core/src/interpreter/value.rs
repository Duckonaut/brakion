use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::repr::{
    look_up_decl, type_implements_trait, Decl, FloatSize, Function, IntSize, NamespaceReference,
    TypeBody, TypeReference, TypeReferenceKind, TypeVariant,
};

macro_rules! int_cast {
    ($i:ident, $ty:ident) => {
        match $ty {
            TypeReferenceKind::Integer(IntSize::I8, true) => Some(Self::I8(*$i as i8)),
            TypeReferenceKind::Integer(IntSize::I16, true) => Some(Self::I16(*$i as i16)),
            TypeReferenceKind::Integer(IntSize::I32, true) => Some(Self::I32(*$i as i32)),
            TypeReferenceKind::Integer(IntSize::I64, true) => Some(Self::I64(*$i as i64)),
            TypeReferenceKind::Integer(IntSize::I8, false) => Some(Self::U8(*$i as u8)),
            TypeReferenceKind::Integer(IntSize::I16, false) => Some(Self::U16(*$i as u16)),
            TypeReferenceKind::Integer(IntSize::I32, false) => Some(Self::U32(*$i as u32)),
            TypeReferenceKind::Integer(IntSize::I64, false) => Some(Self::U64(*$i as u64)),
            TypeReferenceKind::Float(FloatSize::F32) => Some(Self::F32(*$i as f32)),
            TypeReferenceKind::Float(FloatSize::F64) => Some(Self::F64(*$i as f64)),
            _ => None,
        }
    };
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    String(String),
    Char(char),
    Void,
    Instance(Instance<'a>),
    List(ListInstance<'a>),
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // signed
            (Self::I8(l0), Self::I64(r0)) => *l0 as i64 == *r0,
            (Self::I8(l0), Self::I32(r0)) => *l0 as i32 == *r0,
            (Self::I8(l0), Self::I16(r0)) => *l0 as i16 == *r0,
            (Self::I8(l0), Self::I8(r0)) => l0 == r0,
            (Self::I16(l0), Self::I64(r0)) => *l0 as i64 == *r0,
            (Self::I16(l0), Self::I32(r0)) => *l0 as i32 == *r0,
            (Self::I16(l0), Self::I16(r0)) => l0 == r0,
            (Self::I16(l0), Self::I8(r0)) => *l0 == *r0 as i16,
            (Self::I32(l0), Self::I64(r0)) => *l0 as i64 == *r0,
            (Self::I32(l0), Self::I32(r0)) => l0 == r0,
            (Self::I32(l0), Self::I16(r0)) => *l0 == *r0 as i32,
            (Self::I32(l0), Self::I8(r0)) => *l0 == *r0 as i32,
            (Self::I64(l0), Self::I64(r0)) => l0 == r0,
            (Self::I64(l0), Self::I32(r0)) => *l0 == *r0 as i64,
            (Self::I64(l0), Self::I16(r0)) => *l0 == *r0 as i64,
            (Self::I64(l0), Self::I8(r0)) => *l0 == *r0 as i64,
            // unsigned
            (Self::U8(l0), Self::U8(r0)) => l0 == r0,
            (Self::U8(l0), Self::U16(r0)) => *l0 as u16 == *r0,
            (Self::U8(l0), Self::U32(r0)) => *l0 as u32 == *r0,
            (Self::U8(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::U16(l0), Self::U8(r0)) => *l0 == *r0 as u16,
            (Self::U16(l0), Self::U16(r0)) => l0 == r0,
            (Self::U16(l0), Self::U32(r0)) => *l0 as u32 == *r0,
            (Self::U16(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::U32(l0), Self::U8(r0)) => *l0 == *r0 as u32,
            (Self::U32(l0), Self::U16(r0)) => *l0 == *r0 as u32,
            (Self::U32(l0), Self::U32(r0)) => l0 == r0,
            (Self::U32(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::U64(l0), Self::U8(r0)) => *l0 == *r0 as u64,
            (Self::U64(l0), Self::U16(r0)) => *l0 == *r0 as u64,
            (Self::U64(l0), Self::U32(r0)) => *l0 == *r0 as u64,
            (Self::U64(l0), Self::U64(r0)) => l0 == r0,
            // mixed
            (Self::I8(l0), Self::U8(r0)) => *l0 as u8 == *r0,
            (Self::I8(l0), Self::U16(r0)) => *l0 as u16 == *r0,
            (Self::I8(l0), Self::U32(r0)) => *l0 as u32 == *r0,
            (Self::I8(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::I16(l0), Self::U8(r0)) => *l0 as u8 == *r0,
            (Self::I16(l0), Self::U16(r0)) => *l0 as u16 == *r0,
            (Self::I16(l0), Self::U32(r0)) => *l0 as u32 == *r0,
            (Self::I16(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::I32(l0), Self::U8(r0)) => *l0 as u8 == *r0,
            (Self::I32(l0), Self::U16(r0)) => *l0 as u16 == *r0,
            (Self::I32(l0), Self::U32(r0)) => *l0 as u32 == *r0,
            (Self::I32(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::I64(l0), Self::U8(r0)) => *l0 as u8 == *r0,
            (Self::I64(l0), Self::U16(r0)) => *l0 as u16 == *r0,
            (Self::I64(l0), Self::U32(r0)) => *l0 as u32 == *r0,
            (Self::I64(l0), Self::U64(r0)) => *l0 as u64 == *r0,
            (Self::U8(l0), Self::I8(r0)) => *l0 == *r0 as u8,
            (Self::U8(l0), Self::I16(r0)) => *l0 == *r0 as u8,
            (Self::U8(l0), Self::I32(r0)) => *l0 == *r0 as u8,
            (Self::U8(l0), Self::I64(r0)) => *l0 == *r0 as u8,
            (Self::U16(l0), Self::I8(r0)) => *l0 == *r0 as u16,
            (Self::U16(l0), Self::I16(r0)) => *l0 == *r0 as u16,
            (Self::U16(l0), Self::I32(r0)) => *l0 == *r0 as u16,
            (Self::U16(l0), Self::I64(r0)) => *l0 == *r0 as u16,
            (Self::U32(l0), Self::I8(r0)) => *l0 == *r0 as u32,
            (Self::U32(l0), Self::I16(r0)) => *l0 == *r0 as u32,
            (Self::U32(l0), Self::I32(r0)) => *l0 == *r0 as u32,
            (Self::U32(l0), Self::I64(r0)) => *l0 == *r0 as u32,
            (Self::U64(l0), Self::I8(r0)) => *l0 == *r0 as u64,
            (Self::U64(l0), Self::I16(r0)) => *l0 == *r0 as u64,
            (Self::U64(l0), Self::I32(r0)) => *l0 == *r0 as u64,
            (Self::U64(l0), Self::I64(r0)) => *l0 == *r0 as u64,
            // float
            (Self::F32(l0), Self::F32(r0)) => l0 == r0,
            (Self::F64(l0), Self::F64(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Instance(l0), Self::Instance(r0)) => {
                Rc::ptr_eq(&l0.0, &r0.0) || {
                    l0.get_type_body() == r0.get_type_body()
                        && l0.get_type_variant() == r0.get_type_variant()
                        && l0
                            .get_type_variant()
                            .fields
                            .iter()
                            .all(|f| l0.get_field(&f.name.name) == r0.get_field(&f.name.name))
                }
            }
            (Self::List(l0), Self::List(r0)) => Rc::ptr_eq(&l0.0, &r0.0),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance<'a>(Rc<RefCell<InstanceInternal<'a>>>);

impl<'a> Instance<'a> {
    pub(crate) fn new(
        type_reference: TypeReferenceKind,
        type_body: &'a TypeBody,
        type_variant: &'a TypeVariant,
    ) -> Self {
        Self(Rc::new(RefCell::new(InstanceInternal {
            type_reference,
            type_body,
            type_variant,
            fields: HashMap::new(),
        })))
    }

    pub(crate) fn get_field(&self, name: &str) -> Option<Value<'a>> {
        self.0.borrow().fields.get(name).cloned()
    }

    pub(crate) fn set_field(&self, name: &str, value: Value<'a>) {
        self.0.borrow_mut().fields.insert(name.to_owned(), value);
    }

    pub(crate) fn get_type_reference(&self) -> TypeReferenceKind {
        self.0.borrow().type_reference.clone()
    }

    pub(crate) fn get_type_body(&self) -> &'a TypeBody {
        self.0.borrow().type_body
    }

    pub(crate) fn get_type_variant(&self) -> &'a TypeVariant {
        self.0.borrow().type_variant
    }

    pub(crate) fn lookup_method(&self, name: &str) -> Option<&'a Function> {
        self.get_type_body()
            .methods
            .iter()
            .find(|m| m.1.signature.name.name == name)
            .map(|m| &m.1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListInstance<'a>(Rc<RefCell<ListInstanceInternal<'a>>>);

impl<'a> ListInstance<'a> {
    pub(crate) fn new(ty: TypeReferenceKind) -> Self {
        Self(Rc::new(RefCell::new(ListInstanceInternal {
            type_reference: ty,
            elements: Vec::new(),
        })))
    }

    pub(crate) fn wrap(ty: TypeReferenceKind, vec: Vec<Value<'a>>) -> ListInstance<'a> {
        Self(Rc::new(RefCell::new(ListInstanceInternal {
            type_reference: ty,
            elements: vec,
        })))
    }
    pub(crate) fn get_elements(&self) -> Vec<Value<'a>> {
        self.0.borrow().elements.clone()
    }

    pub(crate) fn push(&self, value: Value<'a>) {
        if let TypeReferenceKind::Infer = self.get_type_reference() {
            self.0.borrow_mut().type_reference = value.get_type_reference();
        }
        self.0.borrow_mut().elements.push(value);
    }

    pub(crate) fn pop(&self) -> Option<Value<'a>> {
        self.0.borrow_mut().elements.pop()
    }

    pub(crate) fn get(&self, index: usize) -> Option<Value<'a>> {
        self.0.borrow().elements.get(index).cloned()
    }

    pub(crate) fn set(&self, index: usize, value: Value<'a>) {
        if let TypeReferenceKind::Infer = self.get_type_reference() {
            self.0.borrow_mut().type_reference = value.get_type_reference();
        }
        self.0.borrow_mut().elements[index] = value;
    }

    pub(crate) fn len(&self) -> usize {
        self.0.borrow().elements.len()
    }

    pub(crate) fn get_type_reference(&self) -> TypeReferenceKind {
        self.0.borrow().type_reference.clone()
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ListInstanceInternal<'a> {
    pub type_reference: TypeReferenceKind,
    pub elements: Vec<Value<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceInternal<'a> {
    pub type_reference: TypeReferenceKind,
    pub type_body: &'a TypeBody,
    pub type_variant: &'a TypeVariant,
    pub fields: HashMap<String, Value<'a>>,
}

impl<'a> Value<'a> {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::I8(_)
                | Self::I16(_)
                | Self::I32(_)
                | Self::I64(_)
                | Self::U8(_)
                | Self::U16(_)
                | Self::U32(_)
                | Self::U64(_)
                | Self::F32(_)
                | Self::F64(_)
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::I8(_) | Self::I16(_) | Self::I32(_) | Self::I64(_)
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::U8(_) | Self::U16(_) | Self::U32(_) | Self::U64(_)
        )
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            Self::I8(_)
                | Self::I16(_)
                | Self::I32(_)
                | Self::I64(_)
                | Self::U8(_)
                | Self::U16(_)
                | Self::U32(_)
                | Self::U64(_)
        )
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Self::I8(0) => true,
            Self::I16(0) => true,
            Self::I32(0) => true,
            Self::I64(0) => true,
            Self::U8(0) => true,
            Self::U16(0) => true,
            Self::U32(0) => true,
            Self::U64(0) => true,
            Self::F32(f) => *f == 0.0,
            Self::F64(f) => *f == 0.0,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::F32(_) | Self::F64(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(_))
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn is_instance(&self) -> bool {
        matches!(self, Self::Instance(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(_))
    }

    pub fn get_int_as_usize(&self) -> Option<usize> {
        match self {
            Self::I8(i) => Some(*i as usize),
            Self::I16(i) => Some(*i as usize),
            Self::I32(i) => Some(*i as usize),
            Self::I64(i) => Some(*i as usize),
            Self::U8(i) => Some(*i as usize),
            Self::U16(i) => Some(*i as usize),
            Self::U32(i) => Some(*i as usize),
            Self::U64(i) => Some(*i as usize),
            _ => None,
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Self::I8(i) => *i != 0,
            Self::I16(i) => *i != 0,
            Self::I32(i) => *i != 0,
            Self::I64(i) => *i != 0,
            Self::U8(i) => *i != 0,
            Self::U16(i) => *i != 0,
            Self::U32(i) => *i != 0,
            Self::U64(i) => *i != 0,
            Self::F32(f) => *f != 0.0,
            Self::F64(f) => *f != 0.0,
            Self::Bool(b) => *b,
            Self::String(s) => !s.is_empty(),
            Self::Char(_) => true,
            Self::Void => false,
            Self::Instance(_) => true,
            Self::List(l) => !l.get_elements().is_empty(),
        }
    }

    pub fn is_type(&self, ty: &TypeReferenceKind, root: &[Decl]) -> bool {
        match self {
            Value::I8(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I8, true)),
            Value::I16(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I16, true)),
            Value::I32(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I32, true)),
            Value::I64(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I64, true)),
            Value::U8(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I8, false)),
            Value::U16(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I16, false)),
            Value::U32(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I32, false)),
            Value::U64(_) => matches!(ty, TypeReferenceKind::Integer(IntSize::I64, false)),
            Value::F32(_) => matches!(ty, TypeReferenceKind::Float(FloatSize::F32)),
            Value::F64(_) => matches!(ty, TypeReferenceKind::Float(FloatSize::F64)),
            Value::Bool(_) => matches!(ty, TypeReferenceKind::Bool),
            Value::String(_) => matches!(ty, TypeReferenceKind::String),
            Value::Char(_) => matches!(ty, TypeReferenceKind::Char),
            Value::Void => matches!(ty, TypeReferenceKind::Void),
            Value::Instance(i) => {
                let instance_type_name = if let TypeReferenceKind::Named(n) = i.get_type_reference()
                {
                    n
                } else {
                    panic!("Instance type reference is not a named type"); // we only create instances of named types
                };

                if let TypeReferenceKind::Named(name) = ty {
                    let decl = look_up_decl(root, name).expect("Type not found"); // Validator should have checked this, so this should never fail

                    match decl {
                        NamespaceReference::Decl(d) => match d {
                            Decl::Type { .. } => {
                                instance_type_name.same(name) || instance_type_name.up().same(name)
                            }
                            Decl::Trait { .. } => {
                                type_implements_trait(root, &instance_type_name, name).unwrap()
                                // Validator should have checked this, so this should never fail
                            }
                            _ => false,
                        },
                        NamespaceReference::TypeVariant(variant) => {
                            // logically, a level up from the type variant should be a type
                            // so we can just check if the type name matches, and then if the variant name matches
                            let decl = look_up_decl(root, &name.up()).expect("Type not found");

                            if let NamespaceReference::Decl(Decl::Type { body, .. }) = decl {
                                let variant = body
                                    .variants
                                    .iter()
                                    .find(|v| *v == variant)
                                    .expect("Variant not found"); // Validator should have checked this, so this should never fail

                                i.get_type_variant() == variant
                            } else {
                                false
                            }
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            }
            Value::List(_) => todo!(),
        }
    }

    pub fn get_type_reference(&self) -> TypeReferenceKind {
        match self {
            Self::I8(_) => TypeReferenceKind::Integer(IntSize::I8, true),
            Self::I16(_) => TypeReferenceKind::Integer(IntSize::I16, true),
            Self::I32(_) => TypeReferenceKind::Integer(IntSize::I32, true),
            Self::I64(_) => TypeReferenceKind::Integer(IntSize::I64, true),
            Self::U8(_) => TypeReferenceKind::Integer(IntSize::I8, false),
            Self::U16(_) => TypeReferenceKind::Integer(IntSize::I16, false),
            Self::U32(_) => TypeReferenceKind::Integer(IntSize::I32, false),
            Self::U64(_) => TypeReferenceKind::Integer(IntSize::I64, false),
            Self::F32(_) => TypeReferenceKind::Float(FloatSize::F32),
            Self::F64(_) => TypeReferenceKind::Float(FloatSize::F64),
            Self::Bool(_) => TypeReferenceKind::Bool,
            Self::String(_) => TypeReferenceKind::String,
            Self::Char(_) => TypeReferenceKind::Char,
            Self::Void => TypeReferenceKind::Void,
            Self::Instance(i) => i.get_type_reference(),
            Self::List(l) => TypeReferenceKind::List(Box::new(TypeReference {
                span: None,
                kind: l.get_type_reference(),
            })),
        }
    }

    pub fn cast(&self, ty: &TypeReferenceKind, root: &[Decl]) -> Option<Self> {
        if let TypeReferenceKind::Union(tys) = ty {
            for ty in tys {
                if let Some(v) = self.cast(&ty.kind, root) {
                    return Some(v);
                }
            }

            return None;
        }

        match self {
            Self::I8(i) => int_cast!(i, ty),
            Self::I16(i) => int_cast!(i, ty),
            Self::I32(i) => int_cast!(i, ty),
            Self::I64(i) => int_cast!(i, ty),
            Self::U8(i) => int_cast!(i, ty),
            Self::U16(i) => int_cast!(i, ty),
            Self::U32(i) => int_cast!(i, ty),
            Self::U64(i) => int_cast!(i, ty),
            Self::F32(f) => int_cast!(f, ty),
            Self::F64(f) => int_cast!(f, ty),
            Self::Bool(b) => match ty {
                TypeReferenceKind::Bool => Some(Self::Bool(*b)),
                TypeReferenceKind::Integer(IntSize::I8, true) => Some(Self::I8(*b as i8)),
                TypeReferenceKind::Integer(IntSize::I16, true) => Some(Self::I16(*b as i16)),
                TypeReferenceKind::Integer(IntSize::I32, true) => Some(Self::I32(*b as i32)),
                TypeReferenceKind::Integer(IntSize::I64, true) => Some(Self::I64(*b as i64)),
                TypeReferenceKind::Integer(IntSize::I8, false) => Some(Self::U8(*b as u8)),
                TypeReferenceKind::Integer(IntSize::I16, false) => Some(Self::U16(*b as u16)),
                TypeReferenceKind::Integer(IntSize::I32, false) => Some(Self::U32(*b as u32)),
                TypeReferenceKind::Integer(IntSize::I64, false) => Some(Self::U64(*b as u64)),
                TypeReferenceKind::Float(FloatSize::F32) => Some(Self::F32(*b as u8 as f32)),
                TypeReferenceKind::Float(FloatSize::F64) => Some(Self::F64(*b as u8 as f64)),
                _ => None,
            },
            Self::String(s) => match ty {
                TypeReferenceKind::String => Some(Self::String(s.clone())),
                TypeReferenceKind::Char => {
                    if s.len() == 1 {
                        Some(Self::Char(s.chars().next().unwrap()))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Self::Char(c) => match ty {
                TypeReferenceKind::Char => Some(Self::Char(*c)),
                TypeReferenceKind::String => Some(Self::String(c.to_string())),
                _ => None,
            },
            Self::Void => match ty {
                TypeReferenceKind::Void => Some(Self::Void),
                _ => None,
            },
            Self::Instance(instance) => {
                if ty.same(&instance.get_type_reference())
                    || ty
                        .is_compatible(&instance.get_type_reference(), root)
                        .unwrap()
                {
                    Some(Self::Instance(instance.clone()))
                } else {
                    None
                }
            }
            Self::List(l) => {
                if let TypeReferenceKind::List(ty) = ty {
                    let new_list = ListInstance::new(ty.kind.clone());

                    for v in l.get_elements().iter() {
                        if let Some(v) = v.cast(&ty.kind, root) {
                            new_list.push(v);
                        } else {
                            return None;
                        }
                    }

                    Some(Self::List(new_list))
                } else {
                    None
                }
            }
        }
    }

    pub fn type_to_string(&self) -> String {
        match self {
            Self::I8(_) => "i8".to_string(),
            Self::I16(_) => "i16".to_string(),
            Self::I32(_) => "i32".to_string(),
            Self::I64(_) => "i64".to_string(),
            Self::U8(_) => "u8".to_string(),
            Self::U16(_) => "u16".to_string(),
            Self::U32(_) => "u32".to_string(),
            Self::U64(_) => "u64".to_string(),
            Self::F32(_) => "f32".to_string(),
            Self::F64(_) => "f64".to_string(),
            Self::Bool(_) => "bool".to_string(),
            Self::String(_) => "string".to_string(),
            Self::Char(_) => "char".to_string(),
            Self::Void => "void".to_string(),
            Self::Instance(i) => i.get_type_reference().to_string(),
            Self::List(l) => format!("[{}]", l.get_type_reference()),
        }
    }
}

impl<'a> std::ops::Neg for Value<'a> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::I8(i) => Self::I8(-i),
            Self::I16(i) => Self::I16(-i),
            Self::I32(i) => Self::I32(-i),
            Self::I64(i) => Self::I64(-i),
            Self::U8(i) => Self::U8(-(i as i8) as u8),
            Self::U16(i) => Self::U16(-(i as i16) as u16),
            Self::U32(i) => Self::U32(-(i as i32) as u32),
            Self::U64(i) => Self::U64(-(i as i64) as u64),
            Self::F32(f) => Self::F32(-f),
            Self::F64(f) => Self::F64(-f),
            _ => panic!("Cannot negate value"),
        }
    }
}

impl<'a> std::ops::Not for Value<'a> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Self::Bool(!b),
            _ => panic!("Cannot negate value"),
        }
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I8(i) => write!(f, "{}", i),
            Value::I16(i) => write!(f, "{}", i),
            Value::I32(i) => write!(f, "{}", i),
            Value::I64(i) => write!(f, "{}", i),
            Value::U8(u) => write!(f, "{}", u),
            Value::U16(u) => write!(f, "{}", u),
            Value::U32(u) => write!(f, "{}", u),
            Value::U64(u) => write!(f, "{}", u),
            Value::F32(n) => write!(f, "{}", n),
            Value::F64(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Void => write!(f, "void"),
            Value::Instance(_) => write!(f, "<instance>"),
            Value::List(_) => write!(f, "<list>"),
        }
    }
}

impl<'a> std::ops::Add for Value<'a> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::I8(l), Self::I8(r)) => Self::I8(l.overflowing_add(r).0),
            (Self::I16(l), Self::I16(r)) => Self::I16(l.overflowing_add(r).0),
            (Self::I16(l), Self::I8(r)) => Self::I16(l.overflowing_add(r as i16).0),
            (Self::I32(l), Self::I32(r)) => Self::I32(l.overflowing_add(r).0),
            (Self::I32(l), Self::I16(r)) => Self::I32(l.overflowing_add(r as i32).0),
            (Self::I32(l), Self::I8(r)) => Self::I32(l.overflowing_add(r as i32).0),
            (Self::I64(l), Self::I64(r)) => Self::I64(l.overflowing_add(r).0),
            (Self::I64(l), Self::I32(r)) => Self::I64(l.overflowing_add(r as i64).0),
            (Self::I64(l), Self::I16(r)) => Self::I64(l.overflowing_add(r as i64).0),
            (Self::I64(l), Self::I8(r)) => Self::I64(l.overflowing_add(r as i64).0),
            (Self::U8(l), Self::U8(r)) => Self::U8(l.overflowing_add(r).0),
            (Self::U16(l), Self::U16(r)) => Self::U16(l.overflowing_add(r).0),
            (Self::U16(l), Self::U8(r)) => Self::U16(l.overflowing_add(r as u16).0),
            (Self::U32(l), Self::U32(r)) => Self::U32(l.overflowing_add(r).0),
            (Self::U32(l), Self::U16(r)) => Self::U32(l.overflowing_add(r as u32).0),
            (Self::U32(l), Self::U8(r)) => Self::U32(l.overflowing_add(r as u32).0),
            (Self::U64(l), Self::U64(r)) => Self::U64(l.overflowing_add(r).0),
            (Self::U64(l), Self::U32(r)) => Self::U64(l.overflowing_add(r as u64).0),
            (Self::U64(l), Self::U16(r)) => Self::U64(l.overflowing_add(r as u64).0),
            (Self::U64(l), Self::U8(r)) => Self::U64(l.overflowing_add(r as u64).0),
            (Self::F32(l), Self::F32(r)) => Self::F32(l + r),
            (Self::F32(l), Self::F64(r)) => Self::F32(l + r as f32),
            (Self::F64(l), Self::F64(r)) => Self::F64(l + r),
            (Self::F64(l), Self::F32(r)) => Self::F64(l + r as f64),
            (Self::String(l), Self::String(r)) => Self::String(l + &r),
            (Self::String(s), Self::Char(c)) => Self::String(s + &c.to_string()),
            _ => panic!("Cannot add values"),
        }
    }
}

impl<'a> std::ops::Sub for Value<'a> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::I8(l), Self::I8(r)) => Self::I8(l - r),
            (Self::I16(l), Self::I16(r)) => Self::I16(l - r),
            (Self::I16(l), Self::I8(r)) => Self::I16(l - r as i16),
            (Self::I32(l), Self::I32(r)) => Self::I32(l - r),
            (Self::I32(l), Self::I16(r)) => Self::I32(l - r as i32),
            (Self::I32(l), Self::I8(r)) => Self::I32(l - r as i32),
            (Self::I64(l), Self::I64(r)) => Self::I64(l - r),
            (Self::I64(l), Self::I32(r)) => Self::I64(l - r as i64),
            (Self::I64(l), Self::I16(r)) => Self::I64(l - r as i64),
            (Self::I64(l), Self::I8(r)) => Self::I64(l - r as i64),
            (Self::U8(l), Self::U8(r)) => Self::U8(l - r),
            (Self::U16(l), Self::U16(r)) => Self::U16(l - r),
            (Self::U16(l), Self::U8(r)) => Self::U16(l - r as u16),
            (Self::U32(l), Self::U32(r)) => Self::U32(l - r),
            (Self::U32(l), Self::U16(r)) => Self::U32(l - r as u32),
            (Self::U32(l), Self::U8(r)) => Self::U32(l - r as u32),
            (Self::U64(l), Self::U64(r)) => Self::U64(l - r),
            (Self::U64(l), Self::U32(r)) => Self::U64(l - r as u64),
            (Self::U64(l), Self::U16(r)) => Self::U64(l - r as u64),
            (Self::U64(l), Self::U8(r)) => Self::U64(l - r as u64),
            (Self::F32(l), Self::F32(r)) => Self::F32(l - r),
            (Self::F32(l), Self::F64(r)) => Self::F32(l - r as f32),
            (Self::F64(l), Self::F64(r)) => Self::F64(l - r),
            (Self::F64(l), Self::F32(r)) => Self::F64(l - r as f64),
            (l, r) => panic!("Cannot subtract values {} and {}", dbg!(l), dbg!(r)),
        }
    }
}

impl<'a> std::ops::Mul for Value<'a> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::I8(l), Self::I8(r)) => Self::I8(l * r),
            (Self::I16(l), Self::I16(r)) => Self::I16(l * r),
            (Self::I16(l), Self::I8(r)) => Self::I16(l * r as i16),
            (Self::I32(l), Self::I32(r)) => Self::I32(l * r),
            (Self::I32(l), Self::I16(r)) => Self::I32(l * r as i32),
            (Self::I32(l), Self::I8(r)) => Self::I32(l * r as i32),
            (Self::I64(l), Self::I64(r)) => Self::I64(l * r),
            (Self::I64(l), Self::I32(r)) => Self::I64(l * r as i64),
            (Self::I64(l), Self::I16(r)) => Self::I64(l * r as i64),
            (Self::I64(l), Self::I8(r)) => Self::I64(l * r as i64),
            (Self::U8(l), Self::U8(r)) => Self::U8(l * r),
            (Self::U16(l), Self::U16(r)) => Self::U16(l * r),
            (Self::U16(l), Self::U8(r)) => Self::U16(l * r as u16),
            (Self::U32(l), Self::U32(r)) => Self::U32(l * r),
            (Self::U32(l), Self::U16(r)) => Self::U32(l * r as u32),
            (Self::U32(l), Self::U8(r)) => Self::U32(l * r as u32),
            (Self::U64(l), Self::U64(r)) => Self::U64(l * r),
            (Self::U64(l), Self::U32(r)) => Self::U64(l * r as u64),
            (Self::U64(l), Self::U16(r)) => Self::U64(l * r as u64),
            (Self::U64(l), Self::U8(r)) => Self::U64(l * r as u64),
            (Self::F32(l), Self::F32(r)) => Self::F32(l * r),
            (Self::F32(l), Self::F64(r)) => Self::F32(l * r as f32),
            (Self::F64(l), Self::F64(r)) => Self::F64(l * r),
            (Self::F64(l), Self::F32(r)) => Self::F64(l * r as f64),
            (l, r) => panic!("Cannot multiply values {:?}, {:?}", l, r),
        }
    }
}

impl<'a> std::ops::Div for Value<'a> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::I8(l), Self::I8(r)) => Self::I8(l / r),
            (Self::I16(l), Self::I16(r)) => Self::I16(l / r),
            (Self::I16(l), Self::I8(r)) => Self::I16(l / r as i16),
            (Self::I32(l), Self::I32(r)) => Self::I32(l / r),
            (Self::I32(l), Self::I16(r)) => Self::I32(l / r as i32),
            (Self::I32(l), Self::I8(r)) => Self::I32(l / r as i32),
            (Self::I64(l), Self::I64(r)) => Self::I64(l / r),
            (Self::I64(l), Self::I32(r)) => Self::I64(l / r as i64),
            (Self::I64(l), Self::I16(r)) => Self::I64(l / r as i64),
            (Self::I64(l), Self::I8(r)) => Self::I64(l / r as i64),
            (Self::U8(l), Self::U8(r)) => Self::U8(l / r),
            (Self::U16(l), Self::U16(r)) => Self::U16(l / r),
            (Self::U16(l), Self::U8(r)) => Self::U16(l / r as u16),
            (Self::U32(l), Self::U32(r)) => Self::U32(l / r),
            (Self::U32(l), Self::U16(r)) => Self::U32(l / r as u32),
            (Self::U32(l), Self::U8(r)) => Self::U32(l / r as u32),
            (Self::U64(l), Self::U64(r)) => Self::U64(l / r),
            (Self::U64(l), Self::U32(r)) => Self::U64(l / r as u64),
            (Self::U64(l), Self::U16(r)) => Self::U64(l / r as u64),
            (Self::U64(l), Self::U8(r)) => Self::U64(l / r as u64),
            (Self::F32(l), Self::F32(r)) => Self::F32(l / r),
            (Self::F32(l), Self::F64(r)) => Self::F32(l / r as f32),
            (Self::F64(l), Self::F64(r)) => Self::F64(l / r),
            (Self::F64(l), Self::F32(r)) => Self::F64(l / r as f64),
            _ => panic!("Cannot divide values"),
        }
    }
}

impl<'a> std::cmp::PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::I8(l), Self::I8(r)) => l.partial_cmp(r),
            (Self::I16(l), Self::I16(r)) => l.partial_cmp(r),
            (Self::I16(l), Self::I8(r)) => l.partial_cmp(&(*r as i16)),
            (Self::I32(l), Self::I32(r)) => l.partial_cmp(r),
            (Self::I32(l), Self::I16(r)) => l.partial_cmp(&(*r as i32)),
            (Self::I32(l), Self::I8(r)) => l.partial_cmp(&(*r as i32)),
            (Self::I64(l), Self::I64(r)) => l.partial_cmp(r),
            (Self::I64(l), Self::I32(r)) => l.partial_cmp(&(*r as i64)),
            (Self::I64(l), Self::I16(r)) => l.partial_cmp(&(*r as i64)),
            (Self::I64(l), Self::I8(r)) => l.partial_cmp(&(*r as i64)),
            (Self::U8(l), Self::U8(r)) => l.partial_cmp(r),
            (Self::U16(l), Self::U16(r)) => l.partial_cmp(r),
            (Self::U16(l), Self::U8(r)) => l.partial_cmp(&(*r as u16)),
            (Self::U32(l), Self::U32(r)) => l.partial_cmp(r),
            (Self::U32(l), Self::U16(r)) => l.partial_cmp(&(*r as u32)),
            (Self::U32(l), Self::U8(r)) => l.partial_cmp(&(*r as u32)),
            (Self::U64(l), Self::U64(r)) => l.partial_cmp(r),
            (Self::U64(l), Self::U32(r)) => l.partial_cmp(&(*r as u64)),
            (Self::U64(l), Self::U16(r)) => l.partial_cmp(&(*r as u64)),
            (Self::U64(l), Self::U8(r)) => l.partial_cmp(&(*r as u64)),
            (Self::F32(l), Self::F32(r)) => l.partial_cmp(r),
            (Self::F32(l), Self::F64(r)) => l.partial_cmp(&(*r as f32)),
            (Self::F64(l), Self::F64(r)) => l.partial_cmp(r),
            (Self::F64(l), Self::F32(r)) => l.partial_cmp(&(*r as f64)),
            // compare signed and unsigned too
            (Self::I8(l), Self::U64(r)) => l.partial_cmp(&(*r as i8)),
            (Self::I8(l), Self::U32(r)) => l.partial_cmp(&(*r as i8)),
            (Self::I8(l), Self::U16(r)) => l.partial_cmp(&(*r as i8)),
            (Self::I8(l), Self::U8(r)) => l.partial_cmp(&(*r as i8)),
            (Self::I16(l), Self::U64(r)) => l.partial_cmp(&(*r as i16)),
            (Self::I16(l), Self::U32(r)) => l.partial_cmp(&(*r as i16)),
            (Self::I16(l), Self::U16(r)) => l.partial_cmp(&(*r as i16)),
            (Self::I16(l), Self::U8(r)) => l.partial_cmp(&(*r as i16)),
            (Self::I32(l), Self::U64(r)) => l.partial_cmp(&(*r as i32)),
            (Self::I32(l), Self::U32(r)) => l.partial_cmp(&(*r as i32)),
            (Self::I32(l), Self::U16(r)) => l.partial_cmp(&(*r as i32)),
            (Self::I32(l), Self::U8(r)) => l.partial_cmp(&(*r as i32)),
            (Self::I64(l), Self::U64(r)) => l.partial_cmp(&(*r as i64)),
            (Self::I64(l), Self::U32(r)) => l.partial_cmp(&(*r as i64)),
            (Self::I64(l), Self::U16(r)) => l.partial_cmp(&(*r as i64)),
            (Self::I64(l), Self::U8(r)) => l.partial_cmp(&(*r as i64)),
            (Self::U8(l), Self::I64(r)) => l.partial_cmp(&(*r as u8)),
            (Self::U8(l), Self::I32(r)) => l.partial_cmp(&(*r as u8)),
            (Self::U8(l), Self::I16(r)) => l.partial_cmp(&(*r as u8)),
            (Self::U8(l), Self::I8(r)) => l.partial_cmp(&(*r as u8)),
            (Self::U16(l), Self::I32(r)) => l.partial_cmp(&(*r as u16)),
            (Self::U16(l), Self::I16(r)) => l.partial_cmp(&(*r as u16)),
            (Self::U16(l), Self::I8(r)) => l.partial_cmp(&(*r as u16)),
            (Self::U32(l), Self::I64(r)) => l.partial_cmp(&(*r as u32)),
            (Self::U32(l), Self::I32(r)) => l.partial_cmp(&(*r as u32)),
            (Self::U32(l), Self::I16(r)) => l.partial_cmp(&(*r as u32)),
            (Self::U32(l), Self::I8(r)) => l.partial_cmp(&(*r as u32)),
            (Self::U64(l), Self::I64(r)) => l.partial_cmp(&(*r as u64)),
            (Self::U64(l), Self::I32(r)) => l.partial_cmp(&(*r as u64)),
            (Self::U64(l), Self::I16(r)) => l.partial_cmp(&(*r as u64)),
            (Self::U64(l), Self::I8(r)) => l.partial_cmp(&(*r as u64)),
            _ => panic!("Cannot compare values"),
        }
    }
}
