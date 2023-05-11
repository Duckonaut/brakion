use std::cmp::Ordering;

use crate::repr::*;
use colored::Colorize;

pub(crate) struct Printer;

#[derive(Debug)]
pub struct PrinterNode {
    name: String,
    fields: Vec<PrinterNodeBranch>,
}

#[derive(Debug)]
enum PrinterNodeBranch {
    Flag(String),
    Descriptor(String, String),
    Field(String, PrinterNode),
    Node(PrinterNode),
}

impl PrinterNode {
    fn new(name: String) -> Self {
        Self {
            name,
            fields: Vec::new(),
        }
    }

    pub fn dump(self) {
        self.print(true, 0);
    }

    fn flag(&mut self, name: impl ToString) -> &mut Self {
        self.fields.push(PrinterNodeBranch::Flag(name.to_string()));
        self
    }

    fn descriptor(&mut self, name: impl ToString, value: impl ToString) -> &mut Self {
        self.fields.push(PrinterNodeBranch::Descriptor(
            name.to_string(),
            value.to_string(),
        ));
        self
    }

    fn field(&mut self, name: impl ToString, node: PrinterNode) -> &mut Self {
        self.fields
            .push(PrinterNodeBranch::Field(name.to_string(), node));
        self
    }

    fn node(&mut self, node: PrinterNode) -> &mut Self {
        self.fields.push(PrinterNodeBranch::Node(node));
        self
    }

    fn print(mut self, inline_name: bool, indent: usize) {
        self.fields.sort_by(|a, b| match (a, b) {
            (PrinterNodeBranch::Flag(a), PrinterNodeBranch::Flag(b)) => a.cmp(b),
            (PrinterNodeBranch::Flag(_), _) => Ordering::Less,
            (_, PrinterNodeBranch::Flag(_)) => Ordering::Greater,
            (PrinterNodeBranch::Descriptor(_, _), PrinterNodeBranch::Descriptor(_, _)) => Ordering::Equal,
            (PrinterNodeBranch::Descriptor(_, _), _) => Ordering::Less,
            (_, PrinterNodeBranch::Descriptor(_, _)) => Ordering::Greater,
            (PrinterNodeBranch::Field(_, _), PrinterNodeBranch::Field(_, _)) => Ordering::Equal,
            (PrinterNodeBranch::Field(_, _), _) => Ordering::Less,
            (_, PrinterNodeBranch::Field(_, _)) => Ordering::Greater,
            (PrinterNodeBranch::Node(_), PrinterNodeBranch::Node(_)) => Ordering::Equal,
        });
        let indent_str = " ".repeat(indent * 2);

        if inline_name {
            println!("{}", self.name.bold().bright_blue());
        } else {
            println!("{}{}", indent_str, self.name.bold().bright_blue());
        }

        for node in self.fields {
            match node {
                PrinterNodeBranch::Flag(name) => {
                    println!("{}- {}", indent_str, name.bright_yellow().italic());
                }
                PrinterNodeBranch::Descriptor(name, value) => {
                    println!("{}- {}: {}", indent_str, name.bold().bright_green(), value);
                }
                PrinterNodeBranch::Field(name, node) => {
                    print!("{}- {}: ", indent_str, name.bold());
                    node.print(true, indent + 1);
                }
                PrinterNodeBranch::Node(node) => {
                    node.print(false, indent + 1);
                }
            }
        }
    }
}

impl Printer {
    pub fn new() -> Self {
        Self
    }

    fn print_function_signature(&mut self, f: &mut FunctionSignature) -> PrinterNode {
        let mut node = PrinterNode::new("signature".to_string());
        node.descriptor("name", &f.name.name);
        node.field("return", self.visit_type_reference(&mut f.return_type));

        for param in f.parameters.iter_mut() {
            let mut param_node = PrinterNode::new("param".to_string());
            param_node.descriptor("name", &param.name.name);
            param_node.field("type", self.visit_type_reference(&mut param.ty));
            if let ParameterSpec::Preconditioned(ty) = &mut param.kind {
                param_node.field("precondition", self.visit_type_reference(ty));
            }
        }

        node
    }
}

impl BrakionTreeVisitor for Printer {
    type ExprResult = PrinterNode;
    type StmtResult = PrinterNode;
    type DeclResult = PrinterNode;
    type TypeReferenceResult = PrinterNode;

    fn visit_decl(&mut self, decl: &mut Decl) -> Self::DeclResult {
        let mut node = PrinterNode::new("decl".to_string());

        if let Visibility::Public = decl.visibility {
            node.flag("pub");
        }

        match &mut decl.kind {
            DeclKind::Module { name, body } => {
                let mut module_node = PrinterNode::new("module".to_string());
                module_node.descriptor("name", &name.name);
                for decl in body.iter_mut() {
                    module_node.field("member", self.visit_decl(decl));
                }

                node.node(module_node);
            }
            DeclKind::Function(f) => {
                let mut f_node = PrinterNode::new("fn".to_string());
                f_node.node(self.print_function_signature(&mut f.signature));

                let mut body_node = PrinterNode::new("body".to_string());
                for stmt in f.body.iter_mut() {
                    body_node.field("stmt", self.visit_stmt(stmt));
                }

                f_node.node(body_node);

                node.node(f_node);
            }
            DeclKind::Type { name, body } => {
                let mut type_node = PrinterNode::new("type".to_string());
                type_node.descriptor("name", &name.name);

                for variant in body.variants.iter_mut() {
                    let mut variant_node = PrinterNode::new("variant".to_string());
                    for field in variant.fields.iter_mut() {
                        let mut field_node = PrinterNode::new("field".to_string());
                        field_node.descriptor("name", &field.name.name);
                        field_node.field("type", self.visit_type_reference(&mut field.ty));

                        variant_node.node(field_node);
                    }
                    type_node.node(variant_node);
                }
                for method in body.methods.iter_mut() {
                    let mut method_node = PrinterNode::new("method".to_string());
                    if let Visibility::Public = method.0 {
                        method_node.flag("pub");
                    }
                    method_node.node(self.print_function_signature(&mut method.1.signature));

                    let mut body_node = PrinterNode::new("body".to_string());
                    for stmt in method.1.body.iter_mut() {
                        body_node.field("stmt", self.visit_stmt(stmt));
                    }
                    method_node.node(body_node);
                }

                node.node(type_node);
            }
            DeclKind::Trait { name, body } => {
                let mut trait_node = PrinterNode::new("trait".to_string());
                trait_node.descriptor("name", &name.name);

                for method in body.methods.iter_mut() {
                    trait_node.field("method", self.print_function_signature(method));
                }

                node.node(trait_node);
            }
            DeclKind::Impl {
                trait_name,
                type_name,
                body,
            } => {
                let mut impl_node = PrinterNode::new("impl".to_string());
                impl_node.descriptor("trait", &trait_name);
                impl_node.field("type", self.visit_type_reference(type_name));

                for decl in body.iter_mut() {
                    let mut member_node = PrinterNode::new("member".to_string());
                    member_node.node(self.print_function_signature(&mut decl.signature));

                    let mut body_node = PrinterNode::new("body".to_string());
                    for stmt in decl.body.iter_mut() {
                        body_node.field("stmt", self.visit_stmt(stmt));
                    }
                    member_node.node(body_node);
                    impl_node.node(member_node);
                }

                node.node(impl_node);
            }
        }

        node
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult {
        match &mut stmt.kind {
            StmtKind::Expr(e) => {
                let mut node = PrinterNode::new("expr".to_string());
                node.field("expr", self.visit_expr(e));
                node
            }
            StmtKind::Block(body) => {
                let mut node = PrinterNode::new("block".to_string());
                for stmt in body.iter_mut() {
                    node.field("stmt", self.visit_stmt(stmt));
                }
                node
            }
            StmtKind::Variable { name, ty, value } => {
                let mut node = PrinterNode::new("var".to_string());
                node.descriptor("name", &name.name);
                node.field("type", self.visit_type_reference(ty));
                node.field("value", self.visit_expr(value));
                node
            }
            StmtKind::Assign { target, value } => {
                let mut node = PrinterNode::new("assign".to_string());
                node.field("target", self.visit_expr(target));
                node.field("value", self.visit_expr(value));
                node
            }
            StmtKind::If {
                condition,
                then,
                otherwise,
            } => {
                let mut node = PrinterNode::new("if".to_string());
                node.field("condition", self.visit_expr(condition));
                node.field("then", self.visit_stmt(then));
                if let Some(otherwise) = otherwise {
                    node.field("otherwise", self.visit_stmt(otherwise));
                }
                node
            }
            StmtKind::While { condition, body } => {
                let mut node = PrinterNode::new("while".to_string());
                node.field("condition", self.visit_expr(condition));
                node.field("body", self.visit_stmt(body));
                node
            }
            StmtKind::For {
                name,
                iterable,
                body,
            } => {
                let mut node = PrinterNode::new("for".to_string());
                node.descriptor("name", &name.name);
                node.field("iterable", self.visit_expr(iterable));
                node.field("body", self.visit_stmt(body));
                node
            }
            StmtKind::Match { expr, arms } => {
                let mut node = PrinterNode::new("match".to_string());
                match expr {
                    Some(expr) => {
                        node.descriptor("expr", &expr.name);
                    }
                    None => (),
                }

                for arm in arms.iter_mut() {
                    let mut arm_node = PrinterNode::new("arm".to_string());
                    match &mut arm.pattern {
                        MatchPattern::Expr(e) => {
                            arm_node.field("expr", self.visit_expr(e));
                        }
                        MatchPattern::Type(ty) => {
                            arm_node.field("type", self.visit_type_reference(ty));
                        }
                        MatchPattern::Wildcard => {
                            arm_node.flag("wildcard");
                        }
                    }
                    arm_node.field("body", self.visit_stmt(&mut arm.body));
                    node.node(arm_node);
                }

                node
            }
            StmtKind::Return(e) => {
                let mut node = PrinterNode::new("return".to_string());
                node.field("expr", self.visit_expr(e));
                node
            }
            StmtKind::Break => PrinterNode::new("break".to_string()),
            StmtKind::Continue => PrinterNode::new("continue".to_string()),
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult {
        match &mut expr.kind {
            ExprKind::Literal(l) => match l {
                Literal::Int(i) => {
                    let mut node = PrinterNode::new("int".to_string());
                    node.flag(i);
                    node
                }
                Literal::Float(f) => {
                    let mut node = PrinterNode::new("float".to_string());
                    node.flag(f);
                    node
                }
                Literal::String(s) => {
                    let mut node = PrinterNode::new("string".to_string());
                    node.flag(s);
                    node
                }
                Literal::Char(c) => {
                    let mut node = PrinterNode::new("char".to_string());
                    node.flag(c);
                    node
                }
                Literal::Bool(b) => {
                    let mut node = PrinterNode::new("bool".to_string());
                    node.flag(b);
                    node
                }
                Literal::List(l) => {
                    let mut node = PrinterNode::new("list".to_string());
                    for expr in l.iter_mut() {
                        node.node(self.visit_expr(expr));
                    }
                    node
                }
                Literal::Void => PrinterNode::new("void".to_string()),
            },
            ExprKind::Unary { op, expr } => {
                let mut node = PrinterNode::new("unary".to_string());
                match op {
                    UnaryOp::Neg => node.flag("-"),
                    UnaryOp::Not => node.flag("!"),
                };
                node.field("expr", self.visit_expr(expr));
                node
            }
            ExprKind::Binary { left, op, right } => {
                let mut node = PrinterNode::new("binary".to_string());

                match op {
                    BinaryOp::Add => node.flag("+"),
                    BinaryOp::Sub => node.flag("-"),
                    BinaryOp::Mul => node.flag("*"),
                    BinaryOp::Div => node.flag("/"),
                    BinaryOp::Eq => node.flag("=="),
                    BinaryOp::Neq => node.flag("!="),
                    BinaryOp::Lt => node.flag("<"),
                    BinaryOp::Gt => node.flag(">"),
                    BinaryOp::Leq => node.flag("<="),
                    BinaryOp::Geq => node.flag(">="),
                    BinaryOp::And => node.flag("and"),
                    BinaryOp::Or => node.flag("or"),
                    BinaryOp::Is => node.flag("is"),
                };

                node.field("left", self.visit_expr(left));
                node.field("right", self.visit_expr(right));
                node
            }
            ExprKind::Cast { expr, ty } => {
                let mut node = PrinterNode::new("cast".to_string());
                node.field("expr", self.visit_expr(expr));
                node.field("type", self.visit_type_reference(ty));
                node
            }
            ExprKind::Variable(v) => {
                let mut node = PrinterNode::new("variable".to_string());
                node.descriptor("name", &v);
                node
            }
            ExprKind::Access { expr, field } => {
                let mut node = PrinterNode::new("access".to_string());
                node.field("expr", self.visit_expr(expr));
                node.descriptor("field", &field.name);
                node
            }
            ExprKind::Call { expr, args } => {
                let mut node = PrinterNode::new("call".to_string());
                node.field("expr", self.visit_expr(expr));
                let mut args_node = PrinterNode::new("args".to_string());
                for arg in args.iter_mut() {
                    args_node.node(self.visit_expr(arg));
                }
                node.node(args_node);
                node
            }
            ExprKind::Index { expr, index } => {
                let mut node = PrinterNode::new("index".to_string());
                node.field("expr", self.visit_expr(expr));
                node.field("index", self.visit_expr(index));
                node
            }
            ExprKind::Constructor { ty, fields } => {
                let mut node = PrinterNode::new("constructor".to_string());
                node.descriptor("type", ty);
                let mut fields_node = PrinterNode::new("fields".to_string());
                for field in fields.iter_mut() {
                    match field {
                        FieldConstructor::Named { name, value } => {
                            let mut field_node = PrinterNode::new("named".to_string());
                            field_node.descriptor("name", &name.name);
                            field_node.field("value", self.visit_expr(value));
                            fields_node.node(field_node);
                        }
                        FieldConstructor::Auto(name) => {
                            let mut field_node = PrinterNode::new("auto".to_string());
                            field_node.descriptor("name", &name.name);
                            fields_node.node(field_node);
                        }
                    }
                }
                node.node(fields_node);
                node
            }
        }
    }

    fn visit_type_reference(&mut self, ty: &mut TypeReference) -> Self::TypeReferenceResult {
        match &mut ty.kind {
            TypeReferenceKind::Void => PrinterNode::new("void".to_string()),
            TypeReferenceKind::Named(name) => {
                let mut node = PrinterNode::new("named".to_string());
                node.descriptor("name", name);
                node
            }
            TypeReferenceKind::List(ty) => {
                let mut node = PrinterNode::new("list".to_string());
                node.field("type", self.visit_type_reference(ty));
                node
            }
            TypeReferenceKind::Union(tys) => {
                let mut node = PrinterNode::new("union".to_string());
                for ty in tys.iter_mut() {
                    node.node(self.visit_type_reference(ty));
                }
                node
            }
            TypeReferenceKind::Infer => PrinterNode::new("infer".to_string()),
        }
    }
}
