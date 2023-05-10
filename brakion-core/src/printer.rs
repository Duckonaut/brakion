use crate::repr::*;
use colored::Colorize;

pub(crate) struct Printer;

macro_rules! paren_scope {
    { $i:expr, $($t:tt)* } => {
        print!("({}", $i.bright_blue());

        $($t)*

        print!(")");
    };
}

impl Printer {
    pub fn new() -> Self {
        Self
    }

    fn print_function_signature(&mut self, f: &FunctionSignature) {
        paren_scope! {
            "sig",
            print!(" {}", f.name.name);
            for param in f.parameters.iter() {
                print!(" ");
                paren_scope! {
                    "param",
                    print!(" {} ", param.name.name);
                    self.visit_type_reference(&param.ty);
                    if let ParameterSpec::Preconditioned(ty) = &param.kind {
                        print!(" ");
                        paren_scope! {
                            "pre ",
                            self.visit_type_reference(ty);
                        }
                    }
                }
            }
            print!(" ");
            paren_scope! {
                "return ",
                self.visit_type_reference(&f.return_type);
            }
        }
    }
}

impl BrakionTreeVisitor for Printer {
    fn visit_decl(&mut self, decl: &Decl) {
        paren_scope! {
            "decl ",
            if let Visibility::Public = decl.visibility {
                print!("pub ");
            }

            match &decl.kind {
                DeclKind::Module { name, body } => {
                    print!("(mod {}", name.name);
                    for decl in body.iter() {
                        print!(" ");
                        self.visit_decl(decl);
                    }
                }
                DeclKind::Function(f) => {
                    paren_scope! {
                    "fn ",
                    self.print_function_signature(&f.signature);
                        paren_scope! {
                            "body",
                            for stmt in f.body.iter() {
                                print!(" ");
                                self.visit_stmt(stmt);
                            }
                        }
                    }
                }
                DeclKind::Type { name, body } => {
                    paren_scope! {
                        format!("type {}", name.name),
                        for variant in body.variants.iter() {
                            print!(" ");
                            paren_scope! {
                                format!("variant {}", variant.name.name),
                                for field in variant.fields.iter() {
                                    print!(" ");
                                    paren_scope! {
                                        format!("field {} ", field.name.name),
                                        self.visit_type_reference(&field.ty);
                                    }
                                }
                            }
                        }
                        for method in body.methods.iter() {
                            if let Visibility::Public = method.0 {
                                print!(" (pub");
                            }
                            print!(" ");
                            paren_scope! {
                                "method ",
                                self.print_function_signature(&method.1.signature);

                                print!(" ");
                                paren_scope! {
                                    "body",
                                    for stmt in method.1.body.iter() {
                                        print!(" ");
                                        self.visit_stmt(stmt);
                                    }
                                }
                            }

                            if let Visibility::Public = method.0 {
                                print!(")");
                            }
                        }
                    }
                }
                DeclKind::Trait { name, body } => {
                    paren_scope! {
                        format!("trait {}", name.name),
                        for method in body.methods.iter() {
                            print!(" ");

                            paren_scope! {
                                "method ",
                                self.print_function_signature(method);
                            }
                        }
                    }
                }
                DeclKind::Impl {
                    trait_name,
                    type_name,
                    body,
                } => {
                    paren_scope! {
                        "impl ",
                        for ns in trait_name.namespace.iter() {
                            print!("{}::", ns.name);
                        }
                        print!("{}", trait_name.ident.name);
                        print!(" for ");
                        self.visit_type_reference(type_name);

                        for decl in body.iter() {
                            print!(" ");
                            paren_scope! {
                                "member ",
                                self.print_function_signature(&decl.signature);
                                print!(" ");
                                paren_scope! {
                                    "body",
                                    for stmt in decl.body.iter() {
                                        print!(" ");
                                        self.visit_stmt(stmt);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt {
            Stmt::Expr(e) => {
                paren_scope! {
                    "expr ",
                    self.visit_expr(e);
                }
            }
            Stmt::Block(body) => {
                paren_scope! {
                    "body",
                    for stmt in body.iter() {
                        print!(" ");
                        self.visit_stmt(stmt);
                    }
                }
            }
            Stmt::Variable { name, ty, value } => {
                paren_scope! {
                    format!("var {} ", name.name),
                    self.visit_type_reference(ty);
                    print!(" ");
                    self.visit_expr(value);
                }
            }
            Stmt::Assign { target, value } => {
                paren_scope! {
                    "assign ",
                    self.visit_expr(target);
                    print!(" ");
                    self.visit_expr(value);
                }
            }
            Stmt::If {
                condition,
                then,
                otherwise,
            } => {
                paren_scope! {
                "if ",
                    self.visit_expr(condition);
                    print!(" ");
                    self.visit_stmt(then);
                    if let Some(otherwise) = otherwise {
                        print!(" ");
                        self.visit_stmt(otherwise);
                    }
                }
            }
            Stmt::While { condition, body } => {
                paren_scope! {
                    "while ",
                    self.visit_expr(condition);
                    print!(" ");
                    self.visit_stmt(body);
                }
            }
            Stmt::For {
                name,
                iterable,
                body,
            } => {
                paren_scope! {
                    format!("for {} in ", name.name),
                    self.visit_expr(iterable);
                    print!(" ");
                    self.visit_stmt(body);
                }
            }
            Stmt::Match { expr, arms } => {
                paren_scope! {
                    "match",
                    match expr {
                        Some(expr) => print!(" {} ", expr.name),
                        None => (),
                    }

                    for arm in arms.iter() {
                        print!(" ");
                        paren_scope! {
                            "arm ",
                            match &arm.pattern {
                                MatchPattern::Expr(e) => {
                                    paren_scope! {
                                        "expr ",
                                        self.visit_expr(e);
                                    }
                                }
                                MatchPattern::Type(ty) => {
                                    paren_scope! {
                                        "type ",
                                        self.visit_type_reference(ty);
                                    }
                                }
                                MatchPattern::Wildcard => print!("(wildcard)"),
                            }

                            print!(" ");
                            self.visit_stmt(&arm.body);
                        }
                    }
                }
            }
            Stmt::Return(e) => {
                paren_scope! {
                    "return ",
                    self.visit_expr(e);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr {
            Expr::Literal(l) => match l {
                Literal::Int(i) => print!("{}", i),
                Literal::Float(f) => print!("{}", f),
                Literal::String(s) => print!("\"{}\"", s),
                Literal::Char(c) => print!("'{}'", c),
                Literal::Bool(b) => print!("{}", b),
                Literal::List(l) => {
                    paren_scope! {
                        "list",
                        for expr in l.iter() {
                            print!(" ");
                            self.visit_expr(expr);
                        }
                    }
                }
                Literal::Void => print!("void"),
            },
            Expr::Grouping(e) => {
                paren_scope! {
                    "group ",
                    self.visit_expr(e);
                }
            }
            Expr::Unary { op, expr } => {
                paren_scope! {
                    "unary ",
                    match op {
                        UnaryOp::Neg => print!("-"),
                        UnaryOp::Not => print!("!"),
                    }

                    print!(" ");
                    self.visit_expr(expr);
                }
            }
            Expr::Binary { left, op, right } => {
                paren_scope! {
                    "binary ",
                    match op {
                        BinaryOp::Add => print!("+"),
                        BinaryOp::Sub => print!("-"),
                        BinaryOp::Mul => print!("*"),
                        BinaryOp::Div => print!("/"),
                        BinaryOp::Eq => print!("=="),
                        BinaryOp::Neq => print!("!="),
                        BinaryOp::Lt => print!("<"),
                        BinaryOp::Gt => print!(">"),
                        BinaryOp::Leq => print!("<="),
                        BinaryOp::Geq => print!(">="),
                        BinaryOp::And => print!("and"),
                        BinaryOp::Or => print!("or"),
                        BinaryOp::Is => print!("is"),
                        BinaryOp::As => print!("as"),
                    }

                    print!(" ");
                    self.visit_expr(left);
                    print!(" ");
                    self.visit_expr(right);
                }
            }
            Expr::Variable(v) => {
                paren_scope! {
                    "var ",
                    for ns in v.namespace.iter() {
                        print!("{}::", ns.name);
                    }
                    print!("{}", v.ident.name);
                }
            }
            Expr::Access { expr, field } => {
                paren_scope! {
                    "access ",
                    self.visit_expr(expr);
                    print!(" {}", field.name);
                }
            }
            Expr::Call { expr, args } => {
                paren_scope! {
                    "call ",
                    self.visit_expr(expr);
                    for arg in args.iter() {
                        print!(" ");
                        self.visit_expr(arg);
                    }
                }
            }
            Expr::ListAccess { expr, index } => {
                paren_scope! {
                    "list-access ",
                    self.visit_expr(expr);
                    print!(" ");
                    self.visit_expr(index);
                }
            }
            Expr::Constructor { ty, fields } => {
                paren_scope! {
                    "ctor ",
                    for ns in ty.namespace.iter() {
                        print!("{}::", ns.name);
                    }
                    print!("{}", ty.ident.name);
                    for field in fields.iter() {
                        match field {
                            FieldConstructor::Named { name, value } => {
                                print!(" ");
                                paren_scope! {
                                    format!("field {} ", name.name),
                                    self.visit_expr(value);
                                }
                            }
                            FieldConstructor::Auto(name) => {
                                print!(" (field {})", name.name);
                            }
                        }
                    }
                }
            }
        }
    }

    fn visit_type_reference(&mut self, ty: &TypeReference) {
        match &ty {
            TypeReference::Void => print!("void"),
            TypeReference::Named(name) => {
                for ns in name.namespace.iter() {
                    print!("{}::", ns.name);
                }
                print!("{}", name.ident.name);
            }
            TypeReference::List(ty) => {
                paren_scope! {
                "list ",
                self.visit_type_reference(ty);
                }
            }
            TypeReference::Union(tys) => {
                paren_scope! {
                    "union",
                    for ty in tys.iter() {
                        print!(" ");
                        self.visit_type_reference(ty);
                    }
                }
            }
        }
    }
}
