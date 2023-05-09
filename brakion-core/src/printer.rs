use crate::repr::*;

pub(crate) struct Printer;

impl Printer {
    pub fn new() -> Self {
        Self
    }

    fn print_function_signature(&mut self, f: &FunctionSignature) {
        print!("(sig {}", f.name.name);
        for param in f.parameters.iter() {
            print!(" (param {} ", param.name.name);
            self.visit_type_reference(&param.ty);
            if let ParameterSpec::Preconditioned(ty) = &param.kind {
                print!(" (pre ");
                self.visit_type_reference(ty);
                print!(")");
            }
            print!(")");
        }
        print!(" (return ");
        self.visit_type_reference(&f.return_type);
        print!(")");

        print!(")");
    }
}

impl BrakionTreeVisitor for Printer {
    fn visit_decl(&mut self, decl: &Decl) {
        print!("(decl ");

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
                print!("(fn ");
                self.print_function_signature(&f.signature);
                print!(" (body");
                for stmt in f.body.iter() {
                    print!(" ");
                    self.visit_stmt(stmt);
                }
                print!("))");
            }
            DeclKind::Type { name, body } => {
                print!("(type {}", name.name);
                for variant in body.variants.iter() {
                    print!(" (variant {}", variant.name.name);
                    for field in variant.fields.iter() {
                        print!(" (field {} ", field.name.name);
                        self.visit_type_reference(&field.ty);
                        print!(")");
                    }
                    print!(")");
                }
                for method in body.methods.iter() {
                    if let Visibility::Public = method.0 {
                        print!(" (pub");
                    }
                    print!(" (method ");

                    self.print_function_signature(&method.1.signature);

                    print!(" (body");
                    for stmt in method.1.body.iter() {
                        print!(" ");
                        self.visit_stmt(stmt);
                    }
                    print!(")");

                    print!(")");

                    if let Visibility::Public = method.0 {
                        print!(")");
                    }
                }
            }
            DeclKind::Trait { name, body } => {
                print!("(trait {} ", name.name);
                for method in body.methods.iter() {
                    print!("(member ");

                    self.print_function_signature(method);

                    print!(")");
                }
            }
            DeclKind::Impl {
                trait_name,
                type_name,
                body,
            } => {
                print!("(impl ");
                for ns in trait_name.namespace.iter() {
                    print!("{}::", ns.name);
                }
                print!("{}", trait_name.ident.name);
                print!(" for ");
                self.visit_type_reference(type_name);

                for decl in body.iter() {
                    print!(" (member ");
                    self.print_function_signature(&decl.signature);
                    print!(" (body");
                    for stmt in decl.body.iter() {
                        print!(" ");
                        self.visit_stmt(stmt);
                    }
                    print!("))");
                }

                print!(")");
            }
        }

        println!(")");
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt {
            Stmt::Expr(e) => {
                print!("(expr ");
                self.visit_expr(e);
                print!(")");
            }
            Stmt::Block(body) => {
                print!("(block");
                for stmt in body.iter() {
                    print!(" ");
                    self.visit_stmt(stmt);
                }
                print!(")");
            }
            Stmt::Variable { name, ty, value } => {
                print!("(var {} ", name.name);
                self.visit_type_reference(ty);
                print!(" ");
                self.visit_expr(value);
                print!(")");
            }
            Stmt::Assign { target, value } => {
                print!("(assign ");
                self.visit_expr(target);
                print!(" ");
                self.visit_expr(value);
                print!(")");
            }
            Stmt::If {
                condition,
                then,
                otherwise,
            } => {
                print!("(if ");
                self.visit_expr(condition);
                print!(" ");
                self.visit_stmt(then);
                if let Some(otherwise) = otherwise {
                    print!(" ");
                    self.visit_stmt(otherwise);
                }
                print!(")");
            }
            Stmt::While { condition, body } => {
                print!("(while ");
                self.visit_expr(condition);
                print!(" ");
                self.visit_stmt(body);
                print!(")");
            }
            Stmt::For {
                name,
                iterable,
                body,
            } => {
                print!("(for {} in ", name.name);
                self.visit_expr(iterable);
                print!(" ");
                self.visit_stmt(body);
                print!(")");
            }
            Stmt::Match { expr, arms } => {
                print!("(match");
                match expr {
                    Some(expr) => print!(" {} ", expr.name),
                    None => (),
                }

                for arm in arms.iter() {
                    print!(" (arm ");
                    match &arm.pattern {
                        MatchPattern::Expr(e) => {
                            print!("(expr ");
                            self.visit_expr(e);
                            print!(")");
                        }
                        MatchPattern::Type(ty) => {
                            print!("(type ");
                            self.visit_type_reference(ty);
                            print!(")");
                        }
                        MatchPattern::Wildcard => print!("(wildcard)"),
                    }

                    print!(" ");
                    self.visit_stmt(&arm.body);
                    print!(")");
                }
                print!(")");
            }
            Stmt::Return(e) => {
                print!("(return ");
                self.visit_expr(e);
                print!(")");
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
                    print!("(list");
                    for expr in l.iter() {
                        print!(" ");
                        self.visit_expr(expr);
                    }
                    print!(")");
                }
                Literal::Void => print!("void"),
            }
            Expr::Grouping(e) => {
                print!("(group ");
                self.visit_expr(e);
                print!(")");
            }
            Expr::Unary { op, expr } => {
                print!("(unary ");
                match op {
                    UnaryOp::Neg => print!("-"),
                    UnaryOp::Not => print!("!"),
                }

                print!(" ");
                self.visit_expr(expr);
                print!(")");
            }
            Expr::Binary { left, op, right } => {
                print!("(binary ");
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
                print!(")");
            }
            Expr::Variable(v) => {
                print!("(var ");
                for ns in v.namespace.iter() {
                    print!("{}::", ns.name);
                }
                print!("{}", v.ident.name);
                print!(")");
            }
            Expr::Access { expr, field } => {
                print!("(access ");
                self.visit_expr(expr);
                print!(" {})", field.name);
            }
            Expr::Call { expr, args } => {
                print!("(call ");
                self.visit_expr(expr);
                for arg in args.iter() {
                    print!(" ");
                    self.visit_expr(arg);
                }
                print!(")");
            }
            Expr::ListAccess { expr, index } => {
                print!("(list-access ");
                self.visit_expr(expr);
                print!(" ");
                self.visit_expr(index);
                print!(")");
            },
            Expr::Constructor { ty, fields } => {
                print!("(ctor ");
                for ns in ty.namespace.iter() {
                    print!("{}::", ns.name);
                }
                print!("{}", ty.ident.name);
                for field in fields.iter() {
                    match field {
                        FieldConstructor::Named { name, value } => {
                            print!(" (field {} ", name.name);
                            self.visit_expr(value);
                            print!(")");
                        }
                        FieldConstructor::Auto(name) => {
                            print!(" (field {})", name.name);
                        }
                    }
                }
                print!(")");
            },
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
                print!("(list ");
                self.visit_type_reference(ty);
                print!(")");
            }
            TypeReference::Union(tys) => {
                print!("(union");
                for ty in tys.iter() {
                    print!(" ");
                    self.visit_type_reference(ty);
                }
                print!(")");
            }
        }
    }
}
