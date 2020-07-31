use rustc_hash::FxHashMap;

use crate::ast::{Type as AstType, TypeKind as AstTypeKind, *};
use crate::id::{Id, IdMap};
use crate::scope_map::ScopeMap;
use crate::ty::{generate_arrow_type, unify, MetaMap, Type, TypeCon, TypeError, TypePool};

type AT = AstTypeKind;
type T = Type;
type C = TypeCon;

pub type TypedExpr = Expr<Type>;
pub type TypedExprKind = ExprKind<Type>;
pub type TypedFunction = Function<Type>;
pub type TypedModule = Module<Type>;

macro_rules! try_some {
    ($($var:ident),*) => {
        $(let $var = $var?;)*
    };
}

macro_rules! try_some_mut {
    ($($var:ident),*) => {
        $(let mut $var = $var?;)*
    };
}

macro_rules! try_unify {
    ($self:expr, $metas:expr, $a:expr, $b:expr) => {
        $a.ty = match unify(&mut $self.pool, $metas, $a.ty.clone(), $b) {
            Ok(t) => t,
            Err(TypeError::Mismatch(ad, bd)) => {
                error!(&$a.span, "`{}` is not equivalent to `{}`", ad, bd);
                return None;
            }
            Err(TypeError::Circulation(ty, metavar)) => {
                error!(&$a.span, "detected circulation: `{}` in `{}`", metavar, ty);
                return None;
            }
        }
    };
}

#[derive(Debug)]
struct Typing {
    vars: ScopeMap<Id, Type>,
    types: ScopeMap<Id, TypeCon>,
    pool: TypePool,
}

impl Typing {
    fn new() -> Self {
        Self {
            vars: ScopeMap::new(),
            types: ScopeMap::new(),
            pool: TypePool::new(),
        }
    }

    fn push_scope(&mut self) {
        self.vars.push_scope();
        self.types.push_scope();
    }

    fn pop_scope(&mut self) {
        self.vars.pop_scope();
        self.types.pop_scope();
    }

    fn infer_expr(&mut self, metas: &mut MetaMap, expr: UntypedExpr) -> Option<TypedExpr> {
        type E = UntypedExprKind;
        type TE = TypedExprKind;

        let (typed_expr, ty) = match expr.kind {
            E::Int(n) => (TE::Int(n), T::App(C::Int, vec![])),
            E::Float(n) => (TE::Float(n), T::App(C::Float, vec![])),
            E::Bool(b) => (TE::Bool(b), T::App(C::Bool, vec![])),
            E::String(s) => (TE::String(s), T::App(C::String, vec![])),
            E::Tuple(exprs) => {
                let mut typed_exprs = Vec::with_capacity(exprs.len());
                let mut types = Vec::with_capacity(exprs.len());

                for expr in exprs {
                    if let Some(expr) = self.infer_expr(metas, expr) {
                        types.push(expr.ty.clone());
                        typed_exprs.push(expr);
                    }
                }

                (TE::Tuple(typed_exprs), T::App(C::Tuple, types))
            }
            E::Record(fields) => {
                let mut typed_fields = Vec::with_capacity(fields.len());
                let field_names: Vec<Id> = fields.iter().map(|(name, _)| name.value).collect();
                let mut field_types = Vec::with_capacity(fields.len());

                for (name, expr) in fields {
                    if let Some(expr) = self.infer_expr(metas, expr) {
                        field_types.push(expr.ty.clone());
                        typed_fields.push((name, expr));
                    }
                }

                (
                    TE::Record(typed_fields),
                    T::App(C::Record(field_names), field_types),
                )
            }
            E::Variable(name) => match self.vars.get(&name) {
                Some(ty) => (TE::Variable(name), ty.clone()),
                None => {
                    error!(&expr.span, "undefined variable");
                    return None;
                }
            },
            E::BinOp(binop, lhs, rhs) => {
                let lhs = self.infer_expr(metas, *lhs);
                let rhs = self.infer_expr(metas, *rhs);
                try_some_mut!(lhs, rhs);

                let ty = match binop {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Mod
                    | BinOp::LessThan
                    | BinOp::LessThanOrEqual
                    | BinOp::GreaterThan
                    | BinOp::GreaterThanOrEqual => {
                        try_unify!(self, metas, lhs, T::App(C::Int, vec![]));
                        try_unify!(self, metas, rhs, T::App(C::Int, vec![]));
                        T::App(C::Int, vec![])
                    }
                    BinOp::Equal | BinOp::NotEqual => {
                        try_unify!(self, metas, rhs, lhs.ty.clone());
                        T::App(C::Bool, vec![])
                    }
                    BinOp::And | BinOp::Or => {
                        try_unify!(self, metas, lhs, T::App(C::Bool, vec![]));
                        try_unify!(self, metas, rhs, T::App(C::Bool, vec![]));
                        T::App(C::Bool, vec![])
                    }
                };

                (TE::BinOp(binop, Box::new(lhs), Box::new(rhs)), ty)
            }
            E::Let(name, expr) => {
                let expr = self.infer_expr(metas, *expr)?;
                self.vars.insert(name, expr.ty.clone());

                (TE::Let(name, Box::new(expr)), T::App(C::Unit, vec![]))
            }
            E::LetFun(name, func) => {
                let func = self.infer_func(func)?;
                let ty = generate_arrow_type(&func.param_types, &func.body.ty);
                self.vars.insert(name, ty);

                (TE::LetFun(name, func), T::App(C::Unit, vec![]))
            }
            E::Not(expr) => {
                let mut expr = self.infer_expr(metas, *expr)?;
                try_unify!(self, metas, expr, T::App(C::Bool, vec![]));

                (TE::Not(Box::new(expr)), T::App(C::Bool, vec![]))
            }
            E::Negative(expr) => {
                let mut expr = self.infer_expr(metas, *expr)?;
                try_unify!(self, metas, expr, T::App(C::Int, vec![]));

                (TE::Negative(Box::new(expr)), T::App(C::Int, vec![]))
            }
            E::Call(func, arg) => {
                let func = self.infer_expr(metas, *func);
                let arg = self.infer_expr(metas, *arg);
                try_some!(func, arg);

                let mut arg = arg;

                let (arg_ty, return_ty) = match &func.ty {
                    T::App(C::Arrow, types) => (&types[0], &types[1]),
                    _ => {
                        error!(&func.span, "expected arrow type");
                        return None;
                    }
                };

                try_unify!(self, metas, arg, arg_ty.clone());

                let ty = return_ty.clone();
                (TE::Call(Box::new(func), Box::new(arg)), ty)
            }
            E::If(cond, then_expr, else_expr) => {
                let cond = self.infer_expr(metas, *cond);
                let then_expr = self.infer_expr(metas, *then_expr);
                let mut else_expr = if let Some(else_expr) = else_expr {
                    Some(self.infer_expr(metas, *else_expr)?)
                } else {
                    None
                };

                try_some!(cond, then_expr);
                let mut cond = cond;

                try_unify!(self, metas, cond, T::App(C::Bool, vec![]));

                if let Some(else_expr) = &mut else_expr {
                    try_unify!(self, metas, else_expr, then_expr.ty.clone());
                }

                let ty = then_expr.ty.clone();
                (
                    TE::If(Box::new(cond), Box::new(then_expr), else_expr.map(Box::new)),
                    ty,
                )
            }
            E::Do(exprs) => {
                assert!(!exprs.is_empty());

                self.push_scope();

                let mut new_exprs = Vec::new();
                for expr in exprs {
                    if let Some(expr) = self.infer_expr(metas, expr) {
                        new_exprs.push(expr);
                    }
                }

                self.pop_scope();

                let ty = match new_exprs.last() {
                    Some(result_expr) => result_expr.ty.clone(),
                    None => return None,
                };

                (TE::Do(new_exprs), ty)
            }
        };

        Some(TypedExpr::with_ty(typed_expr, expr.span, ty))
    }

    fn infer_func(&mut self, func: UntypedFunction) -> Option<TypedFunction> {
        self.push_scope();

        // Insert parameters as variables with meta type
        let mut param_metavars = Vec::with_capacity(func.params.len());
        for name in &func.params {
            let meta = self.pool.new_meta();
            self.vars.insert(name.value, T::Meta(meta));
            param_metavars.push(meta);
        }

        let mut metas = FxHashMap::default();
        let body = self.infer_expr(&mut metas, *func.body)?;

        self.pop_scope();

        // Get inferred parameter types
        let mut param_types = Vec::with_capacity(func.params.len());
        for (metavar, name) in param_metavars.into_iter().zip(func.params.iter()) {
            let ty = match metas.get(&metavar) {
                Some(ty) => ty.clone(),
                None => {
                    // TODO: generalize
                    warn!(&name.span, "cannot infer type");
                    T::App(C::Unit, vec![])
                }
            };
            param_types.push(ty);
        }

        Some(TypedFunction {
            name: func.name,
            params: func.params,
            param_types,
            body: Box::new(body),
        })
    }

    fn trans_ty(&mut self, ty: AstType) -> Option<T> {
        match ty.value {
            AT::Int => Some(T::App(C::Int, vec![])),
            AT::Float => Some(T::App(C::Float, vec![])),
            AT::Bool => Some(T::App(C::Bool, vec![])),
            AT::String => Some(T::App(C::String, vec![])),
            AT::Record(fields) => {
                // Split into field name and type
                let field_names: Vec<Id> = fields.iter().map(|field| field.name.value).collect();

                let mut field_types = Vec::with_capacity(fields.len());
                for field in fields {
                    if let Some(ty) = self.trans_ty(field.ty) {
                        field_types.push(ty);
                    }
                }

                if field_types.len() != field_names.len() {
                    // trans_tyに一つでも失敗していたらNoneを返す
                    None
                } else {
                    Some(T::App(C::Record(field_names), field_types))
                }
            }
            AT::Tuple(types) => {
                let types_len = types.len();
                let mut new_types = Vec::with_capacity(types.len());
                for ty in types {
                    if let Some(ty) = self.trans_ty(ty) {
                        new_types.push(ty);
                    }
                }

                if new_types.len() != types_len {
                    None
                } else {
                    Some(T::App(C::Tuple, new_types))
                }
            }
            AT::Name(name) => match self.types.get(&name) {
                Some(tycon) => Some(T::App(tycon.clone(), vec![])),
                None => {
                    error!(&ty.span, "undefined type `{}`", name);
                    None
                }
            },
        }
    }

    fn infer(mut self, module: UntypedModule) -> Option<TypedModule> {
        self.push_scope();

        let mut new_module = TypedModule::new(module.path);

        // Insert types
        for (name, (_, typedef)) in module.types {
            if let Some(ty) = self.trans_ty(typedef.body) {
                let tycon = TypeCon::TyFun(vec![], Box::new(ty));
                self.types.insert(name, tycon);
            }
        }

        // Infer constants
        for (name, (visibility, expr)) in module.constants {
            if let Some(expr) = self.infer_expr(&mut FxHashMap::default(), expr) {
                self.vars.insert(name, expr.ty.clone());
                new_module.constants.insert(name, (visibility, expr));
            }
        }

        // Infer function
        for (name, (visibility, func)) in module.functions {
            if let Some(func) = self.infer_func(func) {
                if func.name == IdMap::new_id("main") && visibility != Visibility::Public {
                    error!(&func.params[0].span, "the main function must be public");
                }

                // Insert as variables
                let ty = generate_arrow_type(&func.param_types, &func.body.ty);
                self.vars.insert(func.name, ty);

                new_module.functions.insert(name, (visibility, func));
            }
        }

        self.pop_scope();

        Some(new_module)
    }
}

pub fn infer_module(module: UntypedModule) -> Option<TypedModule> {
    let typing = Typing::new();
    typing.infer(module)
}
