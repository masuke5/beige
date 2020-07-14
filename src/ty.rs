use std::iter;

use rustc_hash::FxHashMap;

use crate::id::Id;

macro_rules! define_unique {
    ($unique:ident, $generator:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $unique(u64);

        #[derive(Debug)]
        pub struct $generator {
            next: u64,
        }

        impl $generator {
            #[allow(dead_code)]
            pub fn new() -> Self {
                Self { next: 0 }
            }

            #[allow(dead_code)]
            pub fn next(&mut self) -> $unique {
                let id = self.next;
                self.next += 1;
                $unique(id)
            }
        }
    };
}

define_unique!(TypeVar, TypeVarGenerator);
define_unique!(MetaVar, MetaVarGenerator);
define_unique!(Unique, UniqueGenerator);

#[derive(Debug)]
pub struct TypePool {
    tyvar: TypeVarGenerator,
    metavar: MetaVarGenerator,
    unique: UniqueGenerator,
}

impl TypePool {
    pub fn new() -> Self {
        Self {
            tyvar: TypeVarGenerator::new(),
            metavar: MetaVarGenerator::new(),
            unique: UniqueGenerator::new(),
        }
    }

    pub fn new_tyvar(&mut self) -> TypeVar {
        self.tyvar.next()
    }

    pub fn new_unique(&mut self) -> Unique {
        self.unique.next()
    }

    pub fn new_meta(&mut self) -> MetaVar {
        self.metavar.next()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    App(TypeCon, Vec<Type>),
    Var(TypeVar),
    Poly(Vec<TypeVar>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCon {
    Int,
    Float,
    String,
    Bool,
    Unit,
    List,
    Arrow,
    Tuple,
    Record(Vec<Id>),
    TyFun(Vec<TypeVar>, Box<Type>),
    Unique(Unique),
}

pub fn subst(pool: &mut TypePool, ty: Type, map: &FxHashMap<TypeVar, Type>) -> Type {
    match ty {
        Type::Var(var) => match map.get(&var) {
            Some(ty) => ty.clone(),
            None => Type::Var(var),
        },
        Type::App(TypeCon::TyFun(params, body), args) => {
            let new_map: FxHashMap<TypeVar, Type> =
                params.into_iter().zip(args.into_iter()).collect();
            let inner = subst(pool, *body, &new_map);
            subst(pool, inner, map)
        }
        Type::App(tycon, types) => {
            let types: Vec<Type> = types.into_iter().map(|t| subst(pool, t, map)).collect();
            Type::App(tycon, types)
        }
        Type::Poly(vars, ty) => {
            // alpha-conversion
            let new_vars: Vec<TypeVar> = iter::repeat_with(|| pool.new_tyvar())
                .take(vars.len())
                .collect();

            let mut new_map = map.clone();
            for (var, new_var) in vars.iter().zip(new_vars.iter()) {
                new_map.insert(*var, Type::Var(*new_var));
            }

            let ty = subst(pool, *ty, &new_map);
            Type::Poly(new_vars, Box::new(ty))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subst_with_tyfun() {
        use Type::*;
        use TypeCon::*;

        let mut pool = TypePool::new();
        let var_a = pool.new_tyvar();
        let var_b = pool.new_tyvar();
        let var_c = pool.new_tyvar();

        let mut map = FxHashMap::default();
        map.insert(var_a, App(Bool, vec![]));

        // (Int, Var(var_a), Var(var_c), App(TyFun([var_b], App(List, [var_b])), [String])
        let ty = App(
            Tuple,
            vec![
                App(Int, vec![]),
                Var(var_a),
                Var(var_c),
                App(
                    TyFun(vec![var_b], Box::new(App(List, vec![Var(var_b)]))),
                    vec![App(String, vec![])],
                ),
            ],
        );
        // (Int, Bool, Var(var_c), App(List, [String]))
        let expected = App(
            Tuple,
            vec![
                App(Int, vec![]),
                App(Bool, vec![]),
                Var(var_c),
                App(List, vec![App(String, vec![])]),
            ],
        );

        let actual = subst(&mut pool, ty, &map);

        assert_eq!(expected, actual);
    }

    #[test]
    fn subst_with_poly() {
        use Type::*;
        use TypeCon::*;

        let mut pool = TypePool::new();
        let var_a = pool.new_tyvar();

        let mut map = FxHashMap::default();
        map.insert(var_a, App(Bool, vec![]));

        let ty = Poly(vec![var_a], Box::new(App(Tuple, vec![Var(var_a)])));

        let expected = Poly(
            vec![TypeVar(1)],
            Box::new(App(Tuple, vec![Var(TypeVar(1))])),
        );
        let actual = subst(&mut pool, ty, &map);

        assert_eq!(expected, actual);
    }
}
