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
pub enum TypeError {
    Mismatch(Type, Type),
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
    Unique(Box<TypeCon>, Unique),
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

pub fn unify(pool: &mut TypePool, a: Type, b: Type) -> Result<Type, TypeError> {
    type T = Type;
    type C = TypeCon;

    match (a, b) {
        (T::App(C::TyFun(params, body), args), b) => {
            // Remove TyFun
            let new_map: FxHashMap<TypeVar, Type> =
                params.into_iter().zip(args.into_iter()).collect();
            let a = subst(pool, *body, &new_map);
            unify(pool, a, b)
        }
        (a, T::App(C::TyFun(params, body), args)) => {
            // Remove TyFun
            let new_map: FxHashMap<TypeVar, Type> =
                params.into_iter().zip(args.into_iter()).collect();
            let b = subst(pool, *body, &new_map);
            unify(pool, a, b)
        }
        (T::App(C::Unique(tycon, au), at), T::App(C::Unique(tycon_b, bu), bt)) => {
            if au != bu {
                // Return error if uniques are different, though the same tycon.
                Err(TypeError::Mismatch(
                    T::App(C::Unique(tycon, au), at),
                    T::App(C::Unique(tycon_b, bu), bt),
                ))
            } else {
                let types: Result<Vec<Type>, TypeError> = at
                    .into_iter()
                    .zip(bt)
                    .map(|(a, b)| unify(pool, a, b))
                    .collect();
                Ok(T::App(C::Unique(tycon, au), types?))
            }
        }
        (T::App(ac, at), T::App(bc, bt)) if ac == bc => {
            // Check manually to return a correct error
            for (at, bt) in at.iter().zip(bt.iter()) {
                if at != bt {
                    return Err(TypeError::Mismatch(at.clone(), bt.clone()));
                }
            }

            let types: Result<Vec<Type>, TypeError> = at
                .into_iter()
                .zip(bt)
                .map(|(a, b)| unify(pool, a, b))
                .collect();
            Ok(T::App(ac, types?))
        }
        (T::Poly(ap, at), T::Poly(bp, bt)) => {
            let map: FxHashMap<TypeVar, Type> = bp
                .into_iter()
                .zip(ap.iter().map(|var| T::Var(*var)))
                .collect();
            let bt = subst(pool, *bt, &map);
            Ok(T::Poly(ap, Box::new(unify(pool, *at, bt)?)))
        }
        (T::Var(av), T::Var(bv)) if av == bv => Ok(T::Var(av)),
        (a, b) => Err(TypeError::Mismatch(a, b)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type T = Type;
    type C = TypeCon;

    fn int() -> Type {
        T::App(C::Int, vec![])
    }

    fn bool() -> Type {
        T::App(C::Bool, vec![])
    }

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
        let mut pool = TypePool::new();
        let var_a = pool.new_tyvar();

        let mut map = FxHashMap::default();
        map.insert(var_a, T::App(C::Bool, vec![]));

        let ty = T::Poly(vec![var_a], Box::new(T::App(C::Tuple, vec![T::Var(var_a)])));

        let expected = T::Poly(
            vec![TypeVar(1)],
            Box::new(T::App(C::Tuple, vec![T::Var(TypeVar(1))])),
        );
        let actual = subst(&mut pool, ty, &map);

        assert_eq!(expected, actual);
    }

    #[test]
    fn unify_simple() {
        let mut pool = TypePool::new();
        let var = pool.new_tyvar();
        let uniq = pool.new_unique();

        // int, int => int
        assert_eq!(Ok(int()), unify(&mut pool, int(), int()));
        // (int, bool), (int, int) => error
        assert_eq!(
            Err(TypeError::Mismatch(bool(), int())),
            unify(
                &mut pool,
                T::App(C::Tuple, vec![int(), bool()]),
                T::App(C::Tuple, vec![int(), int()])
            ),
        );
        // var, var => var
        assert_eq!(Ok(T::Var(var)), unify(&mut pool, T::Var(var), T::Var(var)));
        // uniq, uniq => uniq
        assert!(unify(
            &mut pool,
            T::App(C::Unique(Box::new(C::Int), uniq), vec![]),
            T::App(C::Unique(Box::new(C::Bool), uniq), vec![]),
        )
        .is_ok())
    }

    #[test]
    fn unify_with_tyfun() {
        let mut pool = TypePool::new();
        let v1 = pool.new_tyvar();
        let v2 = pool.new_tyvar();

        // (fun(v1) -> List<v1>)(bool), (fun(v2) -> List<v2>)(bool) => List<bool>
        assert_eq!(
            Ok(T::App(C::List, vec![bool()])),
            unify(
                &mut pool,
                T::App(
                    C::TyFun(vec![v1], Box::new(T::App(C::List, vec![T::Var(v1)]))),
                    vec![bool()],
                ),
                T::App(
                    C::TyFun(vec![v2], Box::new(T::App(C::List, vec![T::Var(v2)]))),
                    vec![bool()],
                ),
            )
        );
    }

    #[test]
    fn unify_with_poly() {
        let mut pool = TypePool::new();
        let v1 = pool.new_tyvar();
        let v2 = pool.new_tyvar();
        let v3 = pool.new_tyvar();

        // poly<v1>(List<v1>), poly<v2>(List<v2>) -> poly<v1>(List<v1>)
        assert_eq!(
            Ok(T::Poly(
                vec![v1],
                Box::new(T::App(C::List, vec![T::Var(v1)]))
            )),
            unify(
                &mut pool,
                T::Poly(vec![v1], Box::new(T::App(C::List, vec![T::Var(v1)]))),
                T::Poly(vec![v2], Box::new(T::App(C::List, vec![T::Var(v2)]))),
            )
        );

        // poly<v1>(List<v1>), poly<v2>(List<v3>) -> error
        assert_eq!(
            Err(TypeError::Mismatch(Type::Var(v1), Type::Var(v3))),
            unify(
                &mut pool,
                T::Poly(vec![v1], Box::new(T::App(C::List, vec![T::Var(v1)]))),
                T::Poly(vec![v2], Box::new(T::App(C::List, vec![T::Var(v3)]))),
            )
        );
    }
}
