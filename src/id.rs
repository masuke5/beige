use lazy_static::lazy_static;
use rustc_hash::FxHashMap;
use std::fmt;
use std::num::NonZeroUsize;
use std::sync::RwLock;

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct Id(NonZeroUsize);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", IdMap::to_str(*self))
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id({}, {})", IdMap::to_str(*self), self.0)
    }
}

#[derive(Debug)]
pub struct IdMap {}

lazy_static! {
    static ref ID_MAP: RwLock<FxHashMap<Id, String>> = RwLock::new(FxHashMap::default());
    static ref STR_MAP: RwLock<FxHashMap<String, Id>> = RwLock::new(FxHashMap::default());
}

impl IdMap {
    pub fn new_id(id_str: &str) -> Id {
        let mut id_map = ID_MAP.write().expect("ID_MAP poisoned");
        let mut str_map = STR_MAP.write().expect("STR_MAP poisoned");

        match str_map.get(id_str) {
            Some(id) => *id,
            None => {
                // Add 1 to avoid 0
                let id = id_map.len() + 1;
                let id = unsafe { NonZeroUsize::new_unchecked(id) };
                let id = Id(id);

                id_map.insert(id, id_str.to_string());
                str_map.insert(id_str.to_string(), id);
                id
            }
        }
    }

    #[allow(dead_code)]
    pub fn get(id_str: &str) -> Option<Id> {
        let str_map = STR_MAP.read().expect("STR_MAP poisoned");
        str_map.get(id_str).copied()
    }

    pub fn to_str(id: Id) -> String {
        let id_map = ID_MAP.read().expect("ID_MAP poisoned");
        id_map[&id].clone()
    }
}
