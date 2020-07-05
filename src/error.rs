use std::fmt;
use std::fs;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::RwLock;

use lazy_static::lazy_static;
use rustc_hash::FxHashMap;

use crate::id::Id;
use crate::span::Span;

#[derive(Debug, PartialEq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug, PartialEq)]
pub struct Error<'a> {
    pub level: Level,
    pub msg: String,
    pub span: &'a Span,
}

impl<'a> Error<'a> {
    pub fn new(msg: &str, span: &'a Span) -> Self {
        Self {
            level: Level::Error,
            msg: msg.to_string(),
            span,
        }
    }

    pub fn new_warning(msg: &str, span: &'a Span) -> Self {
        Self {
            level: Level::Warning,
            msg: msg.to_string(),
            span,
        }
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}:{}",
            self.msg,
            self.span.line + 1,
            self.span.col
        )
    }
}

#[cfg(debug_assertions)]
macro_rules! error {
    ($span:expr, $fmt: tt $(,$arg:expr)*) => {{
        use crate::error::{Error, ErrorList};

        ErrorList::push(Error::new(
            &format!(concat!($fmt, " <{}:{}>") $(,$arg)*, file!(), line!()),
            $span,
        ));
    }};
}

#[cfg(not(debug_assertions))]
macro_rules! error {
    ($span:expr, $fmt: tt $(,$arg:expr)*) => {{
        use crate::error::{Error, ErrorList};

        ErrorList::push(Error::new(&format!($fmt $(,$arg)*), $span));
    }};
}

#[cfg(debug_assertions)]
macro_rules! warn {
    ($span:expr, $fmt: tt $(,$arg:expr)*) => {{
        use crate::error::{Error, ErrorList};

        ErrorList::push(Error::new_warning(
            &format!(concat!($fmt, " <{}:{}>") $(,$arg)*, file!(), line!()),
            $span,
        ));
    }};
}

#[cfg(not(debug_assertions))]
macro_rules! warn {
    ($span:expr, $fmt: tt $(,$arg:expr)*) => {
        ErrorList::push(Error::new_warning(&format!($fmt $(,$arg)*), $span));
    };
}

#[derive(Debug)]
pub struct ErrorList {}

static HAS_ERRORS: AtomicBool = AtomicBool::new(false);

lazy_static! {
    static ref FILE_CACHE: RwLock<FxHashMap<Id, Vec<String>>> = RwLock::new(FxHashMap::default());
}

impl ErrorList {
    fn print_error(error: Error<'_>) {
        let mut file_cache = FILE_CACHE.write().expect("FILE_CACHE poisoned");
        let es = error.span;

        let input = match file_cache.get(&es.file) {
            Some(input) => input,
            None => {
                let contents = fs::read_to_string(&format!("{}", es.file)).unwrap();
                file_cache.insert(
                    es.file,
                    contents.split('\n').map(|c| c.to_string()).collect(),
                );
                &file_cache[&es.file]
            }
        };

        let (color, label) = match error.level {
            Level::Error => ("\x1b[91m", "error"),     // bright red
            Level::Warning => ("\x1b[93m", "warning"), // bright yellow
        };

        // Print where the error occurred and the message
        eprintln!(
            "{}{}\x1b[0m: {}:{}:{}: \x1b[97m{}\x1b[0m",
            color,
            label,
            es.file,
            es.line + 1,
            es.col,
            error.msg
        );

        // Print the line where the error occurred
        eprintln!("{}", input[es.line as usize]);

        eprintln!("{}{}^\x1b[0m", " ".repeat(es.col as usize), color,);
    }

    pub fn push(error: Error<'_>) {
        if let Level::Error = &error.level {
            HAS_ERRORS.store(true, Ordering::Relaxed);
        }

        Self::print_error(error);
    }

    pub fn has_error() -> bool {
        HAS_ERRORS.load(Ordering::Acquire)
    }
}
