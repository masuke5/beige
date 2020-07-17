use std::fmt;

pub const INDENT_SIZE: usize = 2;

pub struct FormatIter<'a, I, V>(I, &'a str)
where
    I: IntoIterator<Item = V> + Clone,
    V: fmt::Display;

impl<'a, I, V> fmt::Display for FormatIter<'a, I, V>
where
    I: IntoIterator<Item = V> + Clone,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(iter, delimiter) = self;

        let mut iter = iter.clone().into_iter();
        let first = iter.next();

        if let Some(first) = first {
            write!(f, "{}", first)?;
            for value in iter {
                write!(f, "{}{}", delimiter, value)?;
            }
        }

        Ok(())
    }
}

pub fn format_iter<I, V>(iter: I, delimiter: &str) -> FormatIter<I, V>
where
    I: IntoIterator<Item = V> + Clone,
    V: fmt::Display,
{
    FormatIter(iter, delimiter)
}

pub fn print_indent(level: usize) {
    print!("{}", " ".repeat(level * INDENT_SIZE));
}
