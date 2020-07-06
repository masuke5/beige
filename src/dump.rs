pub const INDENT_SIZE: usize = 2;

pub fn print_indent(level: usize) {
    print!("{}", " ".repeat(level * INDENT_SIZE));
}
