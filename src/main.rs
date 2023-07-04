use std::{fs::File, io::{Read, stdout, Write}, collections::HashMap, ops::Range};

use parser::{Command, Infix, Tag};
// use newunits::blah;
use rpn::RPN;
use rug::{Float, Complex};
use rustyline::{hint::Hinter, highlight::Highlighter, error::ReadlineError};
use rustyline_derive::{Completer, Helper, Validator, Highlighter, Hinter};
use units::{Unit, unit_parser, UnitHolder};

use crate::{units::UnitValue, parser::rpn_parser};

mod parser;
mod units;
mod rpn;
mod newunits;
mod newerunits;

#[derive(Validator, Helper, Completer, Hinter)]
struct Session<'a> {
    calculator: RPN,
    units: &'a UnitHolder
}

struct ModifiableString {
    original: String,
    modifications: HashMap<usize, String>
}

impl ModifiableString {
    fn string(&self) -> String {
        let mut output = String::new();
        for (i, c) in self.original.chars().enumerate() {
            let addition = self.modifications.get(&i);
            match addition {
                Some(v) => output.push_str(v.as_str()),
                None => {}
            };
            output.push(c);
        }
        match self.modifications.get(&self.original.len()) {
            Some(v) => output.push_str(v.as_str()),
            None => {}
        };
        output
    }

    fn before(&mut self, index: usize, value: String) {
        match self.modifications.get_mut(&index) {
            Some(v) => { v.push_str(value.as_str()) }
            None => { self.modifications.insert(index, value); }
        }
    }

    fn after(&mut self, index: usize, value: String) {
        match self.modifications.get_mut(&index) {
            Some(v) => { v.insert_str(0, value.as_str()) }
            None => { self.modifications.insert(index, value); }
        }
    }

    fn new(original: String) -> ModifiableString {
        ModifiableString { original, modifications: HashMap::new() }
    }
}

enum Color {
    Number, Unit, Operator, Variable, Comment
}

impl Color {
    fn paint(color: Color, range: Range<usize>, out: &mut ModifiableString) {
        let col = color.col();
        let before = col.prefix().to_string();
        let after = col.suffix().to_string();

        out.before(range.start, before);
        out.after(range.end, after);
    }

    fn pnt<T>(color: Color, range: &Tag<T>, out: &mut ModifiableString) {
        let col = color.col();
        let before = col.prefix().to_string();
        let after = col.suffix().to_string();

        out.before(range.loc.start, before);
        out.after(range.loc.end, after);
    }

    fn infix_color(expr: &Infix, out: &mut ModifiableString) {
        match expr {
            Infix::BiOp(l, o, r) => {
                Color::infix_color(l, out);
                Color::infix_color(r, out);
                Color::pnt(Color::Operator, o, out)
            }
            Infix::Num(v) => {
                match &v.item {
                    parser::PUnitValue::UnV { unit, value } => {
                        Color::pnt(Color::Unit, unit, out);
                        Color::pnt(Color::Number, value, out);
                    }
                    parser::PUnitValue::Dimensionless { value } => {
                        Color::pnt(Color::Number, value, out);
                    }
                }
            }
            Infix::MonoOp(op, expr) => {
                Color::infix_color(expr, out);
                Color::pnt(Color::Operator, op, out);
            }
            Infix::FunctionInv(ident, args) => {
                for arg in args {
                    Color::infix_color(arg, out);
                }
                Color::pnt(Color::Operator, ident, out);
            }
            Infix::VarAccess(v) => {
                Color::pnt(Color::Variable, v, out)
            }
        }
    }

    fn color(c: &Tag<Command>, out: &mut ModifiableString) {
        let color = match &c.item {
            Command::Number(v) => Color::Number,
            Command::Operation(_) => Color::Operator,
            Command::VarAssign(_) => Color::Variable,
            Command::VarAccess(_) => Color::Variable,
            Command::Infix(expr) => {
                Color::infix_color(expr, out);
                return;
            }
            Command::Convert(_) => Color::Operator,
            Command::UnitSet(_) => Color::Operator,
        };
        Color::pnt(color, c, out);
    }

    pub fn highlight(commands: &Vec<Tag<Command>>, inp: &str) -> String {
        let mut out = ModifiableString::new(inp.to_string());
        for cmd in commands {
            Color::color(cmd, &mut out);
        }
        out.string()
    }

    fn col(&self) -> ansi_term::Color {
        match self {
            Color::Number => ansi_term::Color::Blue,
            Color::Unit => ansi_term::Color::Blue,
            Color::Operator => ansi_term::Color::Green,
            Color::Variable => ansi_term::Color::Green,
            Color::Comment => ansi_term::Color::Purple,
        }
    }
}

impl<'a> Highlighter for Session<'a> {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
        let _ = pos;
        let parsed = rpn_parser::commands(line, self.calculator.prec, self.units);
        let l = match parsed {
            Ok(v) => {
                Color::highlight(&v, line)
            }
            Err(v) => line.to_string(),  // FIXME
        };
        std::borrow::Cow::Owned(l)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        let _ = (line, pos);
        true
    }
}

fn main() {
    // blah()
    let mut buf = String::new();
    File::open("src/units.txt").unwrap().read_to_string(&mut buf).unwrap();
    let holder = UnitHolder::new(buf);

    // let unit1 = unit_parser::unit("km", &holder).unwrap();
    // let unit2 = unit_parser::unit("inch", &holder).unwrap();

    // let inp = UnitValue { value: Complex::with_val(1024, 10000), unit: unit1 };
    // println!("{:?}", inp.convert(unit2));


    let rpn = RPN::new(1024);
    let session = Session { calculator: rpn, units: &holder };
    let mut editor = rustyline::Editor::new().unwrap();
    editor.set_helper(Some(session));

    loop {
        let line = match editor.readline("> ") {
            Ok(l) => l,
            Err(ReadlineError::Interrupted) => {
                continue;
            },
            Err(ReadlineError::Eof) => {
                return;
            },
            Err(_) => {
                continue;
            }
        };

        let parsed = rpn_parser::commands(&line, 1024, &holder);
        match parsed {
            Ok(v) => {
                for cmd in v {
                    let res = editor.helper_mut().unwrap().calculator.run(cmd.item);
                    match res {
                        Ok(_) => {}
                        Err(e) => {
                            println!("  {}^", " ".repeat(cmd.loc.start));
                            println!("{}", e);
                            break;  // TODO: restore state
                        }
                    }
                }
            }
            Err(e) => {
                if e.location.line == 1 {
                    println!("{}^", " ".repeat(e.location.column));
                }
                println!("expected one of {}", e.expected);
                continue;
            }
        }
        println!("{}", editor.helper().unwrap().calculator.print_stack());
    }
}
