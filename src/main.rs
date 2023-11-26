use std::{fs::File, io::Read, collections::HashMap, ops::Range, sync::{Mutex, Arc}, cell::UnsafeCell};

use dice::TaggedDiceRoll;
use units::UnitHolder;
use parser::{Command, Infix, Tag};
use rpn::RPN;
use rustyline::{highlight::Highlighter, error::ReadlineError, ConditionalEventHandler, Cmd, EventHandler, KeyEvent, Editor};
use rustyline_derive::{Completer, Helper, Validator, Hinter};

use crate::parser::rpn_parser;

mod parser;
mod rpn;
mod units;
mod dice;

#[derive(Validator, Helper, Completer, Hinter)]
struct Session {
    calculator: RPN
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
        Color::paint(color, range.loc.clone(), out)
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

    fn dice_color(expr: &Tag<TaggedDiceRoll>, out: &mut ModifiableString) {
        match &expr.item {
            TaggedDiceRoll::Simple(d, num) => {
                Color::pnt(Color::Number, num, out)
            }
            TaggedDiceRoll::Constant(n) => Color::pnt(Color::Number, n, out),
            TaggedDiceRoll::Advantage { l_paren, roll, n, r_paren } => {
                Self::dice_color(roll, out)
            }
            TaggedDiceRoll::Sum(a, op, b) => {
                Self::dice_color(a, out);
                Color::pnt(Color::Operator, op, out);
                Self::dice_color(b, out)
            }
            TaggedDiceRoll::Difference(a, op, b) => {
                Self::dice_color(a, out);
                Color::pnt(Color::Operator, op, out);
                Self::dice_color(b, out)
            }
            TaggedDiceRoll::Product(a, op, b) => {
                Self::dice_color(a, out);
                Color::pnt(Color::Operator, op, out);
                Self::dice_color(b, out)
            }
            TaggedDiceRoll::Multi(n, v) => {
                Color::pnt(Color::Operator, n, out);
                Self::dice_color(v, out)
            }
            
            // DiceInfix::Roll(num) => Color::pnt(Color::Number, num, out),
            // DiceInfix::Multi(k, inner) => { Color::pnt(Color::Operator, k, out); Color::dice_color(inner, out) }
            // DiceInfix::Multiply(k, inner) => { Color::pnt(Color::Number, k, out); Color::dice_color(inner, out) }
            // DiceInfix::Advantage(_n, k) => { Color::dice_color(k, out) }
        }
    }

    fn color(c: &Tag<Command>, out: &mut ModifiableString) {
        let color = match &c.item {
            Command::Number(_) => Color::Number,
            Command::Operation(_) => Color::Operator,
            // Command::VarAssign(_) => Color::Variable,
            // Command::VarAccess(_) => Color::Variable,
            Command::Infix(expr) => {
                Color::infix_color(expr, out);
                return;
            }
            Command::Convert(_) => Color::Operator,
            Command::UnitSet(_) => Color::Operator,
            Command::Function(v) => {
                match v {
                    // stack functions
                    rpn::Function::Drop(_) => Color::Operator,
                    rpn::Function::Duplicate => Color::Operator,
                    rpn::Function::Swap => Color::Operator,
                    rpn::Function::Clear => Color::Operator,
                    rpn::Function::Clipboard => Color::Operator,
                    rpn::Function::PrettyPrint => Color::Operator,
                    rpn::Function::VarGet(_) => Color::Variable,
                    rpn::Function::VarSet(_) => Color::Variable,
                    _ => Color::Operator  // math functions
                }
            }
            Command::Comment => Color::Comment,
            Command::Dice(expr) => {
                Color::dice_color(expr, out);
                return;
            },
            Command::DiceProb(expr, comp, thresh) => {
                Color::dice_color(expr, out);
                Color::pnt(Color::Operator, comp, out);
                Color::pnt(Color::Number, thresh, out);
                return;
            },
            Command::DiceHistogram(expr) => {
                Color::dice_color(expr, out);
                return;
            },
            Command::UnitDef(_, _) => {
                Color::Unit
            }
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
            Color::Operator => ansi_term::Color::Yellow,
            Color::Variable => ansi_term::Color::Green,
            Color::Comment => ansi_term::Color::Purple,
        }
    }
}

impl Highlighter for Session {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
        let _ = pos;
        let parsed = rpn_parser::commands(line, &self.calculator);
        let l = match parsed {
            Ok(v) => {
                Color::highlight(&v, line)
            }
            Err(_) => {
                let mut out = line.clone().to_string();
                let mut popped = String::new();
                loop {
                    match out.pop() {
                        Some(v) => { popped.push(v) }
                        None => return std::borrow::Cow::Owned(line.to_string())
                    }
                    let h = self.highlight(out.as_str(), pos).to_string();
                    if h == out {
                        continue;
                    }
                    let style = ansi_term::Color::Red.reverse();
                    return std::borrow::Cow::Owned(h + &style.prefix().to_string() + &popped + &style.suffix().to_string());
                }
            }
        };
        std::borrow::Cow::Owned(l)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        let _ = (line, pos);
        true
    }
}

struct Handler {
    undo: Arc<Mutex<UnsafeCell<bool>>>
}
impl ConditionalEventHandler for Handler {
    fn handle(
            &self,
            _evt: &rustyline::Event,
            _n: rustyline::RepeatCount,
            _positive: bool,
            _ctx: &rustyline::EventContext,
        ) -> Option<rustyline::Cmd> {
            // println!("\n\n\n\n\naiowjafioeiojfjioe\n\n\n\n\n");
            // Some(Cmd::Undo(1))
            // let v = self.undo.lock().unwrap();
            *self.undo.lock().unwrap().get_mut() = true;
            Some(Cmd::Interrupt)
    }
}

fn main() {
    // let roll = DiceRoll::Advantage(Box::new(DiceRoll::Simple(1..=20)), 50);
    // println!("{}", roll.distribution().histogram(80));
    // exit(0);

    let mut r1: Editor<()> = Editor::new().unwrap();

    let prec: u32;
    loop {
        let inp = r1.readline("precision: (1024) ").unwrap().replace("\n", "");
        if inp.is_empty() {
            prec = 1024;
            break;
        } else {
            match inp.parse::<u32>() {
                Ok(v @ 10..=16777216) => {
                    prec = v;
                    break
                }
                _ => println!("Enter a valid number!")
            }
        }
    }
    
    let mut buf = String::new();
    File::open("/home/max/synced/rustcalc-4/units.txt").unwrap().read_to_string(&mut buf).unwrap();
    let holder = UnitHolder::new(buf, prec);

    let rpn = RPN::new(prec, holder);
    let session = Session { calculator: rpn };
    let mut editor = rustyline::Editor::new().unwrap();
    editor.set_helper(Some(session));

    let undo = Arc::new(Mutex::new(UnsafeCell::new(false)));
    editor.bind_sequence(
        KeyEvent::ctrl('z'),
        EventHandler::Conditional(Box::new(Handler { undo: Arc::clone(&undo) })),
    );

    loop {
        println!("{}", editor.helper().unwrap().calculator.print_stack());
        let res = editor.readline("> ");
        let line = match res {
            Ok(l) => l,
            Err(ReadlineError::Interrupted) => {
                if unsafe { *undo.lock().unwrap().get() } {
                    if !editor.helper_mut().unwrap().calculator.restore_last() {
                        print!("undo buffer empty");
                    }
                    *undo.lock().unwrap().get_mut() = false
                }
                continue;
            },
            Err(ReadlineError::Eof) => {
                return;
            },
            Err(_) => {
                continue;
            }
        };
        editor.add_history_entry(line.clone());

        let parsed = rpn_parser::commands(&line, &editor.helper().unwrap().calculator);
        match parsed {
            Ok(v) => {
                if !v.is_empty() {
                    editor.helper_mut().unwrap().calculator.undo_checkpoint();
                }
                for cmd in v {
                    // match &cmd.item {
                    //     Command::Dice(v) => {
                    //         let distr = v.item.as_roll().distribution();
                    //         let expected = distr.expected_value();
                    //         println!("========\n{}expected value:{}\n========", distr.histogram(80), expected)
                    //     }
                    //     _ => {}
                    // }
                    let res = editor.helper_mut().unwrap().calculator.run(cmd.item);
                    match res {
                        Ok(_) => {}
                        Err(e) => {
                            println!("  {}^", " ".repeat(cmd.loc.start));
                            println!("{}", e);
                            editor.helper_mut().unwrap().calculator.restore_last();
                            break;
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
    }
}
