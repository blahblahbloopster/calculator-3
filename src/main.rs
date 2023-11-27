use std::{fs::File, io::Read, collections::HashMap, ops::Range, sync::{Mutex, Arc}, cell::UnsafeCell, env::{self, args}};

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
    
    // let mut buf = String::new();
    // let unit_file = env::var("UNIT_FILE").unwrap_or(args().into_iter().collect::<Vec<_>>().get(0).map(|x| x.clone()).unwrap_or("units.txt".to_string()));
    // File::open(unit_file).unwrap().read_to_string(&mut buf).unwrap();
    let buf = "# base units
second / s / sec (prefixes)
kilogram / kg
mole / mol (prefixes)
candela / cd (prefixes)
kelvin / K (prefixes)
ampere / amp / A (prefixes)
meter / m (prefixes)
radian / rad (prefixes)
*bit / shannon / Sh / b (prefixes)

hertz / Hz: 1 s^-1 (prefixes)
steradian / sr (prefixes)

*jim / j (prefixes)
slimjim / sj: 0.1 jim (prefixes)

byte / B: 8 bit (prefixes)

# derived (metric)
hertz / Hz: 1 s^-1 (prefixes)
*c: 299792458 m/s

minute / min: 60 s
hour / h: 60 min
*day: 24 h

nanogram / ng: 0.000000000001 kg
microgram / ug / μg: 0.000000001 kg
milligram / mg: 0.000001 kg
gram / g: 0.001 kg
*metricton / ton / t: 1000 kg

litre / liter / L: 1000 cm^3 (prefixes)
molar / M: 1 mole/liter (prefixes)
newton / N: 1 kg*m/s^2 (prefixes)
pascal / Pa: 1 N/m^2 (prefixes)
joule / J: 1 N*m (prefixes)
watt / W: 1 J/s (prefixes)
coulomb / C: 1 A/s (prefixes)
volt / V: 1 J/C (prefixes)
weber / Wb: 1 V*s (prefixes)
tesla / T: 1 Wb/m^2 (prefixes)
farad / F: 1 C/V (prefixes)
*ohm: 1 V/A (prefixes)
siemens / S: 1 A/V (prefixes)
henry / H: 1 V*s/A (prefixes)
becquerel / Bq: 1 Hz (prefixes)
gray / Gy: 1 J/kg (prefixes)
sievert / Sv: 1 J/kg (prefixes)
katal / kat: 1 mol/s (prefixes)
micron: 1 um
*bar: 100 kPa (prefixes)
# (conflicts with radian) rad: 0.1 mGy (prefixes)
*rem: 0.01 Sv
watthour / Wh: 1 W*h (prefixes)
calorie / scical / cal: 4.184 J (prefixes)
lumen / lm: 1 cd/sr (prefixes)
*lux / lz: 1 lm / m^2 (prefixes)
hectare / ha: 10000 m^2

kilotontnt / kt: 4.184 TJ
megatontnt / Mt: 1000 kilotontnt
gigatontnt / Gt: 1000 megatontnt


# derived (imperial)
inch / in: 2.54 cm
foot / ft: 12 in
yard / yd: 3 ft
mile / mi: 5280 ft
*mil / thou: 0.001 in
league: 5280 yd
fathom: 2 yd
cable: 120 fathom
nauticalmile / nmi: 1852 m
# no, I'm not adding the other moronic imperial area measures
acre:  4046.8564224 m^2


# these are the US liquid ones, I'm not adding any others, fuck you
minim: 61.611519921875 μL
fluiddram / fldr: 60 minim
teaspoon / tsp: 80 minim
tablespoon / tbsp: 3 tsp
fluidounce / floz: 2 tbsp
shot / jig: 1.5 floz
gill: 4 floz
*cup: 2 gill
pint / pt: 2 cup
quart / qt: 2 pint
pottle / pot: 2 quart
usflgallon / gallon / gal: 4 quart
# except these
usdrygallon / drygal: 4.40488377086 L
imperialgallon: 4.54609 L
barrel: 31.5 gal
oilbarrel: 42 gal
hogshead: 63 gal
# crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy. crazy? I was crazy once. they locked me in a room. a rubber room. a rubber room with rats. and rats make me crazy.

# imperial mass/weight/whatever the fuck
grain / gr: 64.79891 mg
dram / dr: 1.7718451953125 g
ounce / oz: 16 dr
pound / lb / lbs: 16 oz
hundredweight / cwt: 100 lb
uston / shortton: 2000 lb
longton: 2240 lb
troygrain: 64.79891 mg
pennyweight: 1.55517384 g
troyounce / ozt: 31.1034768 g
troypound / lbt: 373.2417216 g
# my soul has left my body

poundforce / lbf: 4.4482216152605 N
poundspersquareinch / psi: 1 lbf / in^2

# derived (other)
furlong / furlongs: 660 ft
parsec / parsecs: 30856775814913673 m
lightyear / ly: 9460730472580800 m
smoot / smoots: 1.702 m
barn / barns: 0.0000000000000000000000000001 m^2 (prefixes)
outhouse: 0.000001 barn (prefixes)
shed: 0.000000000000000000000001 barn (prefixes)
nanoacre: 4.0468564224 mm^2
shake: 10 ns (prefixes)
degree / deg: 2*pi/360 rad
arcminute / arcmin: 1/60 deg
arcsecond / arcsec: 1/60 arcmin
rotation / rot: 2*pi rad

*gee: 9.80665 m/s^2
";
    let holder = UnitHolder::new(buf.to_string(), prec);

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
