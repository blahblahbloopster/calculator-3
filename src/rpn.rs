use std::{collections::HashMap, fmt::{Display, Formatter, Debug}};

use regex::Regex;
use rug::{Float, Complex, float::Constant::Pi, ops::Pow};
use crate::{parser::{Command, Infix}, units::{UV, UnitTree, UVError, UnitHolder}};

pub struct RPN {
    pub prec: u32,
    pub units: UnitHolder,
    pub stack: Vec<UV>,
    pub vars: HashMap<String, UV>,
    pub undo_checkpoints: Vec<UndoCheckpoint>
}

pub struct UndoCheckpoint {
    pub stack: Vec<UV>,
    pub vars: HashMap<String, UV>
}

impl UndoCheckpoint {
    // pain
    fn size(&self) -> usize {
        self.stack.iter().map(|x| (x.value.prec().0 / 8 + x.value.prec().1 / 8) as usize).sum::<usize>() + self.vars.values().map(|x| (x.value.prec().0 / 8 + x.value.prec().1 / 8) as usize).sum::<usize>()
    }
}

impl RPN {
    pub fn new(prec: u32, units: UnitHolder) -> Self {
        let mut vars = HashMap::new();
        vars.insert("pi".to_string(), UV { value: Complex::with_val(prec, Pi), unit: UnitTree::dimensionless() });
        vars.insert("e".to_string(), UV { value: Complex::with_val(prec, 1).exp(), unit: UnitTree::dimensionless() });

        RPN { prec, units, stack: vec![], vars, undo_checkpoints: vec![] }
    }

    fn undo_size(&self) -> usize {
        self.undo_checkpoints.iter().map(|x| x.size()).sum()
    }

    fn limit_undo_size(&mut self) {
        let threshold = 134217728;
        while self.undo_size() > threshold && !self.undo_checkpoints.is_empty() {
            self.undo_checkpoints.remove(0);
        }
    }

    pub fn undo_checkpoint(&mut self) {
        let c = UndoCheckpoint {
            stack: self.stack.clone(),
            vars: self.vars.clone(),
        };
        self.undo_checkpoints.push(c);
        self.limit_undo_size();
    }

    pub fn restore(&mut self, c: UndoCheckpoint) {
        self.stack = c.stack;
        self.vars = c.vars;
    }

    pub fn restore_last(&mut self) -> bool {
        match self.undo_checkpoints.pop() {
            Some(v) => { self.restore(v); true },
            None => false
        }
    }

    fn pop(&mut self, n: usize) -> Result<Vec<UV>, EvalError> {
        if n > self.stack.len() {
            return Err(EvalError::EmptyStack);
        }
        let mut out = vec![];
        for _ in 0..n {
            out.push(self.stack.pop().unwrap());
        }
        out.reverse();
        Ok(out)
    }

    fn pop_single(&mut self) -> Result<UV, EvalError> {
        if self.stack.is_empty() {
            return Err(EvalError::EmptyStack);
        }
        Ok(self.stack.pop().unwrap())
    }

    pub fn run(&mut self, command: Command) -> Result<(), EvalError> {
        match command {
            Command::Number(v) => { self.stack.push(v.item.as_uv()); Ok(()) }
            Command::Operation(op) => {
                let args = self.pop(2)?;
                // TODO: don't clone
                let a = args[0].clone();
                let b = args[1].clone();

                let out = match *op {
                    crate::parser::Op::Plus => a + b,
                    crate::parser::Op::Minus => a - b,
                    crate::parser::Op::Times => Ok(a * b),
                    crate::parser::Op::Divide => a / b,
                    crate::parser::Op::Pow => { let u = a.unit.clone(); let v = a.value.pow(b.value); Ok(UV { unit: u, value: v }) }  // TODO: exponeate units
                }.map_err(|x| EvalError::UnitError(x))?;

                self.stack.push(out);

                Ok(())
            }
            Command::Function(v) => {
                v.eval(self)
            }
            Command::Infix(e) => {
                let out = self.infix_eval(e.item)?;
                self.stack.push(out);
                Ok(())
            }
            Command::Convert(dst) => {
                let v = self.pop(1)?[0].clone();
                let converted = v.convert(dst.item).map_err(|x| EvalError::UnitError(x))?;
                self.stack.push(converted);
                Ok(())
            }
            Command::UnitSet(v) => {
                let popped = self.pop_single()?;
                self.stack.push(UV { unit: v.item, value: popped.value });
                Ok(())
            }
            Command::Comment => Ok(())
        }
    }

    // fn print_u_value(u: &UV) -> String {
    //     match &u.unit {
    //         crate::units::Unit::Base(v) => format!("{} {}", PComplex(&u.value), v),
    //         crate::units::Unit::Derived { top, bottom, multiplier } => {
    //             let mut counts = vec![];

    //             for item in top {
    //                 let mut idx = None;
    //                 for (i, (unit, _)) in counts.iter().enumerate() {
    //                     if unit == &item {
    //                         idx = Some(i);
    //                         break;
    //                     }
    //                 }
    //                 let i = match idx {
    //                     Some(v) => v,
    //                     None => { counts.push((item, 0)); counts.len() - 1 }
    //                 };

    //                 counts[i].1 += 1;
    //             }

    //             for item in bottom {
    //                 let mut idx = None;
    //                 for (i, (unit, _)) in counts.iter().enumerate() {
    //                     if unit == &item {
    //                         idx = Some(i);
    //                         break;
    //                     }
    //                 }
    //                 let i = match idx {
    //                     Some(v) => v,
    //                     None => { counts.push((item, 0)); counts.len() - 1 }
    //                 };

    //                 counts[i].1 -= 1;
    //             }

    //             todo!()
    //         }
    //     }
    // }

    pub fn print_stack(&self) -> String {
        let mut out = String::new();

        for item in &self.stack {
            let value = &item.value;
            let unit = &item.unit;

            out.push_str(PComplex(value).to_string().as_str());
            let u = format!("{}", unit);
            if u.len() != 0 {
                out.push(' ');
                out.push_str(u.as_str());
            }
            out.push('\n');
        }
        
        out.pop();

        out
    }

    pub fn infix_eval(&mut self, expr: Infix) -> Result<UV, EvalError> {
        match expr {
            Infix::BiOp(lv, op, rv) => {
                let l = self.infix_eval(*lv)?;
                let r = self.infix_eval(*rv)?;
                match *op {
                    crate::parser::Op::Plus => (l + r).map_err(|x| EvalError::UnitError(x)),
                    crate::parser::Op::Minus => (l - r).map_err(|x| EvalError::UnitError(x)),
                    crate::parser::Op::Times => Ok(l * r),
                    crate::parser::Op::Divide => (l / r).map_err(|x| EvalError::UnitError(x)),
                    crate::parser::Op::Pow => { let u = l.unit.clone(); let v = l.value.pow(r.value); Ok(UV { unit: u, value: v }) }  // TODO: exponeate units
                }
            }
            Infix::Num(n) => Ok(n.item.as_uv()),
            Infix::MonoOp(op, value) => {
                let v = self.infix_eval(*value)?;
                match *op {
                    crate::parser::MonoOp::Minus => Ok(UV { value: -v.value, unit: v.unit })
                }
            }
            Infix::FunctionInv(func, args) => {
                let found = Function::from_string(func.item.clone()).ok_or(EvalError::FunctionNotFound(func.item))?;
                let exp = match found.num_args() {
                    Some(v) => v,
                    None => return Err(EvalError::IllegalFunction(found))
                };
                
                if exp != args.len() {
                    return Err(EvalError::WrongArgs { expected: exp, found: args.len() });
                }

                let mut a = vec![];

                for item in args {
                    a.push(self.infix_eval(item)?)
                }
                
                found.eval_normal(a, self)
            }
            Infix::VarAccess(name) => {
                match self.vars.get(&name.item) {
                    Some(v) => Ok(v.clone()),
                    None => Err(EvalError::VarNotFound(name.item.clone())),
                }
            }
        }
    }
}

impl Debug for RPN {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RPN").field("prec", &self.prec).field("stack", &self.stack).field("vars", &self.vars).finish()
    }
}

#[derive(Debug)]
pub enum EvalError {
    EmptyStack,
    UnitError(UVError),
    VarNotFound(String),
    FunctionNotFound(String),
    WrongArgs { expected: usize, found: usize },
    IllegalFunction(Function),
    UnimplementedError
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::EmptyStack => write!(f, "empty stack"),
            EvalError::UnitError(e) => write!(f, "{}", e),
            EvalError::VarNotFound(n) => write!(f, "variable '{}' not found", n),
            EvalError::FunctionNotFound(n) => write!(f, "function '{}' not found", n),
            EvalError::WrongArgs { expected, found } => write!(f, "expected {} args, found {}", expected, found),
            EvalError::IllegalFunction(func) => write!(f, "function {:?} can't be used in this context", func),
            EvalError::UnimplementedError => write!(f, "not yet implemented")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function {
    Sin, Cos, Tan, Cot,
    ASin, ACos, ATan, ATan2, ACot,
    Sqrt,
    Ln, Log10, Log2, LogB,
    Drop(usize), Duplicate, Swap, Clear, Clipboard, PrettyPrint, Undo,
    VarGet(String), VarSet(String)
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Function {
    fn num_args(&self) -> Option<usize> {
        match self {
            Function::Sin => Some(1),
            Function::Cos => Some(1),
            Function::Tan => Some(1),
            Function::Cot => Some(1),
            Function::ASin => Some(1),
            Function::ACos => Some(1),
            Function::ATan => Some(1),
            Function::ATan2 => Some(1),
            Function::ACot => Some(1),
            Function::Sqrt => Some(1),
            Function::Ln => Some(1),
            Function::Log10 => Some(1),
            Function::Log2 => Some(1),
            Function::LogB => Some(2),

            Function::Drop(_) => None,
            Function::Duplicate => None,
            Function::Swap => None,
            Function::Clear => None,
            Function::Clipboard => None,
            Function::PrettyPrint => None,
            Function::Undo => None,
            Function::VarGet(_) => None,
            Function::VarSet(_) => None,
        }
    }

    fn eval_normal(self, args: Vec<UV>, calc: &RPN) -> Result<UV, EvalError> {
        match self.num_args() {
            Some(v) => {
                if v != args.len() {
                    return Err(EvalError::WrongArgs { expected: v, found: args.len() });
                }
            }
            None => panic!("stupid.")
        }
        let arg = args[0].clone();
        match self {
            Function::Sin => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.sin() }),  // TODO: circle units
            Function::Cos => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.cos() }),
            Function::Tan => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.tan() }),
            Function::Cot => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.tan().recip() }),
            Function::ASin => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.asin() }),
            Function::ACos => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.acos() }),
            Function::ATan => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.atan() }),
            Function::ACot => Err(EvalError::UnimplementedError),
            Function::Sqrt => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.sqrt() }),  // TODO: unsquare
            Function::Ln => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.ln() }),
            Function::Log10 => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.log10() }),
            Function::Log2 => Ok(UV { unit: UnitTree::dimensionless(), value: arg.value.ln() / Complex::with_val(calc.prec, 2).ln() }),
            Function::LogB => { let a = arg; let b = args[1].clone(); let res = a.value.ln() / b.value.ln(); Ok(UV { unit: a.unit, value: res }) }
            Function::ATan2 => Err(EvalError::UnimplementedError),

            _ => Err(EvalError::IllegalFunction(self))  // this is so fucking stupid holy shit
        }
    }

    fn eval(self, calc: &mut RPN) -> Result<(), EvalError> {
        match self {
            Function::Drop(n) => { let _ = calc.pop(n); Ok(()) },
            Function::Duplicate => { let v = calc.pop_single()?; calc.stack.push(v.clone()); calc.stack.push(v); Ok(()) },
            Function::Swap => { let mut items = calc.pop(2)?; items.reverse(); calc.stack.extend(items); Ok(()) },  // TEST ME
            Function::Clear => { calc.stack.clear(); Ok(()) }
            Function::Clipboard => Err(EvalError::UnimplementedError),
            Function::PrettyPrint => {
                let mut mapping = Vec::new();
                macro_rules! ins {
                    ($($n:literal $s:ident)+) => {
                        $(mapping.push(($n, stringify!($s).to_string().to_lowercase())));+
                    };
                }

                ins!(
                    2    Hundred
                    3    Thousand
                    6    Million
                    9    Billion
                    12   Trillion
                    15   Quadrillion
                    18   Quintillion
                    21   Sextillion
                    24   Septillion 
                    24   Septillion 
                    30   Nonillion
                    33   Decillion 
                    36   Undecillion 
                    39   Duodecillion 
                    42   Tredecillion 
                    45   Quattuordecillion 
                    48   Quindecillion 
                    51   Sedecillion 
                    54   Septendecillion
                    57   Octodecillion 
                    60   Novendecillion 
                    63   Vigintillion 
                    66   Unvigintillion 
                    69   Duovigintillion 
                    72   Tresvigintillion 
                    75   Quattuorvigintillion 
                    78   Quinvigintillion 
                    81   Sesvigintillion 
                    84   Septemvigintillion 
                    87   Octovigintillion 
                    90   Novemvigintillion 
                    93   Trigintillion 
                    96   Untrigintillion 
                    99   Duotrigintillion 
                    102  Trestrigintillion 
                    105  Quattuortrigintillion 
                    108  Quintrigintillion 
                    111  Sestrigintillion 
                    114  Septentrigintillion 
                    117  Octotrigintillion 
                    120  Noventrigintillion 
                    123  Quadragintillion 
                    153  Quinquagintillion 
                    183  Sexagintillion 
                    213  Septuagintillion 
                    243  Octogintillion 
                    273  Nonagintillion 
                    303  Centillion 
                    306  Uncentillion 
                    333  Decicentillion 
                    336  Undecicentillion 
                    363  Viginticentillion 
                    366  Unviginticentillion 
                    393  Trigintacentillion 
                    423  Quadragintacentillion 
                    453  Quinquagintacentillion 
                    483  Sexagintacentillion 
                    513  Septuagintacentillion 
                    543  Octogintacentillion 
                    573  Nonagintacentillion 
                    603  Ducentillion 
                    903  Trecentillion 
                    1203 Quadringentillion 
                    1503 Quingentillion 
                    1803 Sescentillion 
                    2103 Septingentillion 
                    2403 Octingentillion 
                    2703 Nongentillion 
                    3003 Millinillion
                );

                let item = calc.stack.last().ok_or(EvalError::EmptyStack)?;

                let mut out = String::new();
                out.push_str(PComplex(&item.value).pretty_print(&mapping).as_str());
                let u = format!("{}", item.unit);
                if u.len() != 0 {
                    out.push(' ');
                    out.push_str(u.as_str());
                }

                println!("{}\n", out);

                Ok(())
            }
            Function::VarGet(name) => {
                match calc.vars.get(&name) {
                    Some(v) => { calc.stack.push(v.clone()); Ok(()) }
                    None => Err(EvalError::VarNotFound(name))
                }
            }
            Function::VarSet(name) => {
                let arg = calc.pop_single()?;
                calc.vars.insert(name, arg);
                Ok(())
            }
            _ => {
                let args = calc.pop(self.num_args().expect("you brought this on yourself"))?;
                calc.stack.push(self.eval_normal(args, calc)?);
                Ok(())
            }
        }
    }

    pub fn from_string(inp: String) -> Option<Function> {
        match inp.as_str() {
            "sin" => Some(Function::Sin),
            "cos" => Some(Function::Cos),
            "tan" => Some(Function::Tan),
            "cot" => Some(Function::Cot),

            "asin" => Some(Function::ASin),
            "acos" => Some(Function::ACos),
            "atan" => Some(Function::ATan),
            "atan2" => Some(Function::ATan2),
            "acot" => Some(Function::ACot),

            "sqrt" => Some(Function::Sqrt),

            "ln" => Some(Function::Ln),
            "log10" => Some(Function::Log10),
            "log2" => Some(Function::Log2),
            "logb" => Some(Function::LogB),

            "d" => Some(Function::Duplicate),
            "s" => Some(Function::Swap),
            "clear" => Some(Function::Clear),
            "clipboard" => Some(Function::Clipboard),
            "pretty" => Some(Function::PrettyPrint),
            _ => {
                if inp.starts_with("=") {
                    Some(Function::VarSet(inp.strip_prefix("=").unwrap().to_string()))
                } else if inp.chars().all(|x| x == 'r') {
                    Some(Function::Drop(inp.len()))
                } else {
                    Some(Function::VarGet(inp))
                }
            }
        }
    }
}

struct PFloat<'a>(&'a Float);
pub struct PComplex<'a>(pub &'a Complex);

impl<'a> Display for PFloat<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let trailing_zeros: Regex = Regex::new("\\.?0+$").unwrap();
        let trailing_zeros_exp: Regex = Regex::new("\\.?0+e").unwrap();
        let count = (self.0.prec() as f64 / 10.0_f64.log2()) as usize + 2;
        let (sign, digits, exp) = self.0.to_sign_string_exp(10, Some(count - 2));
        if exp.unwrap_or(0).abs() > 500 {
            return write!(f, "{}", trailing_zeros_exp.replace(self.0.to_string_radix(10, Some(count - 2)).as_str(), "e"));
        }
        let point_location = digits.len() as i32 - exp.unwrap_or(0) + 1;
        let string = match point_location {
            x if x <= 0 as i32 => {
                let zeros = (-x) as usize + 1;
                let mut d = digits.clone();
                for _ in 0..zeros {
                    d.push('0');
                }
                d.push_str(".0");
                d
            }
            x if x > digits.len() as i32 => {
                let count = point_location as usize - digits.len();
                let mut output = String::new();
                output.push_str("0.");
                for _ in 0..count - 1 {
                    output.push('0');
                }
                output.push_str(&*digits);
                output
            }
            _ => {
                let mut d = digits.clone();
                d.insert(exp.unwrap() as usize, '.');
                d
            }
        };
        let s = if sign { "-" } else { "" };
        write!(f, "{}{}", s, &*trailing_zeros.replace(string.as_str(), ""))
    }
}

impl<'a> PFloat<'a> {
    fn pretty_print(&self, mapping: &Vec<(i32, String)>) -> String {
        let count = (self.0.prec() as f64 / 10.0_f64.log2()) as usize + 2;
        let (_, _, Some(exp)) = self.0.to_sign_string_exp(10, Some(count - 2)) else { return format!("{}", self) };
        let mut found = None;
        for arr in mapping.iter().collect::<Vec<_>>().windows(2) {
            let a = arr[0];
            let b = arr[1];
            if b.0 > exp - 1 {
                found = Some(a);
                break;
            }
        }

        match found {
            Some((e, n)) => {
                let pow = Float::with_val(self.0.prec(), 10).pow(e);
                format!("{} {}", PFloat(&(self.0 / pow)), n)
            }
            None => {
                if exp < 3 {
                    format!("{}", self)
                } else {
                    let biggest = mapping.last().unwrap();
                    let pow = Float::with_val(self.0.prec(), 10).pow(biggest.0);
                    format!("{} {}", PFloat(&(self.0 / pow)), biggest.1)
                }
            }
        }
    }
}

impl<'a> Display for PComplex<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.imag().is_zero() {
            write!(f, "{}", PFloat(self.0.real()))
        } else if self.0.real().is_zero() {
            let img = self.0.imag();
            if img.is_integer() && img.to_f64() == 1.0 {
                f.write_str("i")
            } else {
                write!(f, "{}", PFloat(self.0.imag()))?;
                f.write_str("i")
            }
        } else {
            write!(f, "{}", PFloat(self.0.real()))?;
            f.write_str(" + ")?;
            write!(f, "{}", PFloat(self.0.imag()))?;
            f.write_str("i")
        }
    }
}

impl<'a> PComplex<'a> {
    fn pretty_print(&self, mapping: &Vec<(i32, String)>) -> String {
        if self.0.imag().is_zero() {
            format!("{}", PFloat(self.0.real()).pretty_print(mapping))
        } else if self.0.real().is_zero() {
            let img = self.0.imag();
            if img.is_integer() && img.to_f64() == 1.0 {
                "i".to_string()
            } else {
                format!("{}i", PFloat(self.0.imag()).pretty_print(mapping))
            }
        } else {
            format!("{} + {}i", PFloat(self.0.real()).pretty_print(mapping), PFloat(self.0.imag()).pretty_print(mapping))
        }
    }
}
