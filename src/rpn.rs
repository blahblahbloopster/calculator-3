use std::{collections::HashMap, fs::File, io::Read, fmt::{Display, Formatter, Debug}};

use regex::Regex;
use rug::{Float, Complex, float::Constant::Pi, ops::Pow};
use crate::{units::{UnitHolder, UnitMathError}, parser::{Command, Infix}, newerunits::{UV, UnitTree, UVError}};

pub struct RPN {
    pub prec: u32,
    pub units: UnitHolder,
    pub stack: Vec<UV>,
    pub vars: HashMap<String, UV>
}

impl RPN {
    pub fn new(prec: u32) -> Self {
        let mut buf = String::new();
        File::open("src/units.txt").unwrap().read_to_string(&mut buf).unwrap();
        let units = UnitHolder::new(buf);

        let mut vars = HashMap::new();
        vars.insert("pi".to_string(), UV { value: Complex::with_val(prec, Pi), unit: UnitTree::dimensionless() });
        vars.insert("e".to_string(), UV { value: Complex::with_val(prec, 1).exp(), unit: UnitTree::dimensionless() });

        RPN { prec, units, stack: vec![], vars }
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
                    crate::parser::Op::Pow => { let u = a.unit.clone(); let v = a.value.pow(b.value); Ok(UV { unit: u, value: v }) }
                }.map_err(|x| EvalError::UnitError(x))?;

                self.stack.push(out);

                Ok(())
            }
            Command::VarAssign(_) => todo!(),
            Command::VarAccess(_) => todo!(),
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
            Command::UnitSet(_) => todo!(),
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
            let u = format!("{:?}", unit);
            if u.len() != 0 {
                out.push(' ');
                out.push_str(u.as_str());
            }
            out.push('\n');
        }

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
                    crate::parser::Op::Pow => todo!(),
                }
            }
            Infix::Num(n) => Ok(n.item.as_uv()),
            Infix::MonoOp(op, value) => {
                let v = self.infix_eval(*value)?;
                match *op {
                    crate::parser::MonoOp::Minus => Ok(UV { value: -v.value, unit: v.unit })
                }
            }
            Infix::FunctionInv(func, args) => todo!(),
            Infix::VarAccess(name) => todo!(),
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
    UnitError(UVError)
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::EmptyStack => write!(f, "empty stack"),
            EvalError::UnitError(e) => write!(f, "{:?}", e),
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
