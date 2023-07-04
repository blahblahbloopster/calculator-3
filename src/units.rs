use std::{ops::{Mul, Div, Add, Sub}, collections::HashMap, fs::File, fmt::Display};

use rug::{Float, ops::NegAssign, Complex};

#[derive(Debug, Clone, PartialEq)]
pub enum Unit {
    Base(String),
    Derived { top: Vec<Unit>, bottom: Vec<Unit>, multiplier: Float }
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum BaseUnit {
//     Second, Kilogram, Mole, Candela, Kelvin, Amp, Meter
// }

impl Unit {
    pub fn simplify(&mut self) {
        // TODO: swap self for base if needed
        match self {
            Unit::Base(_) => { }
            Unit::Derived { top, bottom, multiplier } => {
                let mut top_to_remove = vec![];
                let mut bottom_to_remove = vec![];
                for (idx, item) in top.iter().enumerate() {
                    match bottom.iter().enumerate().filter(|x| !bottom_to_remove.contains(&x.0)).find(|x| x.1 == item) {
                        Some((bidx, _)) => { top_to_remove.push(idx); bottom_to_remove.push(bidx) }
                        None => {}
                    }
                }

                top_to_remove.sort(); top_to_remove.reverse();
                for i in top_to_remove {
                    top.remove(i);
                }

                bottom_to_remove.sort(); bottom_to_remove.reverse();
                for i in bottom_to_remove {
                    bottom.remove(i);
                }

                // TODO: bottom reformat as well

                let mut to_reformat = vec![];
                for (idx, item) in top.iter().enumerate() {
                    match item {
                        Unit::Base(_) => {}
                        Unit::Derived { top, bottom, multiplier } => {
                            to_reformat.push(idx);
                        }
                    }
                }

                to_reformat.reverse();

                for idx in to_reformat {
                    let (t, b, m) = match top.remove(idx) { Self::Derived { top, bottom, multiplier } => (top, bottom, multiplier), _ => panic!() };
                    top.extend(t);
                    bottom.extend(b);
                    // *multiplier *= m;  // TODO: check if this is really right
                }


                let mut to_reformat = vec![];
                for (idx, item) in bottom.iter().enumerate() {
                    match item {
                        Unit::Base(_) => {}
                        Unit::Derived { top, bottom, multiplier } => {
                            to_reformat.push(idx);
                        }
                    }
                }

                to_reformat.reverse();

                for idx in to_reformat {
                    let (t, b, m) = match bottom.remove(idx) { Self::Derived { top, bottom, multiplier } => (top, bottom, multiplier), _ => panic!() };
                    top.extend(b);
                    bottom.extend(t);
                    // *multiplier *= m;  // TODO: check if this is really right
                }

                if top.len() == 1 && bottom.len() == 0 {
                    let mut top_item = top[0].clone();
                    std::mem::swap(self, &mut top_item);
                }
            }
        }
    }

    // TODO: restructure to avoid clone
    fn multiplier(&self) -> Float {
        match self {
            Unit::Base(_) => Float::with_val(1024, 1.0),
            Unit::Derived { top, bottom, multiplier } => multiplier.clone(),
        }
    }

    fn compatible(&self, other: &Self) -> bool {
        let mut simp = self.clone();
        simp.simplify();
        let mut simp2 = other.clone();
        simp2.simplify();
        match simp {
            Unit::Base(_) => simp == simp2,
            Unit::Derived { top, bottom, multiplier } => match simp2 {
                Unit::Base(_) => false,
                Unit::Derived { top: t, bottom: b, multiplier: _ } => top == t && bottom == b,
            }
        }
    }

    fn multiply(self, mul: Float) -> Unit {
        match self {
            Unit::Base(_) => Self::Derived { top: vec![self], bottom: vec![], multiplier: mul },
            Unit::Derived { top, bottom, multiplier } => Self::Derived { top, bottom, multiplier: multiplier * mul }
        }
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unit::Base(v) => write!(f, "{}", v),
            Unit::Derived { top, bottom, multiplier } => {
                if top.len() == 0 && bottom.len() == 0 {
                    write!(f, "")
                } else if (multiplier - Float::with_val(1024, 1)).to_f64() < 0.00001 {
                    let mut out = String::new();
                    if top.len() > 1 {
                        out.push('(')
                    }
                    for (index, item) in top.iter().enumerate() {
                        out.push_str(format!("{}", item).as_str());
                        if index != top.len() - 1 {
                            out.push('*')
                        }
                    }
                    if top.len() > 1 {
                        out.push('(')
                    }
                    if top.len() == 0 {
                        out.push('1')
                    }

                    

                    if bottom.len() > 1 {
                        out.push_str(" / ")
                    }
                    for (index, item) in bottom.iter().enumerate() {
                        out.push_str(format!("{}", item).as_str());
                        if index != bottom.len() - 1 {
                            out.push('*')
                        }
                    }
                    if bottom.len() > 1 {
                        out.push('(')
                    }

                    write!(f, "{}", out)
                } else {
                    write!(f, "?")
                }
            }
        }
    }
}

impl Mul for Unit {
    type Output = Unit;

    fn mul(self, rhs: Self) -> Self::Output {
        let multiplier = self.multiplier() * rhs.multiplier();
        let new = Unit::Derived { top: vec![self, rhs], bottom: vec![], multiplier };

        // new.simplify();

        new
    }
}

impl Div for Unit {
    type Output = Unit;

    fn div(self, rhs: Self) -> Self::Output {
        let multiplier = self.multiplier() / rhs.multiplier();
        let new = Unit::Derived { top: vec![self], bottom: vec![rhs], multiplier };

        // new.simplify();

        new
    }
}

#[derive(Debug, Clone)]
pub struct UnitValue {
    pub value: Complex,
    pub unit: Unit
}

impl UnitValue {
    pub fn convert(self, new: Unit) -> Result<UnitValue, UnitMathError> {
        if !self.unit.compatible(&new) { return Err(UnitMathError::MismatchedUnits { left: self.unit.clone(), right: new }); }
        let metricified = self.value * self.unit.multiplier();
        let converted = metricified / new.multiplier();
        Ok(UnitValue { value: converted, unit: new })
    }

    pub fn metricify(self) -> Self {
        match self.unit {
            Unit::Base(_) => self,
            Unit::Derived { top, bottom, multiplier } => UnitValue { value: self.value * multiplier, unit: Unit::Derived { top: top, bottom: bottom, multiplier: Float::with_val(1024, 1) } }
        }
        // UnitValue { value: self.value * self.unit.multiplier(), unit: Unit::Derived { top: self.unit, bottom: (), multiplier: () } }
    }
}

impl Mul for UnitValue {
    type Output = UnitValue;

    fn mul(self, rhs: Self) -> Self::Output {
        let initial = self.unit.multiplier();
        let new_units = self.unit * rhs.unit;
        let factor = new_units.multiplier() / initial;

        UnitValue {
            value: factor * self.value,
            unit: new_units
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnitMathError {
    MismatchedUnits { left: Unit, right: Unit },
    DivisionByZero
}

impl Display for UnitMathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnitMathError::MismatchedUnits { left, right } => {
                let mut l = left.to_string();
                if l.is_empty() {
                    l = "unitless".to_string()
                }

                let mut r = right.to_string();
                if r.is_empty() {
                    r = "unitless".to_string()
                }

                write!(f, "expected {}, found {}", l, r)
            }
            UnitMathError::DivisionByZero => write!(f, "division by zero"),
        }
    }
}

impl Add for UnitValue {
    type Output = Result<UnitValue, UnitMathError>;

    fn add(self, rhs: Self) -> Self::Output {
        if !self.unit.compatible(&rhs.unit) { return Err(UnitMathError::MismatchedUnits { left: self.unit, right: rhs.unit }) }

        Ok(UnitValue { value: self.value + rhs.value * rhs.unit.multiplier() / self.unit.multiplier(), unit: self.unit })
    }
}

impl Sub for UnitValue {
    type Output = Result<UnitValue, UnitMathError>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + UnitValue { value: -rhs.value, unit: rhs.unit }
    }
}

impl Div for UnitValue {
    type Output = Result<UnitValue, UnitMathError>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.value.eq0() {
            return Err(UnitMathError::DivisionByZero)
        }
        Ok(UnitValue { value: self.value / rhs.value, unit: self.unit / rhs.unit })
    }
}

#[derive(Debug)]
pub struct UnitHolder {
    units: Vec<ParsedUnit>
}

impl UnitHolder {
    pub fn single_from_string(&self, inp: &str) -> Option<Unit> {
        for item in &self.units {
            for subitem in &item.names {
                if subitem == inp {
                    return Some(item.unit.clone())
                }
            }
        }
        None
    }

    pub fn new(inp: String) -> UnitHolder {
        let mut out = UnitHolder { units: vec![] };
        inp.lines().filter(|x| !(x.starts_with('#') || x.is_empty())).for_each(|x| { let unit = unit_file_parser::row(x, &mut out).or(Err(format!("error parsing '{}'", x))).unwrap(); out.units.push(unit); });
        out
    }

    pub fn parse(&self, inp: &str) -> Option<Unit> {
        unit_parser::unit(inp, self).ok()?  // this is fine
    }
}

#[derive(Debug)]
pub struct ParsedUnit {
    names: Vec<String>,
    unit: Unit
}

peg::parser! {
    grammar unit_file_parser() for str {
        rule _ = [' ' | '\n']*

        rule num() -> i32
            = _ v:$("-"? ['0'..='9']+) _ {? v.parse().or(Err("number parse error")) }
            
        rule unit(holder: &mut UnitHolder) -> Unit = precedence! {
            x:(@) "*" y:@ { x * y }
            x:(@) "/" y:@ { x / y }
            --
            u:(@) "^" n:num() { let mut unit = Unit::Derived { top: vec![], bottom: vec![], multiplier: Float::with_val(1024, 1) }; if n == 0 { } else if n > 0 { for _ in 0..n { unit = unit * u.clone() } } else { for _ in 0..n { unit = unit * u.clone() } } unit }
            _ u:$(['a'..='z' | 'A'..='Z' | 'μ']+) _ { holder.single_from_string(u).expect(format!("failed to parse '{}'", u).as_str()) }
        }

        rule name() -> &'input str
            = _ v:$(['a'..='z' | 'A'..='Z' | 'μ']+) _ { v }
        
        rule digits() -> &'input str
            = $(['0'..='9']+)

        rule decimal() -> &'input str
            = $(digits() ("." digits())?)
        
        rule float() -> Float
            = _ v:$(decimal() ("e" float())?) _ {? Ok(Float::with_val(1024, Float::parse(v).or(Err("number format error"))?)) }

        pub rule row(holder: &mut UnitHolder) -> ParsedUnit
            = n:name() ++ "/" ":" v:float() u:unit(holder) { ParsedUnit { names: n.iter().map(|x| x.to_string()).collect(), unit: u.clone().multiply(v) } } /
              n:name() ++ "/" { let name = n[0].to_string(); ParsedUnit { names: n.iter().map(|x| x.to_string()).collect(), unit: Unit::Base(name) } }
    }
}

peg::parser! {
    pub grammar unit_parser(holder: &UnitHolder) for str {
        rule _ = [' ' | '\n']*

        rule num() -> i32
            = _ v:$("-"? ['0'..='9']+) _ {? v.parse().or(Err("number parse error")) }
                
        pub rule unit() -> Option<Unit> = precedence! {
            x:(@) _ "*" _ y:@ { Some(x? * y?) }
            x:(@) _ "/" _ y:@ { Some(x? / y?) }
            --
            u:(@) _ "^" _ n:num() { let mut unit = Unit::Derived { top: vec![], bottom: vec![], multiplier: Float::with_val(1024, 1) }; if n == 0 { } else if n > 0 { for _ in 0..n { unit = unit * u.clone()? } } else { for _ in 0..n { unit = unit * u.clone()? } } Some(unit) }
            _ u:$(['a'..='z' | 'A'..='Z']+) _ { holder.single_from_string(u) }
        }
    }
}
