use std::{fmt::{Display, Debug}, collections::HashMap, ops::{Mul, Div, Add, Neg, Sub}};

use rug::{Float, Complex, ops::Pow};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnitName {
    pub main: String, pub others: Vec<String>
}

impl Display for UnitName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.main)
    }
}

impl Debug for UnitName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BaseUnit(pub UnitName);

#[derive(PartialEq, Clone, Debug)]
pub enum UnitTree {
    Base(BaseUnit),
    Product(Vec<UnitTree>, Option<UnitName>),
    Quotient(Box<UnitTree>, Box<UnitTree>, Option<UnitName>),
    Scale(Box<UnitTree>, Float, UnitName)
}

impl Display for UnitTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_none() {
            return write!(f, "");
        }
        match self {
            UnitTree::Base(name) => write!(f, "{}", name.0),
            UnitTree::Product(items, name) => {
                // TODO: add powers
                match name {
                    Some(v) => {
                        return write!(f, "{}", v);
                    }
                    None => {}
                }
                if items.len() > 1 { write!(f, "(")?; }
                for (idx, item) in items.iter().enumerate() {
                    write!(f, "{}", item)?;
                    if idx != items.len() - 1 {
                        write!(f, "*")?;
                    }
                }
                if items.len() > 1 { write!(f, ")") } else { write!(f, "") }  // cursed
            }
            UnitTree::Quotient(av, b, name) => {
                match name {
                    Some(v) => {
                        return write!(f, "{}", v);
                    }
                    None => {}
                }

                let mut a = av.to_string();

                if a.is_empty() {
                    a = "1".to_string();
                }
                
                write!(f, "{}/{}", a, b)
            }
            UnitTree::Scale(_, _, name) => write!(f, "{}", name),
        }
    }
}

impl UnitTree {
    pub fn dimensionless() -> UnitTree {
        UnitTree::Product(vec![], None)
    }

    fn scl_name(self, factor: Float, name: UnitName) -> UnitTree {
        UnitTree::Scale(Box::new(self), factor, name)
    }

    fn precision(&self) -> Option<u32> {
        match self {
            UnitTree::Base(_) => None,
            UnitTree::Product(items, _) => items.iter().find_map(|x| x.precision()),
            UnitTree::Quotient(top, bottom, _) => Some(top.precision().unwrap_or(bottom.precision()?)),
            UnitTree::Scale(_, k, _) => Some(k.prec()),
        }
    }

    pub fn metricify(&self) -> (HashMap<BaseUnit, i32>, Float) {
        let mut out = HashMap::new();
        let mut mul = Float::with_val(self.precision().unwrap_or(1024), 1);
        match self {
            UnitTree::Base(k) => { out.insert(k.clone(), 1); }
            UnitTree::Product(items, _) => {
                for item in items {
                    let (gotten, k) = item.metricify();
                    mul *= k;
                    for (unit, power) in gotten {
                        if out.contains_key(&unit) {
                            *out.get_mut(&unit).unwrap() += power
                        } else {
                            out.insert(unit, power);
                        }
                    }
                }
            },
            UnitTree::Quotient(a, b, _) => {
                let (gotten, k) = a.metricify();
                mul *= k;
                for (unit, power) in gotten {
                    if out.contains_key(&unit) {
                        *out.get_mut(&unit).unwrap() += power
                    } else {
                        out.insert(unit, power);
                    }
                }

                let (gotten, k) = b.metricify();
                mul /= k;
                for (unit, power) in gotten {
                    if out.contains_key(&unit) {
                        *out.get_mut(&unit).unwrap() -= power
                    } else {
                        out.insert(unit, -power);
                    }
                }
                
            }
            UnitTree::Scale(a, scale, _) => {
                let (gotten, k) = a.metricify();
                mul *= k;  // TEST ME
                mul *= scale;
                for (unit, power) in gotten {
                    if out.contains_key(&unit) {
                        *out.get_mut(&unit).unwrap() += power
                    } else {
                        out.insert(unit, power);
                    }
                }
            }
        }

        let mut to_remove = vec![];
        for (k, v) in &out {
            if *v == 0 {
                to_remove.push(k.clone())
            }
        }

        for item in to_remove {
            out.remove(&item);
        }

        (out, mul)
    }

    pub fn compatible(&self, other: &UnitTree) -> bool {
        self.metricify().0 == other.metricify().0
    }

    // pub fn multiply(self, k: Float) -> UnitTree {
    //     match self {
    //         Self::Scale(inner, factor, name) => 
    //         _ => {}
    //     }
    // }

    pub fn is_dimensionless(&self) -> bool {
        self.metricify().0.is_empty()
    }

    pub fn is_none(&self) -> bool {
        self.is_dimensionless() && (self.metricify().1 - Float::with_val(self.precision().unwrap_or(1024), 1)).to_f64().abs() < 0.00001
    }

    pub fn exp(&self, e: &Complex) -> Option<UnitTree> {
        let as_frac = e.real().clone().recip();
        let is_frac = as_frac.is_integer();  // TODO: epsilon
        let is_suitable = e.imag().is_zero() || (e.real().is_integer() || is_frac);
        if !is_suitable { return None; }

        if is_frac {
            match self {
                UnitTree::Base(_) => return None,
                UnitTree::Product(items, _) => {
                    let first = items.get(0)?;
                    return if items.len() == as_frac.to_i32_saturating()? as usize && items.iter().all(|x| x == first) {
                        Some(first.clone())
                    } else {
                        None
                    }
                }
                UnitTree::Quotient(_, _, _) => return None,
                UnitTree::Scale(_, _, _) => return None,
            }
        }

        let exp = e.real().to_i32_saturating()?;

        let mut prod = vec![];
        for _ in 0..exp.abs() {
            prod.push(self.clone())
        }
        let group = UnitTree::Product(prod, None);
        if exp < 0 {
            Some(UnitTree::Quotient(Box::new(UnitTree::dimensionless()), Box::new(group), None))
        } else {
            Some(group)
        }
    }
}

impl Mul for UnitTree {
    type Output = UnitTree;

    fn mul(self, rhs: Self) -> Self::Output {
        if rhs.is_none() {
            return self;
        } else if self.is_none() {
            return rhs;
        }
        UnitTree::Product(vec![self, rhs], None)
    }
}

impl Div for UnitTree {
    type Output = UnitTree;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_none() {
            return self;
        }
        UnitTree::Quotient(Box::new(self), Box::new(rhs), None)
    }
}

#[derive(Debug, Clone)]
pub enum UVError {
    Mismatch { left: UnitTree, right: UnitTree },
    DivisionByZero
}

pub trait ReplaceIfEmpty {
    fn if_empty(self, other: Self) -> Self;
}

impl ReplaceIfEmpty for String {
    fn if_empty(self, other: Self) -> Self {
        if self.is_empty() {
            other
        } else {
            self
        }
    }
}

impl Display for UVError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UVError::Mismatch { left, right } => {
                let l = left.to_string().if_empty("unitless".to_string());
                let r = right.to_string().if_empty("unitless".to_string());

                write!(f, "expected {}, found {}", l, r)
            }
            UVError::DivisionByZero => write!(f, "division by zero"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UV {
    pub unit: UnitTree,
    pub value: Complex
}

impl Add for UV {
    type Output = Result<UV, UVError>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.unit.compatible(&rhs.unit) {
            let l_mul = self.unit.metricify().1;
            let r_mul = rhs.unit.metricify().1;

            Ok(UV { unit: self.unit, value: self.value + rhs.value * r_mul / l_mul })
        } else {
            Err(UVError::Mismatch { left: self.unit, right: rhs.unit })
        }
    }
}

impl Neg for UV {
    type Output = UV;
    
    fn neg(self) -> Self::Output {
        UV { unit: self.unit, value: -self.value }
    }
}

impl Sub for UV {
    type Output = Result<UV, UVError>;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for UV {
    type Output = UV;

    fn mul(self, rhs: Self) -> Self::Output {
        let new_units = self.unit * rhs.unit;

        UV { unit: new_units, value: self.value * rhs.value }
    }
}

impl Div for UV {
    type Output = Result<UV, UVError>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.value.eq0() {
            return Err(UVError::DivisionByZero)
        }
        Ok(UV { unit: self.unit / rhs.unit, value: self.value / rhs.value })
    }
}

impl UV {
    pub fn convert(self, dst: UnitTree) -> Result<UV, UVError> {
        if !self.unit.compatible(&dst) { return Err(UVError::Mismatch { left: self.unit, right: dst }); }
        let metricified = self.value * self.unit.metricify().1;
        let converted = metricified / dst.metricify().1;
        Ok(UV { unit: dst, value: converted })
    }

    pub fn exp(self, power: Complex) -> UV {
        UV { unit: self.unit.exp(&power).unwrap_or(UnitTree::dimensionless()), value: self.value.pow(power) }
    }

    pub fn as_rad(self, holder: &UnitHolder) -> Result<Complex, UVError> {
        if self.unit.is_none() {
            return Ok(self.value)
        }
        let radians = unit_parser::unit("rad", holder).unwrap().unwrap();
        self.convert(radians).map(|x| x.value)
    }

    pub fn metricify(self) -> UV {
        let (coefficients, multiplier) = self.unit.metricify();
        let new_unit = UnitTree::Product(coefficients.iter().map(|x| {
            if *x.1 == 1 {
                UnitTree::Base(x.0.clone())
            } else if *x.1 == 0 {
                UnitTree::dimensionless()
            } else {
                let mut out = vec![];
                for _ in 0..x.1.abs() {
                    out.push(UnitTree::Base(x.0.clone()));
                }
                if *x.1 > 0 {
                    UnitTree::Product(out, None)
                } else {
                    UnitTree::Quotient(Box::new(UnitTree::dimensionless()), Box::new(UnitTree::Product(out, None)), None)
                }
            }
        }).collect(), None);

        UV { unit: new_unit, value: self.value * multiplier }
    }
}

#[derive(Debug, Clone)]
pub struct UnitHolder {
    pub units: Vec<ParsedUnit>,
    pub prec: u32
}

impl UnitHolder {
    pub fn single_from_string(&self, input: &str) -> Option<UnitTree> {
        let inp = &input.replace("*", "");
        for item in &self.units {
            for subitem in &item.names {
                if subitem == inp {
                    return Some(item.unit.clone())
                }
            }
        }
        None
    }

    pub fn new(inp: &str, prec: u32) -> UnitHolder {
        let mut out = UnitHolder { units: vec![], prec };
        let mut duplicates = vec![];
        inp.lines().filter(|x| !(x.starts_with('#') || x.is_empty())).for_each(|x| {
            let unit = unit_file_parser::row(x, &mut out).or(Err(format!("error parsing '{}'", x))).unwrap();
            for u in unit {
                for v in &out.units {
                    if u.names.iter().any(|n1| v.names.iter().any(|n2| n1 == n2)) {
                        let mut de_microed_a = u.names.clone();
                        let mut de_microed_b = v.names.clone();

                        for item in &mut de_microed_a {
                            if item.starts_with("micro") {
                                *item = format!("μ{}", item.strip_prefix("micro").unwrap());
                            }
                            if item.starts_with("u") {
                                *item = format!("μ{}", item.strip_prefix("u").unwrap());
                            }
                        }
                        for item in &mut de_microed_b {
                            if item.starts_with("micro") {
                                *item = format!("μ{}", item.strip_prefix("micro").unwrap());
                            }
                            if item.starts_with("u") {
                                *item = format!("μ{}", item.strip_prefix("u").unwrap());
                            }
                        }

                        if de_microed_a != de_microed_b {
                            duplicates.push((u.names.clone(), v.names.clone()))
                        }

                        // panic!("duplicate unit {:?} and {:?}", u.names, v.names);
                    }
                }

                out.units.push(u);
            }
            // out.units.extend(unit);
        });
        if !duplicates.is_empty() {
            panic!("duplicate units found:\n{}", duplicates.iter().map(|(a, b)| format!("{a:?} and {b:?}")).collect::<Vec<_>>().join("\n"))
        }

        for u in &out.units {
            for n in &u.names {
                if n.contains('*') {
                    panic!()
                }
            }
            let n = match &u.unit {
                UnitTree::Base(_) => { None }
                UnitTree::Scale(_, _, name) => { Some(name) }
                UnitTree::Product(_, Some(name)) => { Some(name) }
                UnitTree::Product(_, None) => None,
                UnitTree::Quotient(_, _, Some(name)) => { Some(name) }
                UnitTree::Quotient(_, _, _) => None
            };
            if let Some(names) = n {
                if names.main.contains('*') {
                    panic!("name '{}' contains asterisk", names.main)
                }
                for name in &names.others {
                    if name.contains('*') {
                        panic!("secondary name '{}' contains asterisk", name)
                    }
                }
            }
        }
        out
    }

    pub fn parse(&self, inp: &str) -> Option<UnitTree> {
        unit_parser::unit(inp, self).ok()?  // this is fine
    }
}

#[derive(Debug, Clone)]
pub struct ParsedUnit {
    pub names: Vec<String>,
    pub unit: UnitTree
}

peg::parser! {
    grammar unit_file_parser() for str {
        rule _ = [' ' | '\n']*

        rule num() -> i32
            = _ v:$("-"? ['0'..='9']+) _ {? v.parse().or(Err("number parse error")) }
            
        rule unit(holder: &mut UnitHolder) -> UnitTree = precedence! {
            x:(@) "*" y:@ { x * y }
            x:(@) "/" y:@ { x / y }
            --
            u:(@) "^" n:num() {
                let mut out = vec![];
                for _ in 0..n.abs() {
                    out.push(u.clone())
                }
                if n < 0 {
                    UnitTree::Quotient(Box::new(UnitTree::Product(vec![], None)), Box::new(UnitTree::Product(out, None)), None)
                } else {
                    UnitTree::Product(out, None)
                }
            }
            _ u:$(['a'..='z' | 'A'..='Z' | 'μ']+) _ { holder.single_from_string(u).expect(format!("failed to parse '{}'", u).as_str()) }
        }

        rule name() -> &'input str
            = _ v:$(['a'..='z' | 'A'..='Z' | 'μ' | '*'] ['a'..='z' | 'A'..='Z' | 'μ']*) _ { v }
        
        rule digits() -> &'input str
            = $(['0'..='9']+)

        rule decimal() -> &'input str
            = $(digits() ("." digits())?)
        
        rule float(holder: &UnitHolder) -> Float
            = _ v:$("-"? decimal() ("e" float(holder))?) _ {? Ok(Float::with_val(holder.prec, Float::parse(v).or(Err("number format error"))?)) }
            / _ "pi" _ { Float::with_val(holder.prec, rug::float::Constant::Pi) }
        
        rule coefficient(holder: &UnitHolder) -> Float = precedence! {
            x:(@) "+" y:@ { x + y }
            x:(@) "-" y:@ { x - y }
            --
            x:(@) "*" y:@ { x * y }
            x:(@) "/" y:@ { x / y }
            --
            // I don't know why, but this breaks it
            // "(" v:coefficient() ")" { v }
            // --
            v:float(holder) { v }
        }

        rule r(holder: &mut UnitHolder) -> ParsedUnit
            = n:name() ++ "/" ":" v:coefficient(holder) u:unit(holder) { ParsedUnit { names: n.iter().map(|x| x.to_string()).collect(), unit: UnitTree::Scale(Box::new(u), v, UnitName { main: n[0].to_string().replace("*", ""), others: n.iter().skip(1).map(|x| x.to_string().replace("*", "")).collect() }) } } /
              n:name() ++ "/" { let name = n[0].to_string(); ParsedUnit { names: n.iter().map(|x| x.to_string()).collect(), unit: UnitTree::Base(BaseUnit(UnitName { main: n[0].to_string().replace("*", ""), others: n.iter().skip(1).map(|x| x.to_string().replace("*", "")).collect() })) } }
        
        pub rule row(holder: &mut UnitHolder) -> Vec<ParsedUnit>
            = original:r(holder) "(prefixes)" { original.prefixes() } /
              original:r(holder) { original.de_asterisk() }
    }
}

impl ParsedUnit {
    fn de_asterisk(self) -> Vec<ParsedUnit> {
        let mut out = self;
        for item in &mut out.names {
            if item.starts_with("*") {
                *item = item.strip_prefix("*").unwrap().to_string();
            }
        }
        vec![out]
    }

    fn prefixes(self) -> Vec<ParsedUnit> {
        let prefixes = vec![
            ("quetta", 	"Q", 	Float::with_val(1024, 10).pow(30)),
            ("ronna", 	"R", 	Float::with_val(1024, 10).pow(27)),
            ("yotta", 	"Y", 	Float::with_val(1024, 10).pow(24)),
            ("zetta", 	"Z", 	Float::with_val(1024, 10).pow(21)),
            ("exa", 	"E", 	Float::with_val(1024, 10).pow(18)),
            ("peta", 	"P", 	Float::with_val(1024, 10).pow(15)),
            ("tera", 	"T", 	Float::with_val(1024, 10).pow(12)),
            ("giga", 	"G", 	Float::with_val(1024, 10).pow(9)),
            ("mega", 	"M", 	Float::with_val(1024, 10).pow(6)),
            ("kilo", 	"k", 	Float::with_val(1024, 10).pow(3)),
            ("hecto", 	"h", 	Float::with_val(1024, 10).pow(2)),
            ("deca", 	"da", 	Float::with_val(1024, 10).pow(1)),
            ("deci", 	"d", 	Float::with_val(1024, 10).pow(-1)),
            ("centi", 	"c", 	Float::with_val(1024, 10).pow(-2)),
            ("milli", 	"m", 	Float::with_val(1024, 10).pow(-3)),
            ("micro", 	"μ", 	Float::with_val(1024, 10).pow(-6)),
            ("micro", 	"u", 	Float::with_val(1024, 10).pow(-6)),
            ("nano", 	"n", 	Float::with_val(1024, 10).pow(-9)),
            ("pico", 	"p", 	Float::with_val(1024, 10).pow(-12)),
            ("femto", 	"f", 	Float::with_val(1024, 10).pow(-15)),
            ("atto", 	"a", 	Float::with_val(1024, 10).pow(-18)),
            ("zepto", 	"z", 	Float::with_val(1024, 10).pow(-21)),
            ("yocto", 	"y", 	Float::with_val(1024, 10).pow(-24)),
            ("ronto", 	"r", 	Float::with_val(1024, 10).pow(-27)),
            ("quecto", 	"q", 	Float::with_val(1024, 10).pow(-30)),

            ("quebi", 	"Qi", 	Float::with_val(1024, 2).pow(100)),
            ("robi", 	"Ri", 	Float::with_val(1024, 2).pow(90)),
            ("yobi", 	"Yi", 	Float::with_val(1024, 2).pow(80)),
            ("zebi", 	"Zi", 	Float::with_val(1024, 2).pow(70)),
            ("exbi", 	"Ei", 	Float::with_val(1024, 2).pow(60)),
            ("pebi", 	"Pi", 	Float::with_val(1024, 2).pow(50)),
            ("tebi", 	"Ti", 	Float::with_val(1024, 2).pow(40)),
            ("gibi", 	"Gi", 	Float::with_val(1024, 2).pow(30)),
            ("mebi", 	"Mi", 	Float::with_val(1024, 2).pow(20)),
            ("kibi", 	"Ki", 	Float::with_val(1024, 2).pow(10)),
        ];

        let mut out = vec![self.clone()];
        out[0].names.iter_mut().for_each(|x| { *x = x.replace("*", "") });
        for (full, abbrev, factor) in prefixes {
            // let factor = Float::with_val(1024, 10).pow(power);

            let mut names = self.names.iter().filter(|x| x.len() > 3 || x.starts_with("*")).map(|x| format!("{}{}", full, x.strip_prefix("*").unwrap_or(x))).collect::<Vec<_>>();
            names.extend(self.names.iter().filter(|x| x.len() <= 3 && !x.starts_with("*")).map(|x| format!("{}{}", abbrev, x.strip_prefix("*").unwrap_or(x))));

            let mut others = names.clone();
            let main = others.remove(0);
            out.push(ParsedUnit { names, unit: self.unit.clone().scl_name(factor, UnitName { main, others }) });
        }

        out
    }
}

peg::parser! {
    pub grammar unit_parser(holder: &UnitHolder) for str {
        rule _ = [' ' | '\n']*

        rule num() -> i32
            = _ v:$("-"? ['0'..='9']+) _ {? v.parse().or(Err("number parse error")) }
        pub rule unit() -> Option<UnitTree> = precedence! {
            x:(@) _ "*" _ y:@ { Some(x? * y?) }
            x:(@) _ "/" _ y:@ { Some(x? / y?) }
            --
            u:(@) _ "^" _ n:num() {
                let mut out = vec![];
                for _ in 0..n.abs() {
                    out.push(u.clone()?)
                }
                if n < 0 {
                    Some(UnitTree::Quotient(Box::new(UnitTree::Product(vec![], None)), Box::new(UnitTree::Product(out, None)), None))
                } else {
                    Some(UnitTree::Product(out, None))
                }
            }
            --
            "(" u:unit() ")" { u }
            --
            _ u:$(['a'..='z' | 'A'..='Z']+) _ { holder.single_from_string(u) }
        }
    }
}
