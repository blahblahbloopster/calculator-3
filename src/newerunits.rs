use std::{fmt::{Display, Debug}, collections::HashMap, ops::{Mul, Div, Add, Neg, Sub}};

use rug::{Float, Complex};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnitName {
    main: String, others: Vec<String>
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
pub struct BaseUnit(UnitName);

#[derive(PartialEq, Clone)]
pub enum UnitTree {
    Base(BaseUnit),
    Product(Vec<UnitTree>, Option<UnitName>),
    Quotient(Box<UnitTree>, Box<UnitTree>, Option<UnitName>),
    Scale(Box<UnitTree>, Float, UnitName)
}

impl Debug for UnitTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnitTree::Base(name) => write!(f, "{}", name.0),
            UnitTree::Product(items, _) => {
                write!(f, "(")?;
                for item in items {
                    write!(f, "{:?} ", item)?
                }
                write!(f, ")")
            }
            UnitTree::Quotient(a, b, _) => write!(f, "{:?}/{:?}", a, b),
            UnitTree::Scale(_, _, name) => write!(f, "{}", name),
        }
    }
}

impl UnitTree {
    pub fn metricify(&self) -> (HashMap<BaseUnit, i32>, Float) {
        let mut out = HashMap::new();
        let mut mul = Float::with_val(1024, 1);
        match self {
            UnitTree::Base(k) => { out.insert(k.clone(), 1); }
            UnitTree::Product(items, _) => {
                for item in items {
                    let (gotten, k) = item.metricify();
                    mul *= k;  // TEST ME
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
                mul *= k;  // TEST ME
                for (unit, power) in gotten {
                    if out.contains_key(&unit) {
                        *out.get_mut(&unit).unwrap() += power
                    } else {
                        out.insert(unit, power);
                    }
                }

                let (gotten, k) = b.metricify();
                mul /= k;  // TEST ME
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
        (out, mul)
    }

    pub fn compatible(&self, other: &UnitTree) -> bool {
        self.metricify().0 == other.metricify().0
    }
}

impl Mul for UnitTree {
    type Output = UnitTree;

    fn mul(self, rhs: Self) -> Self::Output {
        UnitTree::Product(vec![self, rhs], None)
    }
}

impl Div for UnitTree {
    type Output = UnitTree;

    fn div(self, rhs: Self) -> Self::Output {
        UnitTree::Quotient(Box::new(self), Box::new(rhs), None)
    }
}

pub enum UVError {
    Mismatch { left: UnitTree, right: UnitTree },
    DivisionByZero
}

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

            Ok(UV { unit: self.unit, value: self.value + rhs.value * r_mul / l_mul })  // TEST ME
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
        // TEST ME
        let initial = self.unit.metricify().1;
        let new_units = self.unit * rhs.unit;
        let factor = new_units.metricify().1 / initial;

        UV { unit: new_units, value: self.value * factor }
    }
}

impl Div for UV {
    type Output = Result<UV, UVError>;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.value.eq0() {
            return Err(UVError::DivisionByZero)
        }
        Ok(UV { unit: self.unit / rhs.unit, value: self.value / rhs.value })  // TEST ME
    }
}

impl UV {
    pub fn convert(self, dst: UnitTree) -> Result<UV, UVError> {
        // TEST ME
        if !self.unit.compatible(&dst) { return Err(UVError::Mismatch { left: self.unit, right: dst }); }
        let metricified = self.value * self.unit.metricify().1;
        let converted = metricified / dst.metricify().1;
        Ok(UV { unit: dst, value: converted })
    }
}
