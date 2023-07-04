use std::{fmt::Display, borrow::Borrow, collections::HashMap};

use rug::{Float, Complex};

use crate::rpn::PComplex;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Name {
    full: String,
    abbreviations: Vec<String>
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.full)
    }
}

impl Name {
    fn abr(&self) -> String {
        self.abbreviations.first().unwrap().clone()
    }
}

macro_rules! flt {
    ($value:expr) => {
        Float::with_val(1024, $value)
    };
}

#[derive(PartialEq, Clone)]
pub enum Un {
    Base(BaseUn),
    Quotient { name: Option<Name>, top: Vec<Un>, bottom: Vec<Un> },
    Scaled { name: Name, scale: Float, inner: Box<Un> }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct BaseUn {
    name: Name
}

impl Display for BaseUn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Un {
    pub fn name(&self) -> Option<Name> {
        match self {
            Un::Base(v) => Some(v.name.clone()),
            Un::Quotient { name, .. } => name.clone(),
            Un::Scaled { name, .. } => Some(name.clone())
        }
    }

    fn metricify(&self) -> HashMap<BaseUn, i32> {
        match self {
            Un::Base(v) => { let mut o = HashMap::new(); o.insert(v.clone(), 1); o },
            Un::Quotient { name, top, bottom } => {
                let mut out = HashMap::new();
                for item in top {
                    let gotten = item.metricify();
                    for (un, power) in gotten {
                        let found = out.get_mut(&un);
                        match found {
                            Some(v) => { *v += power; }
                            None => { out.insert(un, power); }
                        }
                    }
                }
                for item in bottom {
                    let gotten = item.metricify();
                    for (un, power) in gotten {
                        let found = out.get_mut(&un);
                        match found {
                            Some(v) => { *v -= power; }
                            None => { out.insert(un, -power); }
                        }
                    }
                }

                let to_remove: Vec<_> = out.iter().filter(|(_, v)| **v == 0).map(|(k, _)| k.clone()).collect();

                for item in to_remove {
                    out.remove(&item);
                }

                out
            }
            Un::Scaled { name, scale, inner } => inner.metricify()
        }
    }

    fn compatible(&self, other: &Un) -> bool {
        self.metricify() == other.metricify()
    }

    // /// Gets a list of all of the factors on the top of the fractions
    // fn factors(inp: &Vec<Un>) -> Vec<Un> {
    //     let mut out = vec![];

    //     for item in inp {
    //         match item {}
    //     }

    //     out
    // }

    fn simplify(&mut self) {
        match self {
            Un::Base(v) => {}
            Un::Quotient { name, top, bottom } => {
                let mut num_removed = 0;
                loop {
                    let mut common = None;
                    for item in top.iter() {
                        for other in bottom.iter() {
                            if item == other {
                                common = Some(item.clone());
                                break;
                            }
                        }
                    }
                    match common {
                        Some(v) => {
                            let top_idx = top.iter().enumerate().find(|(_, item)| item == &&v).map(|(idx, _)| idx).unwrap();
                            let bottom_idx = bottom.iter().enumerate().find(|(_, item)| item == &&v).map(|(idx, _)| idx).unwrap();
                            top.remove(top_idx);
                            bottom.remove(bottom_idx);
                            num_removed += 1;
                        }
                        None => break
                    }
                }

                if num_removed != 0 {
                    *name = None;
                }
            }
            Un::Scaled { name, scale, inner } => {}
        }
    }
}

impl Display for Un {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Un::Base(n) => write!(f, "{}", n),
            Un::Quotient { name, top, bottom } => {
                match name {
                    Some(v) => write!(f, "{}", v.abr()),
                    None => {
                        write!(f, "((")?;
                        for (idx, item) in top.iter().enumerate() {
                            write!(f, "{}", item)?;
                            if idx != top.len() - 1 {
                                write!(f, "*")?
                            }
                        }

                        write!(f, ")/(")?;

                        for (idx, item) in bottom.iter().enumerate() {
                            write!(f, "{}", item)?;
                            if idx != bottom.len() - 1 {
                                write!(f, "*")?
                            }
                        }
                        write!(f, "))")
                    }
                }
            }
            Un::Scaled { name, .. } => write!(f, "{}", name)
        }
    }
}

pub struct UnValue {
    pub unit: Un,
    pub value: Complex
}

impl Display for UnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", PComplex(&self.value), self.unit)
    }
}

impl UnValue {
    fn metricify(&mut self) {
    }
}

pub fn blah() {
}
