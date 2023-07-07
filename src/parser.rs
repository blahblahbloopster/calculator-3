use std::{ops::{Range, Deref, RangeInclusive}, fmt::Debug};

use rand::{rngs::StdRng, Rng};
use rug::{Float, Complex};

use crate::{units::{UnitTree, UV}, rpn::{RPN, Function}};

peg::parser! {
    pub grammar rpn_parser(calc: &RPN) for str {
        rule _ = [' ' | '\n']*

        rule digits() -> &'input str
            = $(['0'..='9']+)

        rule decimal() -> &'input str
            = $(digits() ("." digits())?)
        
        rule float() -> Float
            = v:$(decimal() ("e" float())?) {? Ok(Float::with_val(calc.prec, Float::parse(v).or(Err("number format error"))?)) }
        
        rule num() -> Complex
            = "(" real:float() ("," / "+")? imaginary:float() "i"? ")" {? Ok(Complex::with_val(calc.prec, (real, imaginary))) } /
              real:float() "+" imaginary:float() "i" {? Ok(Complex::with_val(calc.prec, (real, imaginary))) } /
              imaginary:float() "i" {? Ok(Complex::with_val(calc.prec, (Float::new(calc.prec), imaginary))) } /
              real:float() { Complex::with_val(calc.prec, real) }

        rule number() -> Tag<Complex>
            = l:position!() v:num() r:position!() { Tag::new(v, l..r) }
            
        rule unit() -> Tag<UnitTree>
            = l:position!() "`" v:$([^ '`']+) "`" r:position!() {? Tag::new(calc.units.parse(v), l..r).ok_ora("failed to parse unit") }  // TODO make this more helpful
        
        rule u_number() -> Tag<PUnitValue>
            = l:position!() value:number() u:unit()? r:position!() { Tag::new(match u {
                Some(v) => PUnitValue::UnV { unit: v, value },
                None => PUnitValue::Dimensionless { value }
            }, l..r) }
        
        rule o() -> Op
            = "+" { Op::Plus } /
              "-" { Op::Minus } /
              "*" { Op::Times } /
              "/" { Op::Divide } /
              "^" { Op::Pow }
            
        rule op() -> Tag<Op>
            = l:position!() op:o() r:position!() { Tag::new(op, l..r) }
        
        rule mono_op() -> MonoOp
            = "-" { MonoOp::Minus }
        
        rule ident() -> Tag<&'input str>
            = l:position!() v:$(['A'..='Z' | 'a'..='z'] ['A'..='Z' | 'a'..='z' | '0'..='9' | '_']*) r:position!() { Tag::new(v, l..r) }
        
        rule infix() -> Infix = precedence! {
            x:(@) _ tl:position!() "+" tr:position!() _ y:@ { Infix::BiOp(Box::new(x), Tag::new(Op::Plus, tl..tr), Box::new(y)) }
            x:(@) _ tl:position!() "-" tr:position!() _ y:@ { Infix::BiOp(Box::new(x), Tag::new(Op::Minus, tl..tr), Box::new(y)) }
            --
            x:(@) _ tl:position!() "*" tr:position!() _ y:@ { Infix::BiOp(Box::new(x), Tag::new(Op::Times, tl..tr), Box::new(y)) }
            x:(@) _ tl:position!() "/" tr:position!() _ y:@ { Infix::BiOp(Box::new(x), Tag::new(Op::Divide, tl..tr), Box::new(y)) }
            --
            tl:position!() "-" tr:position!() x:(@) { Infix::MonoOp(Tag::new(MonoOp::Minus, tl..tr), Box::new(x)) }
            --
            x:(@) _ tl:position!() "^" tr:position!() _ y:@ { Infix::BiOp(Box::new(x), Tag::new(Op::Pow, tl..tr), Box::new(y)) }
            --
            name:ident() "(" params:infix() ** ("," _) ")" { Infix::FunctionInv(name.map(|x| x.to_string()), params) }
            name:ident() { Infix::VarAccess(name.map(|x| x.to_string())) }
            "(" v:infix() ")" { v }
            --
            v:u_number() { Infix::Num(v) }
        }

        rule pint() -> Tag<i32>
            = l:position!() v:$(['0'..='9']+) r:position!() { Tag::new(v.parse::<i32>().unwrap(), l..r) }
        
        rule int() -> Tag<i32>
            = l:position!() "-" v:pint() r:position!() { Tag::new(-*v, l..r) } / pint()

        rule dice() -> DiceInfix = precedence! {
            n:pint() "*" v:(@) { DiceInfix::Multiply(n, Box::new(v)) }
            --
            n:pint() v:(@) { DiceInfix::Multi(n, Box::new(v)) }
            --
            "adv(" v:dice() ")" { DiceInfix::Advantage(1, Box::new(v)) }
            "dis(" v:dice() ")" { DiceInfix::Advantage(1, Box::new(v)) }
            "(" v:dice() ")" { v }
            --
            "d" l:position!() n:pint() r:position!() { DiceInfix::Roll(Tag::new(1..=*n, l..r)) }
        }

        rule dice_command() -> Tag<Command>
            = l:position!() "[" expr:dice() "]" r:position!() { Tag::new(Command::Dice(expr), l..r) }
        rule comment_command() -> Tag<Command>
            = l:position!() "\"" [^'"']* "\"" r:position!() { Tag::new(Command::Comment, l..r) }
        rule infix_command() -> Tag<Command>
            = l:position!() "'" vl:position!() v:infix() vr:position!() "'" r:position!() { Tag::new(Command::Infix(Tag::new(v, vl..vr)), l..r) }
        rule number_command() -> Tag<Command>
            = l:position!() v:u_number() r:position!() { Tag::new(Command::Number(v), l..r) }
        rule operation() -> Tag<Command>
            = l:position!() v:op() r:position!() { Tag::new(Command::Operation(v), l..r) }
        rule var_assign() -> Tag<Command>
            = l:position!() "=" vl:position!() v:ident() r:position!() { Tag::new(Command::Function(Function::VarSet(v.to_string())), l..r) }
        rule function() -> Tag<Command>
            = l:position!() v:ident() r:position!() {? Ok(Tag::new(Command::Function(Function::from_string(v.to_string()).ok_or("function not found")?), l..r)) }
        rule convert() -> Tag<Command>
            = l:position!() "to" _ v:unit() r:position!() { Tag::new(Command::Convert(v), l..r) }
        rule cast() -> Tag<Command>
            = l:position!() v:unit() r:position!() { Tag::new(Command::UnitSet(v), l..r) }
        
        rule command() -> Tag<Command>
            = convert() / infix_command() / number_command() / operation() / var_assign() / function() / cast() / comment_command() / dice_command()
        
        pub rule commands() -> Vec<Tag<Command>>
            = v:command() ** _ _ { v }
    }
}

#[derive(Clone)]
pub struct Tag<T> {
    pub item: T,
    pub loc: Range<usize>
}

impl<T> Deref for Tag<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T : Debug> Debug for Tag<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.item.fmt(f)
    }
}

impl<T> Tag<T> {
    fn new(item: T, loc: Range<usize>) -> Tag<T> {
        Tag { item, loc }
    }

    fn map<R>(self, func: fn(T) -> R) -> Tag<R> {
        Tag { item: func(self.item), loc: self.loc }
    }
}

impl<T> Tag<Option<T>> {
    fn ok_ora<E>(self, e: E) -> Result<Tag<T>, E> {
        match self.item {
            Some(v) => Ok(Tag::new(v, self.loc)),
            None => Err(e)
        }
    }
}

#[derive(Debug, Clone)]
pub enum PUnitValue {
    UnV { unit: Tag<UnitTree>, value: Tag<Complex> },
    Dimensionless { value: Tag<Complex> }
}

impl PUnitValue {
    pub fn as_uv(self) -> UV {
        match self {
            PUnitValue::UnV { unit, value } => UV { value: value.item, unit: unit.item },
            PUnitValue::Dimensionless { value } => UV { value: value.item, unit: UnitTree::Product(vec![], None) }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Command {
    Number(Tag<PUnitValue>),
    Operation(Tag<Op>),
    Infix(Tag<Infix>),
    Convert(Tag<UnitTree>),
    UnitSet(Tag<UnitTree>),
    Function(Function),
    Dice(DiceInfix),
    Comment,
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Plus, Minus, Times, Divide, Pow
}

#[derive(Debug, Clone, Copy)]
pub enum MonoOp {
    Minus
}

#[derive(Debug, Clone)]
pub enum Infix {
    BiOp(Box<Infix>, Tag<Op>, Box<Infix>),
    Num(Tag<PUnitValue>),
    MonoOp(Tag<MonoOp>, Box<Infix>),
    FunctionInv(Tag<String>, Vec<Infix>),
    VarAccess(Tag<String>)
}

#[derive(Debug, Clone)]
pub enum DiceInfix {
    Roll(Tag<RangeInclusive<i32>>),
    Multi(Tag<i32>, Box<DiceInfix>),
    Multiply(Tag<i32>, Box<DiceInfix>),
    Advantage(i32, Box<DiceInfix>)
}

impl DiceInfix {
    pub fn roll(&self, rng: &mut StdRng) -> i32 {
        match self {
            DiceInfix::Roll(range) => rng.gen_range(range.item.clone()),
            DiceInfix::Multi(n, inner) => {
                let mut out = 0;
                for _ in 0..**n {
                    out += inner.roll(rng);
                }
                out
            }
            DiceInfix::Multiply(n, inner) => **n * inner.roll(rng),
            DiceInfix::Advantage(factor, inner) => {
                let mut out = inner.roll(rng);
                let is_advantage = *factor > 0;
                for _ in 0..factor.abs() {
                    let gotten = inner.roll(rng);
                    if is_advantage {
                        if gotten > out {
                            out = gotten;
                        }
                    } else {
                        if gotten < out {
                            out = gotten;
                        }
                    }
                }
                out
            }
        }
    }
}
