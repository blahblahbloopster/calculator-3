use std::{ops::{Range, Deref}, fmt::Debug};

use rug::{Float, Complex};

use crate::{units::{UnitTree, UV, Base}, rpn::{RPN, Function}, dice::TaggedDiceRoll};

peg::parser! {
    pub grammar rpn_parser(calc: &RPN) for str {
        rule _ = [' ' | '\n']*

        rule float_b() -> (Base, Float)
            = "0x" v:$("-"? $(['0'..='9' | 'a'..='f' | 'A'..='F']+ ("." ['0'..='9' | 'a'..='f' | 'A'..='F']+)?)) {? Ok((Base(16), Float::with_val(calc.prec, Float::parse_radix(v, 16).or(Err("number format error"))?))) } /
              "0b" v:$("-"? $(['0' | '1']+ ("." ['0' | '1']+)?)) {?; Ok((Base(2), Float::with_val(calc.prec, Float::parse_radix(v, 2).or(Err("number format error"))?))) } /
              v:$("-"? ['0'..='9']+ ("." ['0'..='9']+)? ("e" float())?) {? Ok((Base(10), Float::with_val(calc.prec, Float::parse(v).or(Err("number format error"))?))) }

        // rule digits() -> &'input str
        //     = $(['0'..='9']+)

        // rule decimal() -> &'input str
        //     = $(digits() ("." digits())?)
        
        rule float() -> (Base, Float) = float_b()
            // = v:$("-"? decimal() ("e" float())?) {? Ok(Float::with_val(calc.prec, Float::parse(v).or(Err("number format error"))?)) }
        
        rule num() -> (Base, Complex)
            = "(" real:float() ("," / "+")? imaginary:float() "i"? ")" {? Ok((real.0, Complex::with_val(calc.prec, (real.1, imaginary.1)))) } /
              real:float() "+" imaginary:float() "i" {? Ok((real.0, Complex::with_val(calc.prec, (real.1, imaginary.1)))) } /
              imaginary:float() "i" {? Ok((imaginary.0, Complex::with_val(calc.prec, (Float::new(calc.prec), imaginary.1)))) } /
              real:float() { (real.0, Complex::with_val(calc.prec, real.1)) }

        rule number() -> Tag<(Base, Complex)>
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
              "^" { Op::Pow } /
              "!" { Op::Factorial }
            
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
            x:(@) _ tl:position!() "!" tr:position!() { Infix::MonoOp(Tag::new(MonoOp::Factorial, tl..tr), Box::new(x)) }
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

        rule dice() -> Tag<TaggedDiceRoll> = precedence! {
            a:(@) lp:position!() _ "+" _ rp:position!() b:@ { let loc = a.loc.start..b.loc.end; Tag::new(TaggedDiceRoll::Sum(Box::new(a), Tag::new((), lp..rp), Box::new(b)), loc) }
            a:(@) lp:position!() _ "-" _ rp:position!() b:@ { let loc = a.loc.start..b.loc.end; Tag::new(TaggedDiceRoll::Difference(Box::new(a), Tag::new((), lp..rp), Box::new(b)), loc) }
            --
            a:(@) lp:position!() _ "*" _ rp:position!() b:@ { let loc = a.loc.start..b.loc.end; Tag::new(TaggedDiceRoll::Product(Box::new(a), Tag::new((), lp..rp), Box::new(b)), loc) }
            --
            l:position!() n:pint() v:(@) { let loc = l..v.loc.end; Tag::new(TaggedDiceRoll::Multi(n, Box::new(v)), loc) }
            --
            l:position!() "adv(" lp:position!() v:dice() rp:position!() ")" r:position!() { Tag::new(TaggedDiceRoll::Advantage { l_paren: Tag::new((), l..lp), roll: Box::new(v), n: 1, r_paren: Tag::new((), rp..r) }, l..r) }
            l:position!() "dis(" lp:position!() v:dice() rp:position!() ")" r:position!() { Tag::new(TaggedDiceRoll::Advantage { l_paren: Tag::new((), l..lp), roll: Box::new(v), n: -1, r_paren: Tag::new((), rp..r) }, l..r) }
            "(" v:dice() ")" { v }
            --
            n:int() { let loc = n.loc.clone(); Tag::new(TaggedDiceRoll::Constant(n), loc) }
            --
            l:position!() "d" lp:position!() n:pint() r:position!() { Tag::new(TaggedDiceRoll::Simple(Tag::new((), l..lp), n), l..r) }
        }

        rule ordering() -> Comp
            = ">=" { Comp::GrEq } / ">" { Comp::Gr } / "==" { Comp::Eq } / "=" { Comp::Eq } / "!=" { Comp::NEq } / "<=" { Comp::LeEq} / "<" { Comp::Le }
        
        rule ord() -> Tag<Comp>
            = l:position!() o:ordering() r:position!() { Tag::new(o, l..r) }

        rule dice_prop_command() -> Tag<Command>
            = l:position!() ("[P(" / "[p(") expr:dice() _ comp:ord() _ num:pint() ")]" r:position!() { Tag::new(Command::DiceProb(expr, comp, num), l..r) }
        rule dice_histogram_command() -> Tag<Command>
            = l:position!() ("[P(" / "[p(") expr:dice() ")]" r:position!() { Tag::new(Command::DiceHistogram(expr), l..r) }
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
        rule unit_def() -> Tag<Command>
            = l:position!() "|" _ n:ident() _ e:infix()? _ "|" r:position!() { Tag::new(Command::UnitDef(n.map(|x| x.to_string()), e), l..r) }
        
        rule command() -> Tag<Command>
            = convert() / infix_command() / number_command() / operation() / var_assign() / function() / cast() / comment_command() / dice_histogram_command() / dice_prop_command() / dice_command() / unit_def()
        
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
    UnV { unit: Tag<UnitTree>, value: Tag<(Base, Complex)> },
    Dimensionless { value: Tag<(Base, Complex)> }
}

impl PUnitValue {
    pub fn as_uv(self) -> UV {
        match self {
            PUnitValue::UnV { unit, value: Tag { item: (b, v), .. } } => UV { value: v, unit: unit.item, base: b },
            PUnitValue::Dimensionless { value: Tag { item: (b, v), .. } } => UV { value: v, unit: UnitTree::Product(vec![], None), base: b }
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
    Dice(Tag<TaggedDiceRoll>),
    UnitDef(Tag<String>, Option<Infix>),
    DiceProb(Tag<TaggedDiceRoll>, Tag<Comp>, Tag<i32>),
    DiceHistogram(Tag<TaggedDiceRoll>),
    Comment,
}

#[derive(Debug, Clone, Copy)]
pub enum Comp {
    LeEq, Le, Eq, NEq, Gr, GrEq
}

impl Comp {
    pub fn check(&self, a: i32, b: i32) -> bool {
        match self {
            Comp::LeEq => a <= b,
            Comp::Le => a < b,
            Comp::Eq => a == b,
            Comp::NEq => a != b,
            Comp::Gr => a > b,
            Comp::GrEq => a >= b,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Plus, Minus, Times, Divide, Pow, Factorial
}

#[derive(Debug, Clone, Copy)]
pub enum MonoOp {
    Minus, Factorial
}

#[derive(Debug, Clone)]
pub enum Infix {
    BiOp(Box<Infix>, Tag<Op>, Box<Infix>),
    Num(Tag<PUnitValue>),
    MonoOp(Tag<MonoOp>, Box<Infix>),
    FunctionInv(Tag<String>, Vec<Infix>),
    VarAccess(Tag<String>)
}
