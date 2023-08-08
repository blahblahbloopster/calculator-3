use std::{ops::RangeInclusive, collections::HashMap};

use rand::{distributions::WeightedIndex, prelude::Distribution};

use crate::parser::Tag;

pub struct DiceDistr {
    pub range: RangeInclusive<i32>,
    pub distr: Vec<f64>
}

impl Distribution<i32> for DiceDistr {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> i32 {
        // FIXME this is shitty
        WeightedIndex::new(self.distr.clone()).unwrap().sample(rng) as i32 + *self.range.start()
    }
}

impl DiceDistr {
    pub fn p(&self, v: i32) -> Option<f64> {
        if self.range.contains(&v) {
            Some(self.distr[(v - *self.range.start()) as usize])
        } else {
            None
        }
    }

    fn multidimensional<F>(distrs: Vec<&DiceDistr>, func: F) -> DiceDistr where F : Fn(Vec<i32>) -> i32 {
        let mut positions = vec![];

        for item in &distrs {
            positions.push(*item.range.start())
        }

        let mut out: HashMap<i32, f64> = HashMap::new();

        'outer: loop {
            let value = func(distrs.iter().enumerate().map(|(i, x)| positions[i]).collect());
            let probability: f64 = distrs.iter().enumerate().map(|(i, x)| x.p(positions[i]).unwrap()).product();

            let v = *out.get(&value).unwrap_or(&0.0) + probability;
            out.insert(value, v);

            positions[0] += 1;

            for i in 0..distrs.len() {
                let range = distrs[i].range.clone();
                if positions[i] > *range.end() {
                    positions[i] = *range.start();
                    if i == distrs.len() - 1 {
                        break 'outer;
                    }
                    positions[i + 1] += 1;
                }
            }
        }

        let min = *out.keys().min().unwrap();
        let max = *out.keys().max().unwrap();

        let mut distr = vec![];
        for i in min..=max {
            distr.push(*out.get(&i).unwrap_or(&0.0));
        }

        DiceDistr { range: min..=max, distr }
    }

    fn bar(frac: f64, width: usize) -> String {
        let bars = [' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'];
        let count = frac * (width - 2) as f64;
        let full = "█".repeat(count as usize);
        let fractional = bars[(count.fract() * 8.0) as usize];
        // println!("{} {} {} {}", frac, full.chars().count(), count, width);

        format!("{}{}{}", full, fractional, " ".repeat(width - (full.chars().count() + 1)))
    }

    pub fn histogram(&self, width: usize) -> String {
        let maximum = self.distr.iter().map(|x| *x).reduce(f64::max).unwrap();
        let total: f64 = self.distr.iter().map(|x| *x).sum();
        let max_len = (self.range.start().abs().ilog10() + 2).min(self.range.end().abs().ilog10() + 2);

        let mut output = String::new();

        for (value, prob) in self.range.clone().zip(self.distr.iter()) {
            let mut padded = value.to_string();
            padded.push_str(" ".repeat(max_len as usize - padded.len()).as_str());
            output.push_str(format!("{}: {} {:<5.2}%\n", padded, Self::bar(prob / maximum, width - (max_len as usize + 2)), (prob / total) * 100.0).as_str());
        }

        output
    }

    pub fn expected_value(&self) -> f64 {
        let total: f64 = self.distr.iter().sum();
        let mut acc = 0.0;
        for i in 0..self.distr.len() {
            acc += (i as i32 + self.range.start()) as f64 * self.distr[i];
        }

        acc / total
    }
}

pub enum DiceRoll {
    Simple(RangeInclusive<i32>),
    Constant(i32),
    Advantage(Box<DiceRoll>, i32),
    Sum(Box<DiceRoll>, Box<DiceRoll>),
    Product(Box<DiceRoll>, Box<DiceRoll>)
}

#[derive(Debug, Clone)]
pub enum TaggedDiceRoll {
    Simple(Tag<()>, Tag<i32>),
    Constant(Tag<i32>),
    Advantage { l_paren: Tag<()>, roll: Box<Tag<TaggedDiceRoll>>, n: i32, r_paren: Tag<()> },
    Sum(Box<Tag<TaggedDiceRoll>>, Tag<()>, Box<Tag<TaggedDiceRoll>>),
    Product(Box<Tag<TaggedDiceRoll>>, Tag<()>, Box<Tag<TaggedDiceRoll>>),
    Multi(Tag<i32>, Box<Tag<TaggedDiceRoll>>)
}

impl TaggedDiceRoll {
    // pub fn loc(&self) -> Range {}

    pub fn as_roll(&self) -> DiceRoll {
        match self {
            TaggedDiceRoll::Simple(_, r) => DiceRoll::Simple(1..=r.item),
            TaggedDiceRoll::Constant(v) => DiceRoll::Constant(v.item),
            TaggedDiceRoll::Advantage { roll, n, ..  } => DiceRoll::Advantage(Box::new(roll.as_roll()), *n),
            TaggedDiceRoll::Sum(a, _, b) => DiceRoll::Sum(Box::new(a.as_roll()), Box::new(b.as_roll())),
            TaggedDiceRoll::Product(a, _, b) => DiceRoll::Sum(Box::new(a.as_roll()), Box::new(b.as_roll())),
            TaggedDiceRoll::Multi(n, v) => {
                let mut out = DiceRoll::Constant(0);
                for _ in 0..n.item {
                    out = DiceRoll::Sum(Box::new(out), Box::new(v.as_roll()))
                }
                out
            }
        }
    }
}

impl DiceRoll {
    pub fn distribution(&self) -> DiceDistr {
        match self {
            DiceRoll::Simple(v) => DiceDistr { range: v.clone(), distr: vec![1.0; v.clone().count()] },
            DiceRoll::Constant(v) => DiceDistr { range: *v..=*v, distr: vec![1.0] },
            DiceRoll::Advantage(d, count) => {
                let mut distrs = vec![];
                for _ in 0..count.abs() {
                    distrs.push(d.distribution())
                }
                if *count < 0 {
                    distrs.iter().fold(d.distribution(), |a, b| DiceDistr::multidimensional(vec![&a, b], |values| *values.iter().min().unwrap()))
                } else {
                    distrs.iter().fold(d.distribution(), |a, b| DiceDistr::multidimensional(vec![&a, b], |values| *values.iter().max().unwrap()))
                }
            }
            DiceRoll::Sum(a, b) => DiceDistr::multidimensional(vec![&a.distribution(), &b.distribution()], |values| values.iter().sum()),
            DiceRoll::Product(a, b) => DiceDistr::multidimensional(vec![&a.distribution(), &b.distribution()], |values| values.iter().product()),
        }
    }
}
