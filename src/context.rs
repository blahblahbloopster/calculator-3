use rug::{float::Round, Complex};

use crate::units::{UV, UnitHolder, UnitTree};

pub struct ContextItem {
    pub uv_metric: UV,
    pub format: String
}

impl ContextItem {
    pub fn new(holder: &UnitHolder, line: &str) -> ContextItem {
        let (num, format) = line.split_once(" - ").unwrap();
        let (value, unit) = num.split_once(" ").unwrap();
        let un = match holder.parse(unit) {
            Some(v) => v,
            None => panic!("failed to parse context item unit '{line}'"),
        };
        let uv = UV { unit: un, value: Complex::with_val(1024, Complex::parse(value).unwrap()) };

        ContextItem { uv_metric: uv.metricify(), format: format.to_string() }
    }

    pub fn format(&self, v: &UV, unit: &UnitTree) -> String {
        assert!(self.uv_metric.unit.compatible(&v.unit));
        let ratio = (v.clone().metricify() / self.uv_metric.clone()).unwrap();
        assert!(ratio.unit.is_none());
        let stringified = if ratio.value.imag().is_zero() {
            format!("{:.4}", ratio.value.real().to_f64())
        } else {
            ratio.value.to_string_radix_round(10, Some(4), (Round::Nearest, Round::Nearest))
        };
        format!("{} ({:.4} {})", self.format.clone().replace("$", &stringified), self.uv_metric.clone().convert(unit.clone()).unwrap().value.real().to_f64(), unit.to_string())
    }
}

pub struct UnitContext {
    items: Vec<ContextItem>
}

impl UnitContext {
    pub fn new(holder: &UnitHolder, file: &str) -> UnitContext {
        let mut items = vec![];
        for line in file.lines() {
            if line.is_empty() || line.starts_with("#") {
                continue;
            }
            items.push(ContextItem::new(holder, line));
        }
        UnitContext { items }
    }

    pub fn find(&self, v: UV, limit: usize) -> Vec<String> {
        let metricified = v.clone().metricify();
        let mut items = self.items.iter().filter(|x| x.uv_metric.unit.compatible(&v.unit)).map(|x| {
            let diff = (x.uv_metric.clone() / metricified.clone()).unwrap().value.log10();
            (diff, x)
        }).collect::<Vec<_>>();

        items.sort_by(|a, b| a.0.real().to_f64().abs().total_cmp(&b.0.real().to_f64().abs()));
        items.retain(|x| x.0.real().to_f64().abs() < 1.0);
        items.truncate(limit);
        items.sort_by(|a, b| a.1.uv_metric.value.total_cmp(&b.1.uv_metric.value));
        items.iter().map(|(_, context)| context.format(&v, &v.unit)).collect()
    }
}
