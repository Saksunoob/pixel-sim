use bevy::math::IVec2;
use rayon::prelude::*;
use serde::{Deserialize, Deserializer, Serialize};
use std::ops::{Add, Div, Mul, Sub};

use crate::{hash, world::Elements};

#[derive(Debug, Copy, Clone, Serialize)]
pub enum TagValue {
    None,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Element(u64),
}
impl TagValue {
    pub fn as_float(&self) -> f64 {
        match self {
            TagValue::None => 0.,
            TagValue::Integer(i) => *i as f64,
            TagValue::Float(f) => *f,
            TagValue::Boolean(v) => *v as u64 as f64,
            TagValue::Element(el) => *el as f64,
        }
    }
    pub fn to_string(&self, elements: &Elements) -> String {
        match self {
            TagValue::None => "None".to_string(),
            TagValue::Integer(i) => format!("{i}"),
            TagValue::Float(f) => format!("{f}"),
            TagValue::Boolean(b) => if *b {"True".to_string()} else {"False".to_string()},
            TagValue::Element(el) => elements.get(*el).name,
        }.to_string()
    }
}
impl PartialEq for TagValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Element(l0), Self::Element(r0)) => l0 == r0,
            (Self::None, Self::None) => true,
            _ => false,
        }
    }
}
impl PartialOrd for TagValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (TagValue::Integer(i1), TagValue::Integer(i2)) => Some(i1.cmp(i2)),
            (TagValue::Float(f1), TagValue::Float(f2)) => Some(f1.total_cmp(f2)),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => Some(b1.cmp(b2)),
            (TagValue::Float(f1), TagValue::Integer(i2)) => Some(f1.total_cmp(&(*i2 as f64))),
            (TagValue::Integer(i1), TagValue::Float(f2)) => Some((*i1 as f64).total_cmp(f2)),
            _ => None,
        }
    }
}
impl Add for TagValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,

            (TagValue::None, other) | (other, TagValue::None) => other,
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1 + v2),
            (TagValue::Integer(v2), TagValue::Float(v1))
            | (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1 + v2 as f64),
            (TagValue::Boolean(v2), TagValue::Float(v1))
            | (TagValue::Float(v1), TagValue::Boolean(v2)) => {
                TagValue::Float(v1 + v2 as i64 as f64)
            }
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1 + v2),
            (TagValue::Boolean(b), TagValue::Integer(i))
            | (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i + b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 | b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => {
                panic!("Cannot do arithmatic with Tag value of type Element")
            }
        }
    }
}
impl Sub for TagValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (other, TagValue::None) => other,
            (TagValue::None, TagValue::Float(f)) => TagValue::Float(-f),
            (TagValue::None, TagValue::Integer(i)) => TagValue::Integer(-i),
            (TagValue::None, TagValue::Boolean(b)) => TagValue::Boolean(!b),
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1 - v2),
            (TagValue::Integer(v1), TagValue::Float(v2)) => TagValue::Float(v1 as f64 - v2),
            (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1 - v2 as f64),
            (TagValue::Boolean(v1), TagValue::Float(v2)) => TagValue::Float(v1 as i64 as f64 - v2),
            (TagValue::Float(v1), TagValue::Boolean(v2)) => TagValue::Float(v1 - v2 as i64 as f64),
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1 - v2),
            (TagValue::Boolean(b), TagValue::Integer(i)) => TagValue::Integer(b as i64 - i),
            (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i - b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 & !b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => {
                panic!("Cannot do arithmatic with Tag value of type Element")
            }
        }
    }
}
impl Mul for TagValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (_, TagValue::None) => TagValue::None,
            (TagValue::None, TagValue::Float(_)) => TagValue::Float(0.),
            (TagValue::None, TagValue::Integer(_)) => TagValue::Integer(0),
            (TagValue::None, TagValue::Boolean(_)) => TagValue::Boolean(false),
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1 * v2),
            (TagValue::Integer(v2), TagValue::Float(v1))
            | (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1 * v2 as f64),
            (TagValue::Boolean(v2), TagValue::Float(v1))
            | (TagValue::Float(v1), TagValue::Boolean(v2)) => {
                TagValue::Float(v1 * v2 as i64 as f64)
            }
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1 * v2),
            (TagValue::Boolean(b), TagValue::Integer(i))
            | (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i * b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 & b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => {
                panic!("Cannot do arithmatic with Tag value of type Element")
            }
        }
    }
}
impl Div for TagValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (_, TagValue::None) => TagValue::None,
            (TagValue::None, TagValue::Float(_)) => TagValue::Float(0.),
            (TagValue::None, TagValue::Integer(_)) => TagValue::Integer(0),
            (TagValue::None, TagValue::Boolean(_)) => TagValue::Boolean(false),
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1 / v2),
            (TagValue::Integer(v1), TagValue::Float(v2)) => TagValue::Float(v1 as f64 / v2),
            (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1 / v2 as f64),
            (TagValue::Boolean(v1), TagValue::Float(v2)) => TagValue::Float(v1 as i64 as f64 / v2),
            (TagValue::Float(v1), TagValue::Boolean(v2)) => TagValue::Float(v1 / v2 as i64 as f64),
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1 * v2),
            (TagValue::Boolean(b), TagValue::Integer(i)) => TagValue::Integer(b as i64 * i),
            (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i * b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 ^ b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => {
                panic!("Cannot do arithmatic with Tag value of type Element")
            }
        }
    }
}
impl<'de> Deserialize<'de> for TagValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct TagValueVisitor;

        impl<'de> serde::de::Visitor<'de> for TagValueVisitor {
            type Value = TagValue;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a valid TagValue variant")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                return Ok(TagValue::Integer(v));
            }
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_i64(v as i64)
            }
            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(TagValue::Float(v))
            }
            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(TagValue::Boolean(v))
            }

            // Deserialize based on the type of the value
            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if value.starts_with('#') {
                    let r = u8::from_str_radix(&value[1..3], 16);
                    let g = u8::from_str_radix(&value[3..5], 16);
                    let b = u8::from_str_radix(&value[5..7], 16);
                    if let (Ok(r), Ok(g), Ok(b)) = (r, g, b) {
                        return Ok(TagValue::Integer(color_to_int(r, g, b)));
                    } else {
                        eprintln!("invalid color tag {value}")
                    }
                }
                Ok(TagValue::Element(hash(value)))
            }
            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(TagValue::None)
            }
        }
        deserializer.deserialize_any(TagValueVisitor)
    }
}

const fn color_to_int(r: u8, g: u8, b: u8) -> i64 {
    return ((r as i64) << 16) + ((g as i64) << 8) + (b as i64);
}

#[derive(Clone)]
pub struct TagSpace {
    array: Vec<TagValue>,
    world_size: i32,
}
impl TagSpace {
    pub fn new_with_value(value: TagValue, world_size: i32) -> Self {
        Self {
            array: vec![value; (world_size * world_size) as usize],
            world_size,
        }
    }
    pub fn get_tag(&self, pos: IVec2) -> TagValue {
        let index = pos.y * self.world_size + pos.x;
        *self.array.get(index as usize).unwrap_or(&TagValue::None)
    }
    pub fn get_rel_tag(&self, origin: IVec2, pos: IVec2) -> TagValue {
        let pos = origin.wrapping_add(pos);
        if pos.x < 0 || pos.y < 0 || pos.x >= self.world_size || pos.y >= self.world_size {
            return TagValue::None;
        }

        let index = pos.y * self.world_size + pos.x;
        *self.array.get(index as usize).unwrap_or(&TagValue::None)
    }
    pub fn get_tag_at_index(&self, index: i32) -> TagValue {
        if index < 0 {
            return TagValue::None;
        }
        *self.array.get(index as usize).unwrap_or(&TagValue::None)
    }
    pub fn par_iter_mut(&mut self) -> rayon::slice::IterMut<'_, TagValue> {
        self.array.par_iter_mut()
    }
}
