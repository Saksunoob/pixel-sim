use bevy::math::IVec2;
use serde_json::Value;
use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Sub},
};

use crate::world::{pos_in_world, Element, Elements};

#[derive(Debug, Copy, Clone)]
pub enum TagType {
    Integer,
    Float,
    Boolean,
    Element
}

#[derive(Debug, Copy, Clone)]
pub enum TagValue {
    None,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Element(usize),
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
            TagValue::Boolean(b) => {
                if *b {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            TagValue::Element(el) => elements.get(*el).name.to_string(),
        }
    }
    pub fn is_of_type(&self, tagtype: &TagType ) -> bool {
        match (self, tagtype) {
            (TagValue::None, _) => true,
            (TagValue::Integer(_), TagType::Integer) => true,
            (TagValue::Float(_), TagType::Float) => true,
            (TagValue::Boolean(_), TagType::Boolean) => true,
            (TagValue::Element(_), TagType::Element) => true,
            _ => false
        }
    }
    pub fn from_value(value: &Value, elements: &Elements) -> Self {
        match value {
            Value::Null => TagValue::None,
            Value::Bool(b) => TagValue::Boolean(*b),
            Value::Number(n) => {
                if let Some(int) = n.as_i64() {
                    return TagValue::Integer(int);
                }
                return TagValue::Float(n.as_f64().unwrap());
            }
            Value::String(str) => {
                if str.starts_with('#') {
                    let r = u8::from_str_radix(&str[1..3], 16);
                    let g = u8::from_str_radix(&str[3..5], 16);
                    let b = u8::from_str_radix(&str[5..7], 16);
                    if let (Ok(r), Ok(g), Ok(b)) = (r, g, b) {
                        return TagValue::Integer(color_to_int(r, g, b));
                    } else {
                        panic!("invalid color tag {str}")
                    }
                }
                if let Some(tag) = elements.get_index(str) {
                    return TagValue::Element(tag);
                } else {
                    panic!("invalid element tag {str}")
                }
            }
            Value::Array(_) => panic!("invalid tag value {value:?}"),
            Value::Object(_) => panic!("invalid tag value {value:?}"),
        }
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

const fn color_to_int(r: u8, g: u8, b: u8) -> i64 {
    return ((r as i64) << 16) + ((g as i64) << 8) + (b as i64);
}

#[derive(Clone)]
pub struct Tags {
    tag_mapping: HashMap<String, usize>,
    tag_types: Vec<TagType>,
}
impl Tags {
    pub fn new(tags: Vec<(impl ToString, TagType)>) -> Self {
        Self {
            tag_mapping: tags
                .iter()
                .enumerate()
                .map(|(index, (name, _))| (name.to_string(), index))
                .collect(),
            tag_types: tags.into_iter().map(|(_, value)| value).collect(),
        }
    }
    pub fn iter(&self) -> std::ops::Range<usize> {
        0..self.tag_mapping.len()
    }
    pub fn get_index(&self, str: impl ToString) -> Option<usize> {
        self.tag_mapping.get(&str.to_string()).copied()
    }
    pub fn get_name(&self, index: usize) -> Option<&String> {
        self.tag_mapping
            .iter()
            .find(|(_, tag_index)| **tag_index == index)
            .and_then(|(name, _)| Some(name))
    }
}

pub struct TagSpace {
    tag_type: TagType,
    array: Vec<TagValue>,
    world_size: usize,
}
impl TagSpace {
    fn get_index(&self, pos: IVec2) -> usize {
        (pos.y * self.world_size as i32 + pos.x) as usize
    }
    pub fn new_with_value(value: TagValue, tag_type: TagType, world_size: usize) -> Self {
        Self {
            tag_type,
            array: vec![value; (world_size * world_size) as usize],
            world_size,
        }
    }
    pub fn get_tag(&self, pos: IVec2) -> TagValue {
        if !pos_in_world(pos, self.world_size) {
            return TagValue::None;
        }
        let index = self.get_index(pos);
        self.array[index]
    }

    pub fn get_tag_at_index(&self, index: impl Into<i32>) -> TagValue {
        let index: i32 = index.into();
        if index < 0 || index > self.world_size.pow(2) as i32 {
            return TagValue::None;
        }
        self.array[index as usize]
    }
    pub fn set_tag(&mut self, pos: IVec2, value: TagValue) {
        if !value.is_of_type(&self.tag_type) {
            eprintln!(
                "Trying to set a tag of type {:?} to an invalid variant {:?}",
                self.tag_type, value
            );
            return;
        }
        if !pos_in_world(pos, self.world_size) {
            return;
        }
        let index = self.get_index(pos);
        self.array[index] = value;
    }
}

pub struct SimualtionState {
    state: Vec<TagSpace>,
    pub tags: Tags,
}
impl SimualtionState {
    pub fn new(tags: Tags, size: usize, default_element: &Element) -> Self {
        Self {
            state: tags
                .iter()
                .map(|tag| {
                    TagSpace::new_with_value(default_element.tags[tag], tags.tag_types[tag], size)
                })
                .collect(),
            tags,
        }
    }
    pub fn get_tag_at(&self, tag: &usize, pos: IVec2) -> TagValue {
        self.state[*tag].get_tag(pos)
    }
    pub fn get_tags_at(&self, pos: IVec2) -> Vec<TagValue> {
        self.state.iter().map(|space| space.get_tag(pos)).collect()
    }
    pub fn get_space(&self, space: usize) -> &TagSpace {
        &self.state[space]
    }
    pub fn set_tags_at(&mut self, pos: IVec2, new_tags: Vec<TagValue>) {
        self.state
            .iter_mut()
            .enumerate()
            .for_each(|(tag, space)| space.set_tag(pos, new_tags[tag]))
    }
}
