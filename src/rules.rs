use std::{collections::HashMap, sync::RwLock};

use bevy::{
    ecs::system::Res,
    input::{keyboard::ScanCode, Input},
    math::IVec2,
};
use rand::random;
use rayon::prelude::*;
use serde::Deserialize;
use serde_json::Value;

use crate::{
    tags::{TagSpace, TagValue},
    world::Elements,
};

#[derive(Clone)]
pub struct Ruleset {
    pub rules: Vec<RuleType>,
}

impl Ruleset {
    pub fn new(rules: Vec<RuleType>) -> Self {
        Self { rules }
    }
    pub fn get_index(&self, index: (usize, usize)) -> Option<&RuleType> {
        let rule = self.rules.get(index.0)?;
        match rule {
            RuleType::Rule { .. } => Some(rule),
            RuleType::CompoundRule { rules, .. } => {
                if index.1 == 0 {
                    Some(rule)
                } else {
                    rules.get(index.1 - 1)
                }
            }
        }
    }
    pub fn get_index_mut(&mut self, index: (usize, usize)) -> Option<&mut RuleType> {
        let rule = self.rules.get_mut(index.0)?;
        if index.1 == 0 {
            return Some(rule);
        } else {
            match rule {
                RuleType::CompoundRule { rules, .. } => rules.get_mut(index.1 - 1),
                _ => None,
            }
        }
    }

    pub fn execute_rules(
        &self,
        tiles: &mut HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) {
        self.rules.iter().for_each(|rule| {
            if rule.enabled() {
                self.execute_rule(rule, tiles, world_size, elements, frame, input);
            }
        })
    }

    pub fn execute_rule(
        &self,
        rule: &RuleType,
        tiles: &mut HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) {
        let mut actions: Vec<Action> = (0..world_size * world_size)
            .into_par_iter()
            .flat_map(|index| {
                let x = index % world_size;
                let y = index / world_size;
                rule.execute(IVec2::new(x, y), tiles, world_size, elements, frame, input)
            })
            .collect();

        actions.par_sort_unstable_by(|a, b| a.priority.partial_cmp(&b.priority).unwrap().reverse());

        let affected: Vec<RwLock<bool>> = (0..(world_size * world_size))
            .into_par_iter()
            .map(|_| RwLock::from(false))
            .collect();

        let is_affected = |index: &IVec2| -> bool {
            if index.x < 0 || index.y < 0 || index.x >= world_size || index.y >= world_size {
                return true;
            }
            *affected[get_index(&index, world_size)].read().unwrap()
        };

        let actions: HashMap<usize, ActionDataType> = actions
            .into_iter()
            .filter_map(|action| {
                // Find out a way to make this parrallel while preserving priority
                if action.get_affected().iter().any(|index| is_affected(index)) {
                    return None;
                }
                action.get_affected().iter().for_each(|index| {
                    *affected[get_index(index, world_size)].write().unwrap() = true
                });
                Some(
                    action
                        .new_states
                        .iter()
                        .map(|(pos, data)| (get_index(pos, world_size), data.clone()))
                        .collect::<Vec<_>>(),
                )
            })
            .flatten()
            .collect();

        let old_tiles = tiles.clone();

        tiles.into_par_iter().for_each(|(tag_name, value_vec)| {
            value_vec
                .par_iter_mut()
                .enumerate()
                .filter(|(i, _)| *affected[*i].read().unwrap())
                .for_each(|(i, tag_value)| {
                    if let Some(action) = actions.get(&i) {
                        match action {
                            ActionDataType::Clone(from) => {
                                *tag_value = old_tiles.get(tag_name).unwrap().get_tag(*from)
                            }
                            ActionDataType::ReplaceTags(new_tags) => {
                                if let Some(new_value) = new_tags.get(tag_name) {
                                    *tag_value = *new_value
                                }
                            }
                            ActionDataType::ReplaceAllTags(new_tags) => {
                                *tag_value =
                                    new_tags.get(tag_name).copied().unwrap_or(TagValue::None)
                            }
                        }
                    }
                });
        });
    }
}

#[derive(Clone)]
pub enum ActionDataType {
    Clone(IVec2),
    ReplaceTags(HashMap<String, TagValue>),
    ReplaceAllTags(HashMap<String, TagValue>),
}

#[derive(Clone)]
pub struct Action {
    new_states: Vec<(IVec2, ActionDataType)>,
    priority: f64,
}
impl Action {
    pub fn new(states: Vec<(IVec2, ActionDataType)>, priority: f64) -> Self {
        Self {
            new_states: states,
            priority,
        }
    }
    pub fn get_affected(&self) -> Vec<IVec2> {
        self.new_states.iter().map(|(pos, _)| *pos).collect()
    }
}

#[derive(Clone, Debug, Deserialize)]
pub enum ConditionType {
    Random(f64),
    Input(u32),
    Is(IVec2, String, TagValue),
    Eq((IVec2, IVec2), String),
    Lt((IVec2, IVec2), String),
    Lte((IVec2, IVec2), String),
    Gt((IVec2, IVec2), String),
    Gte((IVec2, IVec2), String),
}

#[derive(Clone, Debug)]
pub enum Condition {
    And(Vec<Condition>),
    Or(Vec<Condition>),
    Condition(ConditionType),
}

impl Condition {
    pub fn evaluate(
        &self,
        origin: IVec2,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
        input: &Res<Input<ScanCode>>,
    ) -> bool {
        match self {
            Condition::And(conditions) => conditions
                .iter()
                .all(|condition| condition.evaluate(origin, tiles, world_size, input)),
            Condition::Or(conditions) => conditions
                .iter()
                .any(|condition| condition.evaluate(origin, tiles, world_size, input)),
            Condition::Condition(condition) => match condition {
                ConditionType::Random(chance) => random::<f64>() <= *chance,
                ConditionType::Input(scancode) => input.pressed(ScanCode(*scancode)),
                ConditionType::Is(pos, tag, value) => {
                    tiles.get(tag).unwrap().get_rel_tag(origin, *pos) == *value
                }
                ConditionType::Eq((pos1, pos2), tag) => {
                    let vec = tiles.get(tag).unwrap();
                    vec.get_rel_tag(origin, *pos1) == vec.get_rel_tag(origin, *pos2)
                }
                ConditionType::Lt((pos1, pos2), tag) => {
                    let vec = tiles.get(tag).unwrap();
                    vec.get_rel_tag(origin, *pos1) < vec.get_rel_tag(origin, *pos2)
                }
                ConditionType::Lte((pos1, pos2), tag) => {
                    let vec = tiles.get(tag).unwrap();
                    vec.get_rel_tag(origin, *pos1) <= vec.get_rel_tag(origin, *pos2)
                }
                ConditionType::Gt((pos1, pos2), tag) => {
                    let vec = tiles.get(tag).unwrap();
                    vec.get_rel_tag(origin, *pos1) > vec.get_rel_tag(origin, *pos2)
                }
                ConditionType::Gte((pos1, pos2), tag) => {
                    let vec = tiles.get(tag).unwrap();
                    vec.get_rel_tag(origin, *pos1) >= vec.get_rel_tag(origin, *pos2)
                }
            },
        }
    }
    pub fn from_value(value: &Value) -> Option<Self> {
        let value = value.as_array()?;

        let condition_type = value[0].as_str()?;

        match condition_type {
            "And" => Some(Condition::And(
                value[1]
                    .as_array()?
                    .into_iter()
                    .filter_map(|condition| Condition::from_value(condition))
                    .collect(),
            )),
            "Or" => Some(Condition::Or(
                value[1]
                    .as_array()?
                    .into_iter()
                    .filter_map(|condition| Condition::from_value(condition))
                    .collect(),
            )),
            "Eq" => Some(Condition::Condition(ConditionType::Eq(
                (value_as_ivec(&value[1])?, value_as_ivec(&value[2])?),
                value[3].as_str()?.to_string(),
            ))),
            "Lt" => Some(Condition::Condition(ConditionType::Lt(
                (value_as_ivec(&value[1])?, value_as_ivec(&value[2])?),
                value[3].as_str()?.to_string(),
            ))),
            "Lte" => Some(Condition::Condition(ConditionType::Lte(
                (value_as_ivec(&value[1])?, value_as_ivec(&value[2])?),
                value[3].as_str()?.to_string(),
            ))),
            "Gt" => Some(Condition::Condition(ConditionType::Gt(
                (value_as_ivec(&value[1])?, value_as_ivec(&value[2])?),
                value[3].as_str()?.to_string(),
            ))),
            "Gte" => Some(Condition::Condition(ConditionType::Gte(
                (value_as_ivec(&value[1])?, value_as_ivec(&value[2])?),
                value[3].as_str()?.to_string(),
            ))),
            "Is" => Some(Condition::Condition(ConditionType::Is(
                value_as_ivec(&value[1])?,
                value[2].as_str()?.to_string(),
                TagValue::deserialize(&value[3]).unwrap_or(TagValue::None),
            ))),
            "Input" => Some(Condition::Condition(ConditionType::Input(
                value[1].as_u64()? as u32,
            ))),
            "Random" => Some(Condition::Condition(ConditionType::Random(
                value[1].as_f64()?,
            ))),
            _ => None,
        }
    }
}

fn value_as_ivec(value: &Value) -> Option<IVec2> {
    let arr = value.as_array()?;
    Some(IVec2::new(arr[0].as_i64()? as i32, arr[1].as_i64()? as i32))
}

#[derive(Clone, Debug)]
pub enum RuleOutcome {
    Clone(IVec2),
    ChangeTags(Vec<(String, Math)>),
    SetTags(Vec<(String, TagValue)>),
    SetElement(Math),
}
impl RuleOutcome {
    pub fn from_value(value: &Value) -> Option<Vec<(IVec2, Self)>> {
        let outcomes = value.as_array()?;

        Some(
            outcomes
                .into_iter()
                .filter_map(|outcome| {
                    let elements = outcome.as_array()?;
                    let target = value_as_ivec(&elements[0])?;

                    let outcome = match elements[1].as_str()? {
                        "Clone" => Some(Self::Clone(value_as_ivec(&elements[2])?)),
                        "ChangeTags" => Some(Self::ChangeTags(
                            elements[2]
                                .as_array()?
                                .into_iter()
                                .filter_map(|change| {
                                    let arr = change.as_array()?;
                                    let tag = arr[0].as_str()?.to_string();
                                    let new_value = Math::from_value(&arr[1])?;

                                    Some((tag, new_value))
                                })
                                .collect(),
                        )),
                        "SetTags" => Some(Self::SetTags(
                            elements[2]
                                .as_array()?
                                .into_iter()
                                .filter_map(|change| {
                                    let arr = change.as_array()?;
                                    let tag = arr[0].as_str()?.to_string();
                                    let new_value = TagValue::deserialize(&arr[1]).ok()?;
                                    Some((tag, new_value))
                                })
                                .collect(),
                        )),
                        "SetElement" => Some(Self::SetElement(Math::from_value(&elements[2])?)),
                        _ => None,
                    };

                    Some((target, outcome?))
                })
                .collect(),
        )
    }
}

#[derive(Clone, Debug, Deserialize)]
pub enum Math {
    Minus(Box<Math>, Box<Math>),
    Plus(Box<Math>, Box<Math>),
    Div(Box<Math>, Box<Math>),
    Mul(Box<Math>, Box<Math>),
    Tag(IVec2, String),
    Value(TagValue),
}
impl Math {
    pub fn evaluate(
        &self,
        pos: IVec2,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
    ) -> TagValue {
        match self {
            Math::Minus(a, b) => {
                a.evaluate(pos, tiles, world_size) - b.evaluate(pos, tiles, world_size)
            }
            Math::Plus(a, b) => {
                a.evaluate(pos, tiles, world_size) + b.evaluate(pos, tiles, world_size)
            }
            Math::Div(a, b) => {
                a.evaluate(pos, tiles, world_size) / b.evaluate(pos, tiles, world_size)
            }
            Math::Mul(a, b) => {
                a.evaluate(pos, tiles, world_size) * b.evaluate(pos, tiles, world_size)
            }
            Math::Tag(rel_pos, name) => tiles.get(name).unwrap().get_rel_tag(pos, *rel_pos),
            Math::Value(value) => *value,
        }
    }
    pub fn from_value(value: &Value) -> Option<Self> {
        let elements = value.as_array().cloned().unwrap_or(vec![value.clone()]);

        if elements.len() == 1 {
            return Some(Math::Value(TagValue::deserialize(&elements[0]).ok()?));
        }
        if elements.len() == 2 && elements[1].is_string() {
            return Some(Math::Tag(
                value_as_ivec(&elements[0])?,
                elements[1].as_str()?.to_string(),
            ));
        }
        match elements[1].as_str()? {
            "+" => Some(Math::Plus(
                Box::new(Math::from_value(&elements[0])?),
                Box::new(Math::from_value(&elements[2])?),
            )),
            "-" => Some(Math::Minus(
                Box::new(Math::from_value(&elements[0])?),
                Box::new(Math::from_value(&elements[2])?),
            )),
            "*" => Some(Math::Mul(
                Box::new(Math::from_value(&elements[0])?),
                Box::new(Math::from_value(&elements[2])?),
            )),
            "/" => Some(Math::Div(
                Box::new(Math::from_value(&elements[0])?),
                Box::new(Math::from_value(&elements[2])?),
            )),
            _ => None,
        }
    }
}

fn get_index(pos: &IVec2, world_size: i32) -> usize {
    (pos.y * world_size + pos.x) as usize
}

#[derive(Clone, Debug)]
pub enum RuleType {
    Rule {
        name: String,
        enabled: bool,
        parent_enabled: bool,
        condition: Condition,
        rule_outcome: Vec<(IVec2, RuleOutcome)>,
        priority: Math,
    },
    CompoundRule {
        name: String,
        enabled: bool,
        parent_enabled: bool,
        rules: Vec<RuleType>,
    },
}
impl RuleType {
    pub fn get_name(&self) -> &str {
        match self {
            RuleType::Rule { name, .. } => &name,
            RuleType::CompoundRule { name, .. } => &name,
        }
    }

    pub fn enabled(&self) -> bool {
        match self {
            RuleType::Rule {
                enabled,
                parent_enabled,
                ..
            } => *enabled && *parent_enabled,
            RuleType::CompoundRule {
                enabled,
                parent_enabled,
                ..
            } => *enabled && *parent_enabled,
        }
    }

    pub fn self_enabled(&self) -> bool {
        match self {
            RuleType::Rule { enabled, .. } => *enabled,
            RuleType::CompoundRule { enabled, .. } => *enabled,
        }
    }

    pub fn set_enabled(&mut self, value: bool) {
        match self {
            RuleType::Rule { enabled, .. } => *enabled = value,
            RuleType::CompoundRule {
                enabled,
                parent_enabled,
                rules,
                ..
            } => {
                *enabled = value;
                rules
                    .iter_mut()
                    .for_each(|child| child.set_parent_enabled(*parent_enabled && *enabled))
            }
        }
    }
    fn set_parent_enabled(&mut self, value: bool) {
        match self {
            RuleType::Rule { parent_enabled, .. } => *parent_enabled = value,
            RuleType::CompoundRule {
                parent_enabled,
                enabled,
                rules,
                ..
            } => {
                *parent_enabled = value;
                rules
                    .iter_mut()
                    .for_each(|child| child.set_parent_enabled(*parent_enabled && *enabled))
            }
        }
    }

    pub fn execute(
        &self,
        pos: IVec2,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) -> Vec<Action> {
        match self {
            RuleType::Rule {
                condition,
                rule_outcome,
                priority,
                ..
            } => {
                if condition.evaluate(pos, tiles, world_size, input) {
                    return vec![Action::new(
                        rule_outcome
                            .iter()
                            .map(|(rel_pos, rule_outcome)| {
                                (
                                    pos.wrapping_add(*rel_pos),
                                    match rule_outcome {
                                        RuleOutcome::Clone(from) => {
                                            ActionDataType::Clone(pos.wrapping_add(*from))
                                        }
                                        RuleOutcome::ChangeTags(tags) => {
                                            ActionDataType::ReplaceTags(
                                                tags.iter()
                                                    .map(|(tag, math)| {
                                                        (
                                                            tag.to_string(),
                                                            math.evaluate(pos, tiles, world_size),
                                                        )
                                                    })
                                                    .collect(),
                                            )
                                        }
                                        RuleOutcome::SetTags(tags) => ActionDataType::ReplaceTags(
                                            tags.iter().cloned().collect(),
                                        ),
                                        RuleOutcome::SetElement(el) => {
                                            ActionDataType::ReplaceAllTags(
                                                elements
                                                    .get_el(el.evaluate(pos, tiles, world_size))
                                                    .iter()
                                                    .cloned()
                                                    .collect(),
                                            )
                                        }
                                    },
                                )
                            })
                            .collect(),
                        priority.evaluate(pos, tiles, world_size).as_float(),
                    )];
                }
                Vec::new()
            }
            RuleType::CompoundRule { rules, .. } => rules
                .iter()
                .filter_map(|rule| {
                    if rule.enabled() {
                        return Some(rule.execute(pos, tiles, world_size, elements, frame, input));
                    }
                    None
                })
                .flatten()
                .collect(),
        }
    }
}
