use std::{collections::HashMap, time::Instant};

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
    pub rules: Vec<Rule>,
}

impl Ruleset {
    pub fn new(rules: Vec<Rule>) -> Self {
        Self { rules }
    }

    pub fn execute_rules(
        &self,
        tiles: &mut HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) {
        let start = Instant::now();
        self.rules.iter().for_each(|rule| {
            rule.execute(tiles, world_size, elements, frame, input)
        });
        println!("{}ms", start.elapsed().as_millis());
    }
    pub fn set_enabled(&mut self, index: (usize, Option<usize>), value: bool) {
        // Get rule
        let rule = self.rules.get_mut(index.0);
        // Check if first index was valid
        if let Some(rule) = rule {
            // See if we should check for children
            match index.1 {
                // Child index exists
                Some(inner_index) => {
                    // Check that the initial rule is a compound
                    if let Rule::Compound { rules, .. } = rule {
                        let rule = rules.get_mut(inner_index);
                        // Check if second index was valid
                        if let Some(rule) = rule {
                            rule.set_enabled(value);
                        }
                    }
                },
                None => rule.set_enabled(value),
            }
        }
    }
    pub fn get_enabled(&mut self, index: (usize, Option<usize>)) -> Option<bool> {
        // Get rule
        let rule = self.rules.get(index.0)?;

        // If parent is disabled then so are children, so further checking is pointless
        if !rule.get_enabled() {
            return Some(false);
        }
        // See if we should check for children based on index
        match index.1 {
            // Child index exists
            Some(inner_index) => {
                // Check that the initial rule is a compound
                if let Rule::Compound { rules, .. } = rule {
                    let rule = rules.get(inner_index)?;
                    // Check if second index was valid
                    return Some(rule.get_self_enabled())
                }
                return None
            },
            None => Some(rule.get_enabled()),
        }
    }
    pub fn get_self_enabled(&mut self, index: (usize, Option<usize>)) -> Option<bool> {
        // Get rule
        let rule = self.rules.get(index.0)?;
        // See if we should check for children
        match index.1 {
            // Child index exists
            Some(inner_index) => {
                // Check that the initial rule is a compound
                if let Rule::Compound { rules, .. } = rule {
                    let rule = rules.get(inner_index)?;
                    // Check if second index was valid
                    return Some(rule.get_self_enabled())
                }
                return None
            },
            None => Some(rule.get_enabled()),
        }
    }
}

#[derive(Clone)]
pub enum ActionDataType {
    Clone(IVec2),
    ReplaceTags(Vec<(String, TagValue)>),
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

#[derive(Clone)]
pub enum Rule {
    Single(SingleRule),
    Compound{
        name: String,
        enabled: bool,
        rules: Vec<SingleRule>
    }
}
impl Rule {
    pub fn execute(
        &self,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) {
        (0..world_size.pow(2)).into_par_iter().for_each(|index| {
            let x = index%world_size;
            let y = index/world_size;

            self.execute_at(IVec2::new(x, y), tiles, world_size, elements, frame, input);
        })
    }

    fn execute_at(
        &self,
        pos: IVec2,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) {
        let actions = match self {
            Rule::Single(rule) => rule.get_actions(pos, tiles, world_size, elements, frame, input),
            Rule::Compound{enabled, rules, ..} => {
                if *enabled {
                    rules.iter().flat_map(|rule| rule.get_actions(pos, tiles, world_size, elements, frame, input)).collect()
                } else {
                    Vec::new()
                }
            },
        };

        let mut max_priority: (f64, Option<&Action>) = (f64::NEG_INFINITY, None);
        actions.iter().for_each(|action| {
            if action.priority > max_priority.0 {
                max_priority = (action.priority, Some(action))
            }
        });
        if let (_, Some(action)) = max_priority {
            let new_values: Vec<(IVec2, Vec<(&String, TagValue)>)> = action.new_states.iter().map(|(pos, action_type)| {
                match action_type {
                    ActionDataType::Clone(from) => {
                        (*pos, tiles.iter().map(|(name, tag_space)| {
                            (name, tag_space.get_tag(*from))
                        }).collect())
                    },
                    ActionDataType::ReplaceTags(tags) => {
                        (*pos, tags.iter().map(|(name, value)| {
                            (name, *value)
                        }).collect())
                    },
                    ActionDataType::ReplaceAllTags(tags) => {
                        (*pos, tiles.iter().map(|(name, _)| {
                            (name, *tags.get(name).unwrap_or(&TagValue::None))
                        }).collect())
                    }
                }
            }).collect();

            new_values.iter().for_each(|(pos, tags)| {
                tags.iter().for_each(|(name, value)| {
                    if let Some(tag_space) = tiles.get(*name) {
                        tag_space.set_tag(*pos, *value)
                    }
                })
            })
        }
    }
    fn set_enabled(&mut self, value: bool) {
        match self {
            Rule::Single(rule) => rule.enabled = value,
            Rule::Compound { enabled, rules, .. } => {
                *enabled = value;
                rules.iter_mut().for_each(|child| child.set_parent_enabled(value));
            },
        }
    }
    fn get_enabled(&self) -> bool {
        match self {
            Rule::Single(rule) => rule.enabled,
            Rule::Compound { enabled, .. } => *enabled
        }
    }
    pub fn get_name(&self) -> &str {
        match self {
            Rule::Single(rule) => &rule.name,
            Rule::Compound { name, .. } => name,
        }
    }
}
#[derive(Clone)]
pub struct SingleRule {
    name: String,
    enabled: bool,
    parent_enabled: bool,
    condition: Condition,
    rule_outcome: Vec<(IVec2, RuleOutcome)>,
    priority: Math
}

impl SingleRule {

    pub fn new(name: impl Into<String>, condition: Condition, rule_outcome: Vec<(IVec2, RuleOutcome)>, priority: Math) -> Self {
        Self {
            name: name.into(),
            enabled: true,
            parent_enabled: true,
            condition,
            rule_outcome,
            priority,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_enabled(&self) -> bool {
        self.enabled && self.parent_enabled
    }

    pub fn get_self_enabled(&self) -> bool {
        self.enabled
    }

    pub fn set_enabled(&mut self, value: bool) {
        self.enabled = value;
    }

    fn set_parent_enabled(&mut self, value: bool) {
        self.parent_enabled = value;
    }

    fn get_actions(
        &self,
        pos: IVec2,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) -> Vec<Action> {
        if !self.condition.evaluate(pos, tiles, world_size, input) || !self.get_enabled() {
            return Vec::new();
        }

        vec![Action::new(
            self.rule_outcome
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
            self.priority.evaluate(pos, tiles, world_size).as_float(),
        )]
    }
}