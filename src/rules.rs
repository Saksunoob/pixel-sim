use std::time::{Duration, Instant};

use bevy::{
    ecs::system::Res,
    input::{keyboard::ScanCode, Input},
    math::IVec2,
};
use rand::random;
use rayon::prelude::*;
use serde_json::Value;

use crate::{
    file_parser::ValueParseError, tags::{SimualtionState, TagValue, Tags}, world::Elements
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
        state: &mut SimualtionState,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) {
        let start = Instant::now();
        let mut times: (Duration, Duration) = (Duration::ZERO, Duration::ZERO);
        self.rules.iter().for_each(|rule| {
            let start_actions = Instant::now();
            let actions = rule.execute(state, world_size, elements, frame, input);
            times.0 += start_actions.elapsed();

            let mut modified = vec![false; (world_size * world_size) as usize];

            let get_index = |pos: IVec2| -> usize { (pos.y * world_size + pos.x) as usize };

            let start_modification = Instant::now();
            actions.into_iter().for_each(|pixel| {
                for action in pixel {
                    if action
                        .iter()
                        .any(|(pos, _)| *modified.get(get_index(*pos)).unwrap_or(&true))
                    {
                        continue;
                    }
                    for (pos, new_tags) in action {
                        state.set_tags_at(pos, new_tags);
                        modified[get_index(pos)] = true;
                    }
                }
            });

            times.1 += start_modification.elapsed();
        });
        println!(
            "{}ms ({}ms, {}ms)",
            start.elapsed().as_millis(),
            times.0.as_millis(),
            times.1.as_millis()
        );
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
                }
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
                    return Some(rule.get_self_enabled());
                }
                return None;
            }
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
                    return Some(rule.get_self_enabled());
                }
                return None;
            }
            None => Some(rule.get_enabled()),
        }
    }
}

#[derive(Clone)]
pub enum ActionDataType {
    Clone(IVec2),
    ReplaceTags(Vec<(usize, TagValue)>),
    ReplaceAllTags(Vec<TagValue>),
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

#[derive(Clone, Debug)]
pub enum ConditionType {
    Random(f64),
    Input(u32),
    Is(IVec2, usize, TagValue),
    Eq((IVec2, IVec2), usize),
    Lt((IVec2, IVec2), usize),
    Lte((IVec2, IVec2), usize),
    Gt((IVec2, IVec2), usize),
    Gte((IVec2, IVec2), usize),
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
        state: &SimualtionState,
        world_size: i32,
        input: &Res<Input<ScanCode>>,
    ) -> bool {
        match self {
            Condition::And(conditions) => conditions
                .iter()
                .all(|condition| condition.evaluate(origin, state, world_size, input)),
            Condition::Or(conditions) => conditions
                .iter()
                .any(|condition| condition.evaluate(origin, state, world_size, input)),
            Condition::Condition(condition) => match condition {
                ConditionType::Random(chance) => random::<f64>() <= *chance,
                ConditionType::Input(scancode) => input.pressed(ScanCode(*scancode)),
                ConditionType::Is(pos, tag, value) => {
                    state.get_tag_at(tag, origin + *pos) == *value
                }
                ConditionType::Eq((pos1, pos2), tag) => {
                    state.get_tag_at(tag, origin + *pos1) == state.get_tag_at(tag, origin + *pos2)
                }
                ConditionType::Lt((pos1, pos2), tag) => {
                    state.get_tag_at(tag, origin + *pos1) < state.get_tag_at(tag, origin + *pos2)
                }
                ConditionType::Lte((pos1, pos2), tag) => {
                    state.get_tag_at(tag, origin + *pos1) <= state.get_tag_at(tag, origin + *pos2)
                }
                ConditionType::Gt((pos1, pos2), tag) => {
                    state.get_tag_at(tag, origin + *pos1) > state.get_tag_at(tag, origin + *pos2)
                }
                ConditionType::Gte((pos1, pos2), tag) => {
                    state.get_tag_at(tag, origin + *pos1) >= state.get_tag_at(tag, origin + *pos2)
                }
            },
        }
    }
    pub fn from_value(value: &Value, tags: &Tags, elements: &Elements) -> Result<Self, ValueParseError> {
        let value = value.as_array().ok_or(ValueParseError::new("Condition should be an array"))?;

        let condition_type = value[0].as_str().ok_or(ValueParseError::new("First item of a condition array should be a String"))?;

        match condition_type {
            "And" => {
                let sub_condition_array = value[1].as_array().ok_or(ValueParseError::new("Second element of And condition should be an Array"))?;
                let mut sub_conditions = Vec::new();
                for sub_condition in sub_condition_array {
                    sub_conditions.push(Condition::from_value(sub_condition, tags, elements)?);
                }
                Ok(Condition::And(sub_conditions))
            },
            "Or" => {
                let sub_condition_array = value[1].as_array().ok_or(ValueParseError::new("Second element of Or condition should be an Array"))?;
                let mut sub_conditions = Vec::new();
                for sub_condition in sub_condition_array {
                    sub_conditions.push(Condition::from_value(sub_condition, tags, elements)?);
                }
                Ok(Condition::Or(sub_conditions))
            },
            "Eq" => {
                let pos1 = value_as_ivec(&value[1])?;
                let pos2 = value_as_ivec(&value[2])?;


                let compare_str = value[3].as_str().ok_or(ValueParseError::new("Fourth item of Eq condition should be a String"))?;

                let compare = tags.get_index(compare_str).ok_or(ValueParseError::new(format!("Eq comparison tag \"{compare_str}\" doesn't exist")))?;

                Ok(Condition::Condition(ConditionType::Eq((pos1, pos2), compare)))
            },
            "Lt" => {
                let pos1 = value_as_ivec(&value[1])?;
                let pos2 = value_as_ivec(&value[2])?;

                let compare_str = value[3].as_str().ok_or(ValueParseError::new("Fourth item of Lt condition should be a String"))?;

                let compare = tags.get_index(compare_str).ok_or(ValueParseError::new(format!("Lt comparison tag \"{compare_str}\" doesn't exist")))?;

                Ok(Condition::Condition(ConditionType::Lt((pos1, pos2), compare)))
            },
            "Lte" => {
                let pos1 = value_as_ivec(&value[1])?;
                let pos2 = value_as_ivec(&value[2])?;

                let compare_str = value[3].as_str().ok_or(ValueParseError::new("Fourth item of Lte condition should be a String"))?;

                let compare = tags.get_index(compare_str).ok_or(ValueParseError::new(format!("Lte comparison tag \"{compare_str}\" doesn't exist")))?;

                Ok(Condition::Condition(ConditionType::Lte((pos1, pos2), compare)))
            },
            "Gt" => {
                let pos1 = value_as_ivec(&value[1])?;
                let pos2 = value_as_ivec(&value[2])?;

                let compare_str = value[3].as_str().ok_or(ValueParseError::new("Fourth item of Gt condition should be a String"))?;

                let compare = tags.get_index(compare_str).ok_or(ValueParseError::new(format!("Gt comparison tag \"{compare_str}\" doesn't exist")))?;

                Ok(Condition::Condition(ConditionType::Gt((pos1, pos2), compare)))
            },
            "Gte" => {
                let pos1 = value_as_ivec(&value[1])?;
                let pos2 = value_as_ivec(&value[2])?;

                let compare_str = value[3].as_str().ok_or(ValueParseError::new("Fourth item of Gte condition should be a String"))?;

                let compare = tags.get_index(compare_str).ok_or(ValueParseError::new(format!("Gte comparison tag \"{compare_str}\" doesn't exist")))?;

                Ok(Condition::Condition(ConditionType::Gte((pos1, pos2), compare)))
            },
            "Is" => {
                let pos = value_as_ivec(&value[1])?;
                let compare_str = value[2].as_str().ok_or(ValueParseError::new("Thrird item of Is condition should be a String"))?;
                let compare = tags.get_index(compare_str).ok_or(ValueParseError::new(format!("Is comparison tag \"{compare_str}\" doesn't exist")))?;
                let tag_value = TagValue::from_value(&value[3], elements);

                Ok(Condition::Condition(ConditionType::Is(pos, compare, tag_value)))
            },
            "Input" => {
                let scancode = value[1].as_u64().and_then(|code| code.try_into().ok()).ok_or(ValueParseError::new("Second element of Input should be i32 (-2147483648 - 2147483647)"))?;
                Ok(Condition::Condition(ConditionType::Input(scancode)))
            },
            "Random" => {
                let chance = value[1].as_f64().ok_or(ValueParseError::new("Second element of Random should be f64"))?;
                Ok(Condition::Condition(ConditionType::Random(chance)))
            },
            _ => Err(ValueParseError::new(format!("\"{}\" isn't a valid condition type", condition_type))),
        }
    }
}

fn value_as_ivec(value: &Value) -> Result<IVec2, ValueParseError> {
    let arr = value.as_array().ok_or(ValueParseError::new("Position should be an array"))?;
    if arr.len() != 2 {return Err(ValueParseError::new("Position array length should be 2"))}
    let x: i32 = arr[0].as_i64().and_then(|num| num.try_into().ok()).ok_or(ValueParseError::new("Positions arguments should be i32 (-2147483648 - 2147483647)"))?;
    let y: i32 = arr[1].as_i64().and_then(|num| num.try_into().ok()).ok_or(ValueParseError::new("Positions arguments should be i32 (-2147483648 - 2147483647)"))?;
    Ok(IVec2::new(x, y))
}

#[derive(Clone, Debug)]
pub enum RuleOutcome {
    Clone(IVec2),
    ChangeTags(Vec<(usize, Math)>),
    SetTags(Vec<(usize, TagValue)>),
    SetElement(Math),
}
impl RuleOutcome {
    pub fn from_value(
        value: &Value,
        tags: &Tags,
        elements: &Elements,
    ) -> Result<Vec<(IVec2, Self)>, ValueParseError> {
        let outcomes_array = value.as_array().ok_or(ValueParseError::new("Rule outcomes should be an array"))?;

        let mut rule_outcomes = Vec::new();

        for outcome in outcomes_array {
            let outcome = outcome.as_array().ok_or(ValueParseError::new("Individual rule outcome should be an array"))?;
            let target = value_as_ivec(&outcome[0])?;

            let outcome_type = outcome[1].as_str().ok_or(ValueParseError::new("Second item of an outcome should be a String"))?;

            let outcome = match outcome_type {
                "Clone" => Ok(Self::Clone(value_as_ivec(&outcome[2])?)),
                "ChangeTags" => {
                    let change_tags_array = outcome[2].as_array().ok_or(ValueParseError::new("Third element of ChangeTags should be an array"))?;

                    let mut change_tags = Vec::new();

                    for change in change_tags_array {
                        let tag = change.as_array().ok_or(ValueParseError::new("Tag element of ChangeTags should be an array"))?;
                        let tag_name = tag[0].as_str().ok_or(ValueParseError::new("First item of tag array should be a String"))?;
                        let tag_index = tags.get_index(tag_name).ok_or(ValueParseError::new(format!("Tag \"{tag_name}\" doesn't exist")))?;
                        let new_value = Math::from_value(&tag[1], tags, elements)?;

                        change_tags.push((tag_index, new_value));
                    }

                    Ok(Self::ChangeTags(change_tags))
                },
                "SetTags" => {
                    let set_tags_array = outcome[2].as_array().ok_or(ValueParseError::new("Third element of ChangeTags should be an array"))?;

                    let mut set_tags = Vec::new();

                    for new_tag in set_tags_array {
                        let tag = new_tag.as_array().ok_or(ValueParseError::new("Tag element of ChangeTags should be an array"))?;
                        let tag_name = tag[0].as_str().ok_or(ValueParseError::new("First item of tag array should be a String"))?;
                        let tag_index = tags.get_index(tag_name).ok_or(ValueParseError::new(format!("Tag \"{tag_name}\" doesn't exist")))?;
                        let new_value = TagValue::from_value(&tag[1], elements);

                        set_tags.push((tag_index, new_value));
                    }

                    Ok(Self::SetTags(set_tags))
                },
                "SetElement" => Ok(Self::SetElement(Math::from_value(
                    &outcome[2],
                    tags,
                    elements,
                )?)),
                _ => {
                    Err(ValueParseError::new(format!("Invalid rule outcome type \"{outcome_type}\"")))
                },
            }?;

            rule_outcomes.push((target, outcome))
        }
        Ok(rule_outcomes)
    }
}

#[derive(Clone, Debug)]
pub enum Math {
    Minus(Box<Math>, Box<Math>),
    Plus(Box<Math>, Box<Math>),
    Div(Box<Math>, Box<Math>),
    Mul(Box<Math>, Box<Math>),
    Tag(IVec2, usize),
    Value(TagValue),
}
impl Math {
    pub fn evaluate(&self, pos: IVec2, state: &SimualtionState, world_size: i32) -> TagValue {
        match self {
            Math::Minus(a, b) => {
                a.evaluate(pos, state, world_size) - b.evaluate(pos, state, world_size)
            }
            Math::Plus(a, b) => {
                a.evaluate(pos, state, world_size) + b.evaluate(pos, state, world_size)
            }
            Math::Div(a, b) => {
                a.evaluate(pos, state, world_size) / b.evaluate(pos, state, world_size)
            }
            Math::Mul(a, b) => {
                a.evaluate(pos, state, world_size) * b.evaluate(pos, state, world_size)
            }
            Math::Tag(rel_pos, tag) => state.get_tag_at(tag, pos + *rel_pos),
            Math::Value(value) => *value,
        }
    }
    pub fn from_value(value: &Value, tags: &Tags, elements: &Elements) -> Result<Self, ValueParseError> {
        println!("{:?}", value);

        if !value.is_array() {
            return Ok(Math::Value(TagValue::from_value(value, elements)));
        }

        let sections = value.as_array().ok_or(ValueParseError::new("Math element should be an array"))?;

        if sections.len() == 1 {
            return Ok(Math::Value(TagValue::from_value(&sections[0], elements)));
        }
        if sections.len() == 2 && sections[1].is_string() {
            let tag_name = sections[1].as_str().ok_or(ValueParseError::new("Second item of tag value should be a String"))?;
            let tag_index = tags.get_index(tag_name).ok_or(ValueParseError::new(format!("Tag \"{tag_name}\" doesn't exist")))?;

            return Ok(Math::Tag(
                value_as_ivec(&sections[0])?,
                tag_index,
            ));
        }
        let operator = sections[1].as_str().ok_or(ValueParseError::new("Second item of Math should be a String operator"))?;

        match operator {
            "+" => Ok(Math::Plus(
                Box::new(Math::from_value(&sections[0], tags, elements)?),
                Box::new(Math::from_value(&sections[2], tags, elements)?),
            )),
            "-" => Ok(Math::Minus(
                Box::new(Math::from_value(&sections[0], tags, elements)?),
                Box::new(Math::from_value(&sections[2], tags, elements)?),
            )),
            "*" => Ok(Math::Mul(
                Box::new(Math::from_value(&sections[0], tags, elements)?),
                Box::new(Math::from_value(&sections[2], tags, elements)?),
            )),
            "/" => Ok(Math::Div(
                Box::new(Math::from_value(&sections[0], tags, elements)?),
                Box::new(Math::from_value(&sections[2], tags, elements)?),
            )),
            _ => Err(ValueParseError::new(format!("Invalid operator \"{operator}\""))),
        }
    }
}

#[derive(Clone)]
pub enum Rule {
    Single(SingleRule),
    Compound {
        name: String,
        enabled: bool,
        rules: Vec<SingleRule>,
    },
}
impl Rule {
    pub fn execute(
        &self,
        tiles: &SimualtionState,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) -> Vec<Vec<Vec<(IVec2, Vec<TagValue>)>>> {
        (0..world_size * world_size)
            .into_par_iter()
            .map(|index| {
                let x = index % world_size;
                let y = index / world_size;

                self.execute_at(IVec2::new(x, y), tiles, world_size, elements, frame, input)
            })
            .collect()
    }

    fn execute_at(
        &self,
        pos: IVec2,
        state: &SimualtionState,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) -> Vec<Vec<(IVec2, Vec<TagValue>)>> {
        let mut actions = match self {
            Rule::Single(rule) => rule.get_actions(pos, state, world_size, elements, frame, input),
            Rule::Compound { enabled, rules, .. } => {
                if *enabled {
                    rules
                        .iter()
                        .flat_map(|rule| {
                            rule.get_actions(pos, state, world_size, elements, frame, input)
                        })
                        .collect()
                } else {
                    Vec::new()
                }
            }
        };
        actions.par_sort_by(|a, b| a.priority.total_cmp(&b.priority).reverse());
        actions
            .into_iter()
            .map(|action| {
                action
                    .new_states
                    .into_iter()
                    .map(|(pos, action_type)| {
                        (
                            pos,
                            match action_type {
                                ActionDataType::Clone(from) => state.get_tags_at(from),
                                ActionDataType::ReplaceTags(new_tags) => {
                                    let mut state = state.get_tags_at(pos);
                                    for (name, value) in new_tags {
                                        if let Some(tag) = state.get_mut(name) {
                                            *tag = value;
                                        }
                                    }
                                    state
                                }
                                ActionDataType::ReplaceAllTags(new_tags) => new_tags,
                            },
                        )
                    })
                    .collect()
            })
            .collect()
    }
    fn set_enabled(&mut self, value: bool) {
        match self {
            Rule::Single(rule) => rule.enabled = value,
            Rule::Compound { enabled, rules, .. } => {
                *enabled = value;
                rules
                    .iter_mut()
                    .for_each(|child| child.set_parent_enabled(value));
            }
        }
    }
    fn get_enabled(&self) -> bool {
        match self {
            Rule::Single(rule) => rule.enabled,
            Rule::Compound { enabled, .. } => *enabled,
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
    priority: Math,
}

impl SingleRule {
    pub fn new(
        name: impl Into<String>,
        condition: Condition,
        rule_outcome: Vec<(IVec2, RuleOutcome)>,
        priority: Math,
    ) -> Self {
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
        state: &SimualtionState,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>,
    ) -> Vec<Action> {
        if !self.condition.evaluate(pos, state, world_size, input) || !self.get_enabled() {
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
                            RuleOutcome::ChangeTags(new_tags) => ActionDataType::ReplaceTags(
                                new_tags
                                    .iter()
                                    .map(|(tag, math)| {
                                        (*tag, math.evaluate(pos, state, world_size))
                                    })
                                    .collect(),
                            ),
                            RuleOutcome::SetTags(tags) => {
                                ActionDataType::ReplaceTags(tags.iter().cloned().collect())
                            }
                            RuleOutcome::SetElement(el) => ActionDataType::ReplaceAllTags(
                                elements
                                    .get_el(el.evaluate(pos, state, world_size))
                                    .iter()
                                    .cloned()
                                    .collect(),
                            ),
                        },
                    )
                })
                .collect(),
            self.priority.evaluate(pos, state, world_size).as_float(),
        )]
    }
}
