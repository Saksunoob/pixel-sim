use std::{collections::HashMap, sync::RwLock};

use bevy::{math::IVec2, ecs::system::Res, input::{keyboard::ScanCode, Input}};
use rand::random;
use rayon::prelude::*;

use crate::world::{TagValue, Elements, TagSpace};

#[derive(Clone)]
pub struct Ruleset {
    rules: Vec<Rule>
}

impl Ruleset {
    pub fn new(rules: Vec<Rule>) -> Self {
        Self {rules}
    }
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }
    pub fn execute_rules(
        &self, 
        tiles: &mut HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>
    ) {
        self.rules.iter().for_each(|rule| {
            self.execute_rule(rule, tiles, world_size, elements, frame, input);
        })
    }

    pub fn execute_rule(
        &self,
        rule: &Rule,
        tiles: &mut HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        frame: u128,
        input: &Res<Input<ScanCode>>
    ) {
        let mut actions: Vec<Action> = (0..world_size * world_size)
            .into_par_iter()
            .flat_map(|index| {
                let x = index % world_size;
                let y = index / world_size;
                let out = rule.execute(IVec2::new(x, y), tiles, world_size, elements, frame, input);

                out
            })
            .collect();

        actions.par_sort_unstable_by(|a, b| a.priority.partial_cmp(&b.priority).unwrap().reverse());

        let affected: Vec<RwLock<bool>> = (0..(world_size * world_size)).into_par_iter().map(|_| RwLock::from(false)).collect();

        let actions: HashMap<usize, ActionDataType> = actions.into_par_iter().filter_map(|action| {
            if action.get_affected().iter().any(|index| *affected[get_index(index, world_size)].read().unwrap()) {
                return None;
            }
            action.get_affected().iter().for_each(|index| *affected[get_index(index, world_size)].write().unwrap() = true);
            Some(action.new_states.iter().map(|(pos, data)| (get_index(pos, world_size), data.clone())).collect::<Vec<_>>())
        }).flatten().collect();

        let old_tiles = tiles.clone();

        tiles.into_par_iter().for_each(|(tag_name, value_vec)| {
            value_vec.par_iter_mut().enumerate().filter(|(i, _)| *affected[*i].read().unwrap()).for_each(|(i, tag_value)| {
                if let Some(action) = actions.get(&i) {
                    match action {
                        ActionDataType::Clone(from) => {
                            *tag_value = old_tiles.get(tag_name).unwrap().get_tag(*from)
                        },
                        ActionDataType::ReplaceTags(new_tags) => {
                            if let Some(new_value) = new_tags.get(tag_name) {
                                *tag_value = *new_value
                            }
                        },
                        ActionDataType::ReplaceAllTags(new_tags) => {
                            *tag_value = new_tags.get(tag_name).copied().unwrap_or(TagValue::None)
                        },
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

#[derive(Clone)]
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

#[derive(Clone)]
pub enum Condition {
    And(Vec<Condition>),
    Or(Vec<Condition>),
    Condition(ConditionType)
}

impl Condition {
    pub fn evaluate(&self, origin: IVec2, tiles: &HashMap<String, TagSpace>, world_size: i32, input: &Res<Input<ScanCode>>) -> bool {
        match self {
            Condition::And(conditions) => conditions.iter().all(|condition| condition.evaluate(origin, tiles, world_size, input)),
            Condition::Or(conditions) => conditions.iter().any(|condition| condition.evaluate(origin, tiles, world_size, input)),
            Condition::Condition(condition) => {
                match condition {
                    ConditionType::Random(chance) => random::<f64>() <= *chance,
                    ConditionType::Input(scancode) => input.pressed(ScanCode(*scancode)),
                    ConditionType::Is(pos, tag, value) => tiles.get(tag).unwrap().get_rel_tag(origin, *pos) == *value,
                    ConditionType::Eq((pos1, pos2), tag) => {
                        let vec = tiles.get(tag).unwrap();
                        vec.get_rel_tag(origin, *pos1) == vec.get_rel_tag(origin, *pos2)
                    },
                    ConditionType::Lt((pos1, pos2), tag) => {
                        let vec = tiles.get(tag).unwrap();
                        vec.get_rel_tag(origin, *pos1) < vec.get_rel_tag(origin, *pos2)
                    },
                    ConditionType::Lte((pos1, pos2), tag) => {
                        let vec = tiles.get(tag).unwrap();
                        vec.get_rel_tag(origin, *pos1) <= vec.get_rel_tag(origin, *pos2)
                    },
                    ConditionType::Gt((pos1, pos2), tag) => {
                        let vec = tiles.get(tag).unwrap();
                        vec.get_rel_tag(origin, *pos1) > vec.get_rel_tag(origin, *pos2)
                    },
                    ConditionType::Gte((pos1, pos2), tag) => {
                        let vec = tiles.get(tag).unwrap();
                        vec.get_rel_tag(origin, *pos1) >= vec.get_rel_tag(origin, *pos2)
                    },
                }
            },
        }
    }
}

#[derive(Clone)]
pub enum Result {
    Clone(IVec2),
    ChangeTags(Vec<(String, Math)>),
    SetTags(Vec<(String, TagValue)>),
    SetElement(Math)
}
#[derive(Clone)]
pub enum Math {
    Minus(Box<Math>, Box<Math>),
    Plus(Box<Math>, Box<Math>),
    Div(Box<Math>, Box<Math>),
    Mul(Box<Math>, Box<Math>),
    Tag(IVec2, String),
    Value(TagValue)
}
impl Math {
    pub fn evaluate(&self, pos: IVec2, tiles: &HashMap<String, TagSpace>, world_size: i32) -> TagValue {
        match self {
            Math::Minus(a, b) => a.evaluate(pos, tiles, world_size)-b.evaluate(pos, tiles, world_size),
            Math::Plus(a, b)  => a.evaluate(pos, tiles, world_size)+b.evaluate(pos, tiles, world_size),
            Math::Div(a, b)   => a.evaluate(pos, tiles, world_size)/b.evaluate(pos, tiles, world_size),
            Math::Mul(a, b)   => a.evaluate(pos, tiles, world_size)*b.evaluate(pos,tiles, world_size),
            Math::Tag(rel_pos, name) => tiles.get(name).unwrap().get_rel_tag(pos, *rel_pos),
            Math::Value(value) => *value,
        }
    }
}

fn get_index(pos: &IVec2, world_size: i32) -> usize {
    (pos.y*world_size+pos.x) as usize
}

#[derive(Clone)]
pub struct Rule {
    pub condition: Condition,
    pub result: Vec<(IVec2, Result)>,
    pub priority: Math
}
impl Rule {
    pub fn execute(&self, 
        pos: IVec2,
        tiles: &HashMap<String, TagSpace>,
        world_size: i32,
        elements: &Elements,
        _: u128,
        input: &Res<Input<ScanCode>>
    ) -> Option<Action> {

        if self.condition.evaluate(pos, tiles, world_size, input) {
            return Some(Action::new(self.result.iter().map(|(rel_pos, result)| {
                (pos.wrapping_add(*rel_pos), match result {
                    Result::Clone(from) => ActionDataType::Clone(pos.wrapping_add(*from)),
                    Result::ChangeTags(tags) => ActionDataType::ReplaceTags(tags.iter().map(|(tag, math)| (tag.to_string(), math.evaluate(pos, tiles, world_size))).collect()),
                    Result::SetTags(tags) => ActionDataType::ReplaceTags(tags.iter().cloned().collect()),
                    Result::SetElement(el) => ActionDataType::ReplaceAllTags(elements.get_el(el.evaluate(pos, tiles, world_size)).iter().cloned().collect()),
                })
            }).collect(), self.priority.evaluate(pos, tiles, world_size).as_float()));
        }
        None
    }
}