use bevy::utils::thiserror::Error;
use serde_json::Value;
use std::{fmt::Display, fs, path::Path};

use crate::{
    rules::{Condition, Math, RuleOutcome}, tags::{TagType, TagValue, Tags}, world::{Element, Elements}, Rule, Ruleset, SingleRule
};

#[derive(Error, Debug)]
pub struct ValueParseError {
    msg: String
}
impl ValueParseError {
    pub fn new(msg: impl ToString) -> Self {
        Self {
            msg: msg.to_string()
        }
    }
}
impl Display for ValueParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}


fn get_file_content(path: &Path) -> Value {
    if path.is_file() {
        let content = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                panic!("Error reading file \"{}\"\n{err}", path.to_string_lossy())
            },
        };
        match serde_json::from_str::<Value>(&content) {
            Ok(value) => value,
            Err(_) => panic!("Error reading file \"{}\"\nFile content isn't valid JSON", path.to_string_lossy()),
        }
    } else {
        match path.exists() {
            true => panic!("Error reading file \"{}\"\nPath isn't a file", path.to_string_lossy()),
            false => panic!("Error reading file \"{}\"\nPath doesn't exist", path.to_string_lossy())
        }
    }
}

pub fn load_tags(path: &Path) -> Tags {
    let tags_file = get_file_content(path);
    let mut tags = Vec::new();
    let tags_list = match tags_file.as_object() {
        Some(object) => object,
        None => panic!("Error reading file \"{}\"\nFile should start with an object", path.to_string_lossy()),
    };

    for (name, space_type) in tags_list {
        if let Value::String(str) = space_type {
            match str.to_lowercase().as_str() {
                "float" => tags.push((name, TagType::Float)),
                "integer" => tags.push((name, TagType::Integer)),
                "boolean" => tags.push((name, TagType::Boolean)),
                "element" => tags.push((name, TagType::Element)),
                _ => {
                    eprintln!("Error reading file \"{}\"\n\
                            Invalid value type \"{}\"", path.to_string_lossy(), str);
                }
            }
        }
    }
    return Tags::new(tags)
}

pub fn load_elements(path: &Path, tags: &Tags) -> Elements {
    let value = get_file_content(path);
    let object = value.as_object().unwrap_or_else(|| {
        panic!("Elements should start with an object");
    });
        
    let mapper = Elements::new(object
        .iter()
        .map(|(name, _)| Element{name: name.to_string(),tags: Vec::new()})
        .collect());

    let elements = object.into_iter().filter_map(|(element_name, element_tags)| {
        let def_element_tags = element_tags.as_object().or_else(|| {eprintln!("Element should be an object"); None})?;

        // Create a default tag vector
        let mut element_tags: Vec<_> = tags.iter().map(|_| TagValue::None).collect();

        // Replace defined tags
        for (name, value) in def_element_tags {
            let tag_index = tags.get_index(name);
            if let Some(tag_index) = tag_index {
                element_tags[tag_index] = TagValue::from_value(value, &mapper);
            } else {
                eprintln!("Error loading Element \"{element_name}\"\nTag \"{name}\" doesn't exist");
            }
        }

        Some(Element {
            name: element_name.to_string(),
            tags: element_tags,
        })
    }).collect();

    Elements::new(elements)
}

pub fn load_rule(name: &String, rule: &Value, tags: &Tags, elements: &Elements) -> Option<SingleRule> {
    let properties = rule.as_object().or_else(|| {
        eprintln!("Error reading rule \"{}\"\nRule should be an object", name);
        None
    })?;

    let condition_value = properties.get("condition").or_else(||{
        eprintln!("Error reading rule \"{}\"\nMissing condition property", name);
        None
    })?;

    let outcome_value = properties.get("rule_outcome").or_else(||{
        eprintln!("Error reading rule \"{}\"\nMissing rule_outcome property", name);
        None
    })?;

    let priority_value = properties.get("priority").or_else(||{
        eprintln!("Error reading rule \"{}\"\nMissing priority property", name);
        None
    })?;

    let condition = Condition::from_value(condition_value, tags, elements).or_else(|err| {
        eprintln!("Error parsing condition for {name}\n{err}");
        Err(ValueParseError::new(""))
    }).ok()?;

    let rule_outcome = RuleOutcome::from_value(outcome_value, tags, elements).or_else(|err| {
        eprintln!("Error parsing rule outcome for {name}\n{err}");
        Err(ValueParseError::new(""))
    }).ok()?;

    let priority = Math::from_value(priority_value, tags, elements).or_else(|err| {
        eprintln!("Error parsing priority for {name}\n{err}");
        Err(ValueParseError::new(""))
    }).ok()?;

    Some(SingleRule::new(name, condition, rule_outcome, priority))
}

pub fn load_ruleset(path: &Path, tags: &Tags, elements: &Elements) -> Ruleset {
    let rules = get_file_content(path);

    let rules = match rules.as_object() {
        Some(rules) => rules,
        None => panic!("Error reading file \"{}\"\nFile should start with an object", path.to_string_lossy()),
    };

    Ruleset::new(rules
        .into_iter()
        .filter_map(|(rule_name, value)| {
            let rule_info = match value.as_object() {
                Some(info) => info,
                None => panic!("Error reading rule \"{}\"\nRule should be an object", rule_name),
            };

            if rule_info.keys().any(|name| {// Test if rule has a structure of a single rule
                match name.as_str() {
                    "condition" => true,
                    "rule_outcome" => true,
                    "priority" => true,
                    _ => false
                }
            }) {
                load_rule(rule_name, value, tags, elements)
                .and_then(|rule| Some(Rule::Single(rule)))
            } else {
                Some(Rule::Compound {
                    name: rule_name.to_string(),
                    enabled: true,
                    rules: rule_info
                        .into_iter()
                        .filter_map(|rule| load_rule(rule.0, rule.1, tags, elements))
                        .collect(),
                })
            }
        })
        .collect())
}
