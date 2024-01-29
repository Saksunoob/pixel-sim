use serde_json::Value;
use std::{fs, path::Path};

use crate::{
    rules::{Condition, Math, RuleOutcome},
    tags::{TagValue, Tags},
    world::Elements,
    Rule, SingleRule,
};

pub fn load_tags(path: &Path) -> Option<Tags> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => None,
        }?;
        match serde_json::from_str::<Value>(&file_content) {
            Ok(tags_file) => {
                let mut tags = Vec::new();
                let tags_list = tags_file.as_object()?;
                for (name, space_type) in tags_list {
                    if let Value::String(str) = space_type {
                        match str.to_lowercase().as_str() {
                            "float" => tags.push((name, TagValue::Float(0.))),
                            "integer" => tags.push((name, TagValue::Integer(0))),
                            "boolean" => tags.push((name, TagValue::Boolean(false))),
                            "element" => tags.push((name, TagValue::Element(0))),
                            _ => {
                                panic!("Invalid value type: {}, try one of the following:\nfloat\ninteger\nboolean\nelement", str)
                            }
                        }
                    }
                }
                Some(Tags::new(tags))
            }
            Err(_) => None,
        }
    } else {
        None
    }
}

pub fn load_elements(path: &Path, tags: &Tags) -> Option<Elements> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => None,
        }?;
        match serde_json::from_str::<Value>(&file_content) {
            Ok(value) => Elements::from_value(&value, tags),
            Err(_) => None,
        }
    } else {
        None
    }
}

pub fn load_rule(name: &String, rule: &Value, tags: &Tags, elements: &Elements) -> Option<SingleRule> {
    let properties = rule.as_object()?;
    let condition = Condition::from_value(properties.get("condition")?, tags, elements);
    let rule_outcome = RuleOutcome::from_value(properties.get("rule_outcome")?, tags, elements);
    let priority = Math::from_value(properties.get("priority")?, tags, elements);

    match (condition, rule_outcome, priority) {
        (Some(condition), Some(rule_outcome), Some(priority)) => {
            println!("Loaded rule {name}");
            Some(SingleRule::new(name, condition, rule_outcome, priority))
        }
        (condition, rule_outcome, priority) => {
            eprintln!("Failed to load rule {name}:");
            if condition.is_none() {
                eprintln!("\tFailed to load condition");
            }
            if rule_outcome.is_none() {
                eprintln!("\tFailed to load rule outcome");
            }
            if priority.is_none() {
                eprintln!("\tFailed to load priority");
            }
            None
        }
    }
}

pub fn load_rules(path: &Path, tags: &Tags, elements: &Elements) -> Option<Vec<Rule>> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => {
                eprintln!("couldn't read file");
                None
            }
        }?;
        match serde_json::from_str::<Value>(&file_content) {
            Ok(json) => {
                let rules = json.as_object()?;

                Some(
                    rules
                        .into_iter()
                        .filter_map(|(rule_name, value)| {
                            let rule_info = value.as_object()?;

                            if rule_info.keys().all(|name| {// Test if rule has a structure of a single rule
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
                        .collect(),
                )
            }
            Err(_) => {
                eprintln!("invalid rule");
                None
            }
        }
    } else {
        eprintln!("Path is not a file");
        None
    }
}
