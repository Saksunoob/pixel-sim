use std::{path::Path, fs};
use serde_json::Value;

use crate::{world::Element, rules::{RuleType, Condition, RuleOutcome, Math}};

pub fn load_elements(path: &Path) -> Option<Vec<Element>> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => None,
        }?;
        match serde_json::from_str(&file_content) {
            Ok(element) => Some(element),
            Err(_) => None
        }
    } else {
        None
    }
}

pub fn load_rules(path: &Path) -> Option<Vec<RuleType>> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => {
                eprintln!("couldn't read file");
                None
            },
        }?;
        match serde_json::from_str::<Value>(&file_content) {
            Ok(json) => {
                let rules = json.as_array()?;
                
                Some(rules.into_iter().filter_map(|rule| {
                    let properties = rule.as_object()?;
                    let condition = Condition::from_value(properties.get("condition")?)?;
                    let rule_outcome = RuleOutcome::from_value(properties.get("rule_outcome")?)?;
                    let priority = Math::from_value(properties.get("priority")?)?;

                    Some(RuleType::Rule { condition, rule_outcome, priority })
                }).collect())
            },
            Err (_) => {
                eprintln!("invalid rule");
                None
            }
        }
        
    } else {
        eprintln!("Path is not a file");
        None
    }
}