use std::{path::Path, fs};
use serde_json::Value;

use crate::{world::Elements, rules::{RuleType, Condition, RuleOutcome, Math}};

pub fn load_elements(path: &Path) -> Option<Elements> {
    if path.is_file() {
        let file_content = match fs::read_to_string(path) {
            Ok(content) => Some(content),
            Err(_) => None,
        }?;
        match serde_json::from_str(&file_content) {
            Ok(elements) => Some(Elements::new(elements)),
            Err(_) => None
        }
    } else {
        None
    }
}

pub fn load_rule(rule: &Value) -> Option<RuleType> {
    let properties = rule.as_object()?;
    let name = match properties.get("name") {
        Some(value) => value.as_str().unwrap_or("no_name").to_string(),
        None => "no_name".to_string()
    };
    let condition = Condition::from_value(properties.get("condition")?);
    let rule_outcome = RuleOutcome::from_value(properties.get("rule_outcome")?);
    let priority = Math::from_value(properties.get("priority")?);

    match (condition, rule_outcome, priority) {
        (Some(condition), Some(rule_outcome), Some(priority)) => {
            println!("Loaded rule {name}");
            Some(RuleType::Rule { name, condition, rule_outcome, priority })
        },
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
                    if let Some(compound) = rule.as_array() { // Compound rule
                        Some(RuleType::CompoundRule(compound[0].as_str()?.to_string(), compound[1..].into_iter().filter_map(|rule| load_rule(rule)).collect()))
                    } else { // Single rule
                        load_rule(rule)
                    }
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