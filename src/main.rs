use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::Path;

use bevy::{
    prelude::*,
    render::{
        settings::{Backends, WgpuSettings},
        texture::ImageSamplerDescriptor,
        RenderPlugin,
    },
};
use file_parser::load_rules;
use rules::*;
use world::WorldPlugin;

use crate::camera::CameraPlugin;
use crate::file_parser::load_elements;
use crate::tags::TagValue;

mod camera;
mod world;
mod rules;
mod tags;
mod file_parser;

fn main() {
    let elements = load_elements(Path::new("elements.json")).unwrap();
    let mut rules = load_rules(Path::new("rules.json")).unwrap();

    rules.append(&mut vec![
        /*// Gravity
        RuleType::CompoundRule(vec![
            // Down
            RuleType::Rule {
                condition: rules::Condition::Condition(ConditionType::Lt((IVec2::new(0, 1), IVec2::ZERO), "mass".to_string())),
                rule_outcome: vec![(IVec2::new(0, 1), RuleOutcome::Clone(IVec2::ZERO)), (IVec2::ZERO, RuleOutcome::Clone(IVec2::new(0, 1)))],
                priority: Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "mass".to_string())), Box::new(Math::Tag(IVec2::new(0, 1), "mass".to_string()))),
            },
            // Diagonal right
            RuleType::Rule {
                condition: rules::Condition::And(vec![
                    rules::Condition::Condition(ConditionType::Lt((IVec2::new(1, 0), IVec2::ZERO), "mass".to_string())), 
                    rules::Condition::Condition(ConditionType::Lte((IVec2::new(1, 1), (IVec2::new(1, 0))), "mass".to_string()))]),
                    rule_outcome: vec![(IVec2::new(1, 1), RuleOutcome::Clone(IVec2::ZERO)), (IVec2::ZERO, RuleOutcome::Clone(IVec2::new(1, 1)))],
                priority: Math::Div(Box::new(Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "mass".to_string())), Box::new(Math::Tag(IVec2::new(1, 1), "mass".to_string())))), Box::new(Math::Value(TagValue::Float(0.1)))),
            },
            // Diagonal left
            RuleType::Rule {
                condition: rules::Condition::And(vec![
                    rules::Condition::Condition(ConditionType::Lt((IVec2::new(-1, 0), IVec2::ZERO), "mass".to_string())), 
                    rules::Condition::Condition(ConditionType::Lte((IVec2::new(-1, 1), (IVec2::new(-1, 0))), "mass".to_string()))]),
                    rule_outcome: vec![(IVec2::new(-1, 1), RuleOutcome::Clone(IVec2::ZERO)), (IVec2::ZERO, RuleOutcome::Clone(IVec2::new(-1, 1)))],
                priority: Math::Div(Box::new(Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "mass".to_string())), Box::new(Math::Tag(IVec2::new(-1, 1), "mass".to_string())))), Box::new(Math::Value(TagValue::Float(0.1)))),
            },
        ]),*/
        RuleType::CompoundRule("rain".to_string(), vec![
            RuleType::Rule {
                name: "paper_rain".to_string(),
                condition: rules::Condition::And(vec![rules::Condition::Condition(ConditionType::Is(IVec2::new(0, -1), "mass".to_string(), TagValue::None)), rules::Condition::Condition(ConditionType::Random(0.2)), rules::Condition::Condition(ConditionType::Input(57))]),
                rule_outcome: vec![(IVec2::ZERO, RuleOutcome::SetElement(Math::Value(TagValue::Element(hash("paper")))))],
                priority: Math::Value(TagValue::Float(0.1))
            },
            RuleType::Rule {
                name: "spark_rain".to_string(),
                condition: rules::Condition::And(vec![rules::Condition::Condition(ConditionType::Is(IVec2::new(0, -1), "mass".to_string(), TagValue::None)), rules::Condition::Condition(ConditionType::Random(0.0001)), rules::Condition::Condition(ConditionType::Input(57))]),
                rule_outcome: vec![(IVec2::ZERO, RuleOutcome::SetElement(Math::Value(TagValue::Element(hash("spark")))))],
                priority: Math::Value(TagValue::Float(0.1)),
            }
        ]),
        /*// Ignition
        RuleType::Rule {
            condition: rules::Condition::And(vec![
                rules::Condition::Condition(ConditionType::Is(IVec2::ZERO, "flammable".to_string(), TagValue::Empty)),
                rules::Condition::Or(vec![
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(1, 0), "burning".to_string(), TagValue::Empty)),
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(0, 1), "burning".to_string(), TagValue::Empty)),
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(-1, 0), "burning".to_string(), TagValue::Empty)),
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(0, -1), "burning".to_string(), TagValue::Empty)),
            ])
            ]),
            rule_outcome: vec![(IVec2::ZERO, RuleOutcome::ChangeTags(vec![("burning".to_string(), Math::Value(TagValue::Empty))]))],
            priority: Math::Value(TagValue::Float(1.)),
        },
        // Burning
        RuleType::Rule {
            condition: rules::Condition::Condition(ConditionType::Is(IVec2::ZERO, "burning".to_string(), TagValue::Empty)),
            rule_outcome: vec![(IVec2::ZERO, RuleOutcome::ChangeTags(vec![("burn_time".to_string(), Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "burn_time".to_string())), Box::new(Math::Value(TagValue::Integer(1)))))]))],
            priority: Math::Value(TagValue::Float(0.1)),
        },
        // Burn out
        RuleType::Rule {
            condition: rules::Condition::Condition(ConditionType::Is(IVec2::ZERO, "burn_time".to_string(), TagValue::Integer(0))),
            rule_outcome: vec![(IVec2::ZERO, RuleOutcome::SetElement(Math::Tag(IVec2::ZERO, "on_burn_out".to_string())))],
            priority: Math::Value(TagValue::Float(0.1)),
        }*/
    ]);

    let ruleset = Ruleset::new(rules);

    App::new()
        .add_plugins((
            DefaultPlugins
                .set(ImagePlugin {
                    default_sampler: ImageSamplerDescriptor::nearest(),
                })
                .set(RenderPlugin {
                    render_creation: bevy::render::settings::RenderCreation::Automatic(
                        WgpuSettings {
                            backends: Some(Backends::VULKAN),
                            ..default()
                        },
                    ),
                }),
            WorldPlugin(world::World::new(250, elements, ruleset)),
            CameraPlugin,
        ))
        .run();
}

pub fn hash(str: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    str.hash(&mut hasher);
    hasher.finish()
}
