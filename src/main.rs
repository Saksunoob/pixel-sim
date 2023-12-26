use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use bevy::{
    prelude::*,
    render::{
        settings::{Backends, WgpuSettings},
        texture::ImageSamplerDescriptor,
        RenderPlugin,
    },
};
use rules::*;
use world::WorldPlugin;

use crate::camera::CameraPlugin;
use crate::world::TagValue;

mod camera;
mod world;
mod rules;

fn color_to_int(r: u8, g: u8, b: u8, a: u8) -> i64 {
    return ((r as i64) << 24) + ((g as i64) << 16) + ((b as i64) << 8) + (a as i64);
}

fn main() {
    let elements = vec![
        (
            "air",
            vec![
                ("color", TagValue::Integer(color_to_int(0, 0, 0, 255))),
                ("mass", TagValue::Float(0.1)),
            ],
        ),
        (
            "paper",
            vec![
                ("color", TagValue::Integer(color_to_int(255, 255, 255, 255))),
                ("mass", TagValue::Float(1.)),
                ("flammable", TagValue::Empty),
                ("burn_time", TagValue::Integer(5)),
                ("on_burn_out", TagValue::Element(hash("flame"))),
            ],
        ),
        (
            "spark",
            vec![
                ("color", TagValue::Integer(color_to_int(255, 140, 0, 255))),
                ("mass", TagValue::Float(1.)),
                ("burning", TagValue::Empty),
                ("burn_time", TagValue::Integer(250)),
                ("on_burn_out", TagValue::Element(hash("air"))),
            ],
        ),
        (
            "flame",
            vec![
                ("color", TagValue::Integer(color_to_int(255, 255, 0, 255))),
                ("mass", TagValue::Float(0.05)),
                ("burning", TagValue::Empty),
                ("burn_time", TagValue::Integer(10)),
                ("on_burn_out", TagValue::Element(hash("air"))),
            ],
        ),
    ];

    let ruleset = Ruleset::new(vec![
        // Gravity
        Rule {
            condition: rules::Condition::Condition(ConditionType::Lt((IVec2::new(0, 1), IVec2::ZERO), "mass".to_string())),
            result: vec![(IVec2::new(0, 1), Result::Clone(IVec2::ZERO)), (IVec2::ZERO, Result::Clone(IVec2::new(0, 1)))],
            priority: Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "mass".to_string())), Box::new(Math::Tag(IVec2::new(0, 1), "mass".to_string()))),
        },
        // Gravity diagonal right
        Rule {
            condition: rules::Condition::And(vec![
                rules::Condition::Condition(ConditionType::Lt((IVec2::new(1, 0), IVec2::ZERO), "mass".to_string())), 
                rules::Condition::Condition(ConditionType::Lte((IVec2::new(1, 1), (IVec2::new(1, 0))), "mass".to_string()))]),
            result: vec![(IVec2::new(1, 1), Result::Clone(IVec2::ZERO)), (IVec2::ZERO, Result::Clone(IVec2::new(1, 1)))],
            priority: Math::Div(Box::new(Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "mass".to_string())), Box::new(Math::Tag(IVec2::new(1, 1), "mass".to_string())))), Box::new(Math::Value(TagValue::Float(1.4)))),
        },
        // Gravity diagonal left
        Rule {
            condition: rules::Condition::And(vec![
                rules::Condition::Condition(ConditionType::Lt((IVec2::new(-1, 0), IVec2::ZERO), "mass".to_string())), 
                rules::Condition::Condition(ConditionType::Lte((IVec2::new(-1, 1), (IVec2::new(-1, 0))), "mass".to_string()))]),
            result: vec![(IVec2::new(-1, 1), Result::Clone(IVec2::ZERO)), (IVec2::ZERO, Result::Clone(IVec2::new(-1, 1)))],
            priority: Math::Div(Box::new(Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "mass".to_string())), Box::new(Math::Tag(IVec2::new(-1, 1), "mass".to_string())))), Box::new(Math::Value(TagValue::Float(1.4)))),
        },
        // Paper rain
        Rule {
            condition: rules::Condition::And(vec![rules::Condition::Condition(ConditionType::Is(IVec2::new(0, -1), "mass".to_string(), TagValue::None)), rules::Condition::Condition(ConditionType::Random(0.2))]),
            result: vec![(IVec2::ZERO, Result::SetElement(Math::Value(TagValue::Element(hash("paper")))))],
            priority: Math::Value(TagValue::Float(0.1)),
        },
        // Spark rain
        Rule {
            condition: rules::Condition::And(vec![rules::Condition::Condition(ConditionType::Is(IVec2::new(0, -1), "mass".to_string(), TagValue::None)), rules::Condition::Condition(ConditionType::Random(0.0001))]),
            result: vec![(IVec2::ZERO, Result::SetElement(Math::Value(TagValue::Element(hash("spark")))))],
            priority: Math::Value(TagValue::Float(0.1)),
        },
        // Ignition
        Rule {
            condition: rules::Condition::And(vec![
                rules::Condition::Condition(ConditionType::Is(IVec2::ZERO, "flammable".to_string(), TagValue::Empty)),
                rules::Condition::Or(vec![
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(1, 0), "burning".to_string(), TagValue::Empty)),
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(0, 1), "burning".to_string(), TagValue::Empty)),
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(-1, 0), "burning".to_string(), TagValue::Empty)),
                    rules::Condition::Condition(ConditionType::Is(IVec2::new(0, -1), "burning".to_string(), TagValue::Empty)),
            ])
            ]),
            result: vec![(IVec2::ZERO, Result::ChangeTags(vec![("burning".to_string(), Math::Value(TagValue::Empty))]))],
            priority: Math::Value(TagValue::Float(1.)),
        },
        // Burning
        Rule {
            condition: rules::Condition::Condition(ConditionType::Is(IVec2::ZERO, "burning".to_string(), TagValue::Empty)),
            result: vec![(IVec2::ZERO, Result::ChangeTags(vec![("burn_time".to_string(), Math::Minus(Box::new(Math::Tag(IVec2::ZERO, "burn_time".to_string())), Box::new(Math::Value(TagValue::Integer(1)))))]))],
            priority: Math::Value(TagValue::Float(0.1)),
        },
        // Burn out
        Rule {
            condition: rules::Condition::Condition(ConditionType::Is(IVec2::ZERO, "burn_time".to_string(), TagValue::Integer(0))),
            result: vec![(IVec2::ZERO, Result::SetElement(Math::Tag(IVec2::ZERO, "on_burn_out".to_string())))],
            priority: Math::Value(TagValue::Float(0.1)),
        }
    ]);

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
