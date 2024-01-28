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
use ui::UIPlugin;
use world::WorldPlugin;

use crate::camera::CameraPlugin;
use crate::file_parser::load_elements;

mod camera;
mod file_parser;
mod rules;
mod tags;
mod ui;
mod world;

fn main() {
    let elements = load_elements(Path::new("src/elements.json")).unwrap();
    let rules = load_rules(Path::new("src/rules.json")).unwrap();

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
            WorldPlugin(256, elements, ruleset),
            CameraPlugin,
            UIPlugin,
        ))
        .run();
}

pub fn hash(str: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    str.hash(&mut hasher);
    hasher.finish()
}
