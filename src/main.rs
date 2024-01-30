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
use file_parser::{load_ruleset, load_tags, load_elements};
use rules::*;
use ui::UIPlugin;
use world::WorldPlugin;

use crate::camera::CameraPlugin;

mod camera;
mod file_parser;
mod rules;
mod tags;
mod ui;
mod world;

fn main() {
    let tags = load_tags(Path::new("simulation_data/tags.json"));
    let elements = load_elements(Path::new("simulation_data/elements.json"), &tags);
    let ruleset = load_ruleset(Path::new("simulation_data/rules.json"), &tags, &elements);

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
            WorldPlugin(256, tags, elements, ruleset),
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
