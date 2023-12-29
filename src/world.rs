use crate::hash;
use crate::rules::*;
use crate::tags::*;
use bevy::prelude::*;
use bevy::{
    app::Plugin,
    asset::{AssetServer, Handle},
    ecs::system::{Commands, Res, Resource},
    math::Vec2,
    render::{
        color::Color,
        render_resource::{Extent3d, TextureDimension, TextureFormat},
        texture::Image,
    },
    sprite::{Sprite, SpriteBundle},
    transform::components::Transform,
};
use rayon::prelude::*;
use serde::Serialize;
use std::collections::HashMap;
use serde::Deserialize;

pub struct WorldPlugin(pub World);

impl Plugin for WorldPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.insert_resource(self.0.clone())
            .add_systems(Startup, setup)
            .add_systems(Update, (update, simulation_step));
    }
}
#[derive(Resource, Clone)]
pub struct World {
    bits: HashMap<String, TagSpace>,
    world_size: i32,
    ruleset: Ruleset,
    elements: Elements,
}

impl World {
    pub fn new(size: usize, elements: Vec<Element>, ruleset: Ruleset) -> Self {
        let elements = Elements::new(elements);

        let mut bits = HashMap::new();
        elements.elements.iter().for_each(|(_, tags)| {
            tags.iter().for_each(|(tag, _)| {
                if !bits.contains_key(tag) {
                    bits.insert(
                        tag.to_string(),
                        TagSpace::new_with_value(elements
                            .get(elements.default).into_iter()
                            .collect::<HashMap<String, TagValue>>()
                            .get(tag)
                            .copied()
                            .unwrap_or(TagValue::None), size as i32
                        )
                    );
                }
            })
        });

        Self {
            bits,
            world_size: size as i32,
            ruleset,
            elements,
        }
    }
    pub fn step(&mut self, frame: u128, input: &Res<Input<ScanCode>>) {
        self.ruleset.execute_rules(&mut self.bits, self.world_size, &self.elements, frame, input);
    }
    pub fn get_image(&self) -> Image {
        let colors = self.bits.get("color").unwrap();
        let image_data = (0..self.world_size * self.world_size)
            .into_par_iter()
            .flat_map(|index| {
                if let TagValue::Integer(value) = colors.get_tag_at_index(index) {
                    [
                        (value >> 16) as u8,
                        (value >> 8) as u8,
                        value as u8,
                        255
                    ]
                } else {
                    Color::PURPLE.as_rgba_u8()
                }
            })
            .collect();

        Image::new(
            Extent3d {
                width: self.world_size as u32,
                height: self.world_size as u32,
                depth_or_array_layers: 1,
            },
            TextureDimension::D2,
            image_data,
            TextureFormat::Rgba8UnormSrgb,
        )
    }
}

#[derive(Resource)]
struct GameScreen(Handle<Image>, Rect);

#[derive(Resource)]
struct Simulation {
    last_step_time: u128,
    steps: u128,
}
impl Simulation {
    pub fn new() -> Self {
        Self {
            last_step_time: 0,
            steps: 0,
        }
    }
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let image = Image::default();
    let image_handle = asset_server.add(image);
    commands.insert_resource(GameScreen(image_handle.clone(), Rect::default()));
    commands.insert_resource(Simulation::new());

    commands.spawn(SpriteBundle {
        sprite: Sprite {
            anchor: bevy::sprite::Anchor::Center,
            custom_size: Some(Vec2::splat(1.0)),
            ..Default::default()
        },
        transform: Transform::from_xyz(0., 0., 0.),
        texture: image_handle,
        ..default()
    });
}

fn simulation_step(
    time: Res<Time>,
    mut simulation: ResMut<Simulation>,
    mut world: ResMut<World>,
    input: Res<Input<ScanCode>>,
) {
    simulation.last_step_time = time.elapsed().as_millis();
    world.step(simulation.steps, &input);
    simulation.steps += 1;
}

fn update(
    game_screen: ResMut<GameScreen>,
    mut images: ResMut<Assets<Image>>,
    world: Res<World>,
    mut target: Query<&mut Sprite>,
) {
    if !world.is_changed() {
        return;
    }

    let mut target = target.single_mut();
    let image = images.get_mut(game_screen.0.clone());

    match image {
        Some(image) => {
            *image = world.get_image();
            target.custom_size = Some(image.size_f32() * 2.);
        }
        None => eprintln!("Simualtion image handle not initialized"),
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Element {
    pub name: String,
    pub tags: HashMap<String, TagValue>
}

#[derive(Clone)]
pub struct Elements {
    elements: HashMap<u64, Vec<(String, TagValue)>>,
    default: u64,
}
impl Elements {
    pub fn new(elements: Vec<Element>) -> Self {
        let default = hash(&elements[0].name);
        Self {
            elements: elements
                .into_iter()
                .map(|element| {
                    (
                        hash(&element.name),
                        element.tags.into_iter()
                            .map(|(tag, value)| (tag.to_string(), value))
                            .collect(),
                    )
                })
                .collect(),
            default,
        }
    }
    pub fn get(&self, element: u64) -> Vec<(String, TagValue)> {
        self.elements.get(&element).cloned().unwrap_or_else(|| {
            eprintln!("unable to get element {element}");
            self.elements.get(&self.default).cloned().unwrap()
        })
    }
    pub fn get_el(&self, element: TagValue) -> Vec<(String, TagValue)> {
        if let TagValue::Element(el) = element {
            self.elements.get(&el).cloned().unwrap_or_else(|| {
                eprintln!("unable to get element {el}");
                self.elements.get(&self.default).cloned().unwrap()
            })
        } else {
            Vec::new()
        }
    }
}