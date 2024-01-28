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
use serde_json::Value;
use std::collections::HashMap;

pub struct WorldPlugin(pub usize, pub Tags, pub Elements, pub Ruleset);

impl Plugin for WorldPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.insert_resource(World::new(
            self.0,
            self.1.clone(),
            self.2.clone(),
            self.3.clone(),
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, (update, simulation_step));
    }
}
#[derive(Resource)]
pub struct World {
    pub names: Vec<String>,
    pub state: SimualtionState,
    pub world_size: i32,
    pub ruleset: Ruleset,
    pub elements: Elements,
}

impl World {
    pub fn new(size: usize, tags: Tags, elements: Elements, ruleset: Ruleset) -> Self {
        let names = (0..size.pow(2))
            .map(|_| elements.get(elements.default).name.to_string())
            .collect();

        Self {
            names,
            state: SimualtionState::new(tags, size, elements.get(elements.default)),
            world_size: size as i32,
            ruleset,
            elements,
        }
    }
    pub fn step(&mut self, frame: u128, input: &Res<Input<ScanCode>>) {
        self.ruleset.execute_rules(
            &mut self.state,
            self.world_size,
            &self.elements,
            frame,
            input,
        );
    }
    pub fn get_image(&self) -> Image {
        let colors = self
            .state
            .get_space(self.state.tags.get_index("color").unwrap());
        let image_data = (0..self.world_size * self.world_size)
            .into_par_iter()
            .flat_map(|index| {
                if let TagValue::Integer(value) = colors.get_tag_at_index(index) {
                    [(value >> 16) as u8, (value >> 8) as u8, value as u8, 255]
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

#[derive(Component)]
struct PixelGrid;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let image = Image::default();
    let image_handle = asset_server.add(image);
    commands.insert_resource(GameScreen(image_handle.clone(), Rect::default()));
    commands.insert_resource(Simulation::new());

    commands.spawn((
        SpriteBundle {
            sprite: Sprite {
                anchor: bevy::sprite::Anchor::Center,
                custom_size: Some(Vec2::splat(1.0)),
                ..Default::default()
            },
            transform: Transform::from_xyz(0., 0., 0.),
            texture: image_handle,
            ..default()
        },
        PixelGrid,
    ));
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
    mut target: Query<(&mut Sprite, &PixelGrid)>,
) {
    if !world.is_changed() {
        return;
    }

    let (mut target, _) = target.single_mut();
    let image = images.get_mut(game_screen.0.clone());

    match image {
        Some(image) => {
            *image = world.get_image();
            target.custom_size = Some(image.size_f32() * 2.);
        }
        None => eprintln!("Simualtion image handle not initialized"),
    }
}

#[derive(Debug, Clone)]
pub struct Element {
    pub name: String,
    pub tags: Vec<TagValue>,
}

#[derive(Clone, Debug)]
pub struct Elements {
    element_mapping: HashMap<String, usize>,
    pub elements: Vec<Element>,
    default: usize,
}
impl Elements {
    pub fn new(elements: Vec<Element>) -> Self {
        Self {
            element_mapping: elements
                .iter()
                .enumerate()
                .map(|(i, el)| (el.name.to_string(), i))
                .collect(),
            elements,
            default: 0,
        }
    }
    pub fn from_value(value: &Value, tags: &Tags) -> Option<Self> {
        let object = value.as_object()?;

        let mapper = Elements {
            element_mapping: object
                .iter()
                .enumerate()
                .map(|(index, (name, _))| (name.to_string(), index))
                .collect(),
            elements: Vec::new(),
            default: 0,
        };

        let elements = object
            .into_iter()
            .filter_map(|(name, element_tags)| {
                let def_element_tags = element_tags.as_object()?;
                let mut element_tags: Vec<_> = tags.iter().map(|_| TagValue::None).collect();

                def_element_tags.into_iter().for_each(|(name, value)| {
                    element_tags[tags.get_index(name).unwrap()] =
                        TagValue::from_value(value, &mapper);
                });

                Some(Element {
                    name: name.to_string(),
                    tags: element_tags,
                })
            })
            .collect();

        Some(Self::new(elements))
    }
    pub fn get_index(&self, name: impl ToString) -> Option<usize> {
        self.element_mapping.get(&name.to_string()).copied()
    }
    pub fn get(&self, element: usize) -> &Element {
        self.elements.get(element).unwrap_or_else(|| {
            eprintln!("unable to get element {element}");
            self.elements.get(self.default).unwrap()
        })
    }
    pub fn get_el(&self, element: TagValue) -> Vec<TagValue> {
        if let TagValue::Element(el) = element {
            self.elements
                .get(el)
                .cloned()
                .unwrap_or_else(|| {
                    eprintln!("unable to get element {el}");
                    self.elements.get(self.default).cloned().unwrap()
                })
                .tags
        } else {
            Vec::new()
        }
    }
}

pub const fn pos_in_world(pos: IVec2, world_size: usize) -> bool {
    pos.x >= 0 && pos.y >= 0 && pos.x < world_size as i32 && pos.y < world_size as i32
}
