use crate::hash;
use crate::rules::*;
use bevy::prelude::*;
use bevy::{
    app::Plugin,
    asset::{AssetServer, Handle},
    ecs::system::{Commands, Res, Resource},
    math::{IVec2, Vec2},
    render::{
        color::Color,
        render_resource::{Extent3d, TextureDimension, TextureFormat},
        texture::Image,
    },
    sprite::{Sprite, SpriteBundle},
    transform::components::Transform,
};
use rayon::prelude::*;
use std::collections::HashMap;
use std::ops::{Add, Sub, Mul, Div};

pub struct WorldPlugin(pub World);

impl Plugin for WorldPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.insert_resource(self.0.clone())
            .add_systems(Startup, setup)
            .add_systems(Update, (update, simulation_step));
    }
}

#[derive(Copy, Clone)]
pub enum TagValue {
    None,
    Empty,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Element(u64),
}
impl TagValue {
    pub fn as_float(&self) -> f64 {
        match self {
            TagValue::None => 0.,
            TagValue::Empty => 0.,
            TagValue::Integer(i) => *i as f64,
            TagValue::Float(f) => *f,
            TagValue::Boolean(v) => {
                if *v {
                    1.
                } else {
                    0.
                }
            }
            TagValue::Element(el) => *el as f64,
        }
    }
}
impl PartialEq for TagValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Element(l0), Self::Element(r0)) => l0 == r0,
            (Self::None, Self::None) | (Self::Empty, Self::Empty) => true,
            _ => false,
        }
    }
}
impl PartialOrd for TagValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (TagValue::Integer(i1), TagValue::Integer(i2)) => Some(i1.cmp(i2)),
            (TagValue::Float(f1), TagValue::Float(f2)) => Some(f1.total_cmp(f2)),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => Some(b1.cmp(b2)),
            (TagValue::Float(f1), TagValue::Integer(i2)) => Some(f1.total_cmp(&(*i2 as f64))),
            (TagValue::Integer(i1), TagValue::Float(f2)) => Some((*i1 as f64).total_cmp(f2)),
            _ => None
        }
    }
}
impl Add for TagValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (TagValue::Empty, TagValue::Empty) | (TagValue::Empty, TagValue::None) | (TagValue::None, TagValue::Empty)  => TagValue::Empty,
            (TagValue::Empty, other) | (other, TagValue::Empty) | (TagValue::None, other) | (other, TagValue::None) => other,
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1+v2),
            (TagValue::Integer(v2), TagValue::Float(v1)) | (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1+v2 as f64),
            (TagValue::Boolean(v2), TagValue::Float(v1)) | (TagValue::Float(v1), TagValue::Boolean(v2)) => TagValue::Float(v1+v2 as i64 as f64),
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1+v2),
            (TagValue::Boolean(b), TagValue::Integer(i)) | (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i+b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 | b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => panic!("Cannot do arithmatic with Tag value of type Element"),
        }
    }
}
impl Sub for TagValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (TagValue::Empty, TagValue::Empty) | (TagValue::Empty, TagValue::None) | (TagValue::None, TagValue::Empty)  => TagValue::Empty,
            (other, TagValue::Empty) | (other, TagValue::None) => other,
            (TagValue::Empty, TagValue::Float(f)) | (TagValue::None, TagValue::Float(f)) => TagValue::Float(-f),
            (TagValue::Empty, TagValue::Integer(i)) | (TagValue::None, TagValue::Integer(i)) => TagValue::Integer(-i),
            (TagValue::Empty, TagValue::Boolean(b)) | (TagValue::None, TagValue::Boolean(b)) => TagValue::Boolean(!b),
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1-v2),
            (TagValue::Integer(v1), TagValue::Float(v2)) => TagValue::Float(v1 as f64-v2),
            (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1-v2 as f64),
            (TagValue::Boolean(v1), TagValue::Float(v2)) => TagValue::Float(v1 as i64 as f64-v2),
            (TagValue::Float(v1), TagValue::Boolean(v2)) => TagValue::Float(v1-v2 as i64 as f64),
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1-v2),
            (TagValue::Boolean(b), TagValue::Integer(i)) => TagValue::Integer(b as i64-i),
            (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i-b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 & !b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => panic!("Cannot do arithmatic with Tag value of type Element"),
        }
    }
}
impl Mul for TagValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (TagValue::Empty, TagValue::Empty) | (TagValue::Empty, TagValue::None) | (TagValue::None, TagValue::Empty)  => TagValue::Empty,
            (_, TagValue::Empty) | (_, TagValue::None) => TagValue::Empty,
            (TagValue::Empty, TagValue::Float(_)) | (TagValue::None, TagValue::Float(_)) => TagValue::Float(0.),
            (TagValue::Empty, TagValue::Integer(_)) | (TagValue::None, TagValue::Integer(_)) => TagValue::Integer(0),
            (TagValue::Empty, TagValue::Boolean(_)) | (TagValue::None, TagValue::Boolean(_)) => TagValue::Boolean(false),
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1*v2),
            (TagValue::Integer(v2), TagValue::Float(v1)) | (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1*v2 as f64),
            (TagValue::Boolean(v2), TagValue::Float(v1)) | (TagValue::Float(v1), TagValue::Boolean(v2)) => TagValue::Float(v1*v2 as i64 as f64),
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1*v2),
            (TagValue::Boolean(b), TagValue::Integer(i)) | (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i*b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 & b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => panic!("Cannot do arithmatic with Tag value of type Element"),
        }
    }
}
impl Div for TagValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TagValue::None, TagValue::None) => TagValue::None,
            (TagValue::Empty, TagValue::Empty) | (TagValue::Empty, TagValue::None) | (TagValue::None, TagValue::Empty)  => TagValue::Empty,
            (_, TagValue::Empty) | (_, TagValue::None) => TagValue::Empty,
            (TagValue::Empty, TagValue::Float(_)) | (TagValue::None, TagValue::Float(_)) => TagValue::Float(0.),
            (TagValue::Empty, TagValue::Integer(_)) | (TagValue::None, TagValue::Integer(_)) => TagValue::Integer(0),
            (TagValue::Empty, TagValue::Boolean(_)) | (TagValue::None, TagValue::Boolean(_)) => TagValue::Boolean(false),
            (TagValue::Integer(v1), TagValue::Integer(v2)) => TagValue::Integer(v1/v2),
            (TagValue::Integer(v1), TagValue::Float(v2)) => TagValue::Float(v1 as f64/v2),
            (TagValue::Float(v1), TagValue::Integer(v2)) => TagValue::Float(v1/v2 as f64),
            (TagValue::Boolean(v1), TagValue::Float(v2)) => TagValue::Float(v1 as i64 as f64/v2),
            (TagValue::Float(v1), TagValue::Boolean(v2)) => TagValue::Float(v1/v2 as i64 as f64),
            (TagValue::Float(v1), TagValue::Float(v2)) => TagValue::Float(v1*v2),
            (TagValue::Boolean(b), TagValue::Integer(i)) => TagValue::Integer(b as i64*i),
            (TagValue::Integer(i), TagValue::Boolean(b)) => TagValue::Integer(i*b as i64),
            (TagValue::Boolean(b1), TagValue::Boolean(b2)) => TagValue::Boolean(b1 ^ b2),
            (TagValue::Element(_), _) | (_, TagValue::Element(_)) => panic!("Cannot do arithmatic with Tag value of type Element"),
        }
    }
}

#[derive(Clone)]
pub struct TagSpace {
    array: Vec<TagValue>,
    world_size: i32
}
impl TagSpace {
    pub fn new(world_size: i32) -> Self {
        Self { array: vec![TagValue::None; (world_size*world_size) as usize], world_size }
    }
    pub fn new_with_value(value: TagValue, world_size: i32) -> Self {
        Self { array: vec![value; (world_size*world_size) as usize], world_size }
    }
    pub fn get_tag(&self, pos: IVec2) -> TagValue {
        let index = pos.y*self.world_size+pos.x;
        *self.array.get(index as usize).unwrap_or(&TagValue::None)
    }
    pub fn get_rel_tag(&self, origin: IVec2, pos: IVec2) -> TagValue {
        let pos = origin.wrapping_add(pos);
        if pos.x < 0 || pos.y < 0 || pos.x >= self.world_size || pos.y >= self.world_size {
            return TagValue::None;
        }

        let index = pos.y*self.world_size+pos.x;
        *self.array.get(index as usize).unwrap_or(&TagValue::None)
    }
    pub fn get_tag_at_index(&self, index: i32) -> TagValue {
        if index < 0 {
            return TagValue::None;
        }
        *self.array.get(index as usize).unwrap_or(&TagValue::None)
    }
    pub fn par_iter_mut(&mut self) -> rayon::slice::IterMut<'_, TagValue> {
        self.array.par_iter_mut()
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
    pub fn new(size: usize, elements: Vec<(&str, Vec<(&str, TagValue)>)>, ruleset: Ruleset) -> Self {
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
                        (value >> 24) as u8,
                        (value >> 16) as u8,
                        (value >> 8) as u8,
                        value as u8,
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

#[derive(Clone)]
pub struct Elements {
    elements: HashMap<u64, Vec<(String, TagValue)>>,
    default: u64,
}
impl Elements {
    pub fn new(elements: Vec<(&str, Vec<(&str, TagValue)>)>) -> Self {
        let default = hash(elements[0].0);
        Self {
            elements: elements
                .into_iter()
                .map(|(id, tags)| {
                    (
                        hash(id),
                        tags.into_iter()
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