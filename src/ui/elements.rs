use crate::{tags::TagValue, world::World};
use bevy::{prelude::*, render::render_resource::Extent3d};

use super::{Fonts, Panel, UICaptureMouse};

pub fn spawn_elements_panel(
    info_panel: &mut ChildBuilder<'_, '_, '_>,
    fonts: &Fonts,
    world: &Res<World>,
    asset_server: &Res<AssetServer>,
) {
    info_panel
        .spawn((
            NodeBundle {
                style: Style {
                    display: Display::None,
                    width: Val::Vw(20.),
                    height: Val::Percent(100.0),
                    align_self: AlignSelf::End,
                    flex_direction: FlexDirection::Column,
                    border: UiRect::right(Val::Px(2.)),
                    ..default()
                },
                background_color: BackgroundColor(Color::Rgba {
                    red: 0.1,
                    green: 0.1,
                    blue: 0.1,
                    alpha: 0.8,
                }),
                border_color: BorderColor(Color::WHITE),
                ..default()
            },
            Panel("Elements".to_string()),
            UICaptureMouse
        ))
        // Content
        .with_children(|rules_panel| {
            // Title
            rules_panel.spawn(TextBundle {
                style: Style {
                    margin: UiRect::horizontal(Val::Auto),
                    ..default()
                },
                text: Text::from_section(
                    "Elements",
                    TextStyle {
                        font: fonts.get_font("Roboto"),
                        font_size: 30.,
                        color: Color::WHITE,
                    },
                ),
                ..default()
            });
            // List
            rules_panel
                .spawn(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::Column,
                        height: Val::Auto,
                        padding: UiRect::horizontal(Val::Px(5.)),
                        overflow: Overflow::clip_y(),
                        ..default()
                    },
                    ..default()
                })
                .with_children(|list| {
                    for element in world.elements.elements.iter() {
                        let color_tag = element.1.tags.iter().find(|(tag, _)| tag == "color");
                        let color = match color_tag {
                            Some((_, value)) => {
                                if let TagValue::Integer(value) = value {
                                    [
                                        (value >> 16) as u8 as f32,
                                        (value >> 8) as u8 as f32,
                                        *value as u8 as f32,
                                        255 as f32,
                                    ]
                                } else {
                                    Color::PURPLE.as_rgba_f32()
                                }
                            }
                            None => todo!(),
                        };
                        spawn_list_element(list, &element.1.name, fonts, &color, asset_server);
                    }
                });
        });
}

fn spawn_list_element(
    list: &mut ChildBuilder<'_, '_, '_>,
    name: &str,
    fonts: &Fonts,
    color: &[f32; 4],
    asset_server: &Res<AssetServer>,
) {
    list.spawn(NodeBundle {
        style: Style {
            flex_direction: FlexDirection::Row,
            height: Val::Auto,
            ..default()
        },
        ..default()
    })
    .with_children(|row| {
        let data: Vec<u8> = (0..4)
            .flat_map(|index| (color[index] / 255.).to_ne_bytes())
            .collect();
        let image = Image::new_fill(
            Extent3d::default(),
            bevy::render::render_resource::TextureDimension::D2,
            data.as_slice(),
            bevy::render::render_resource::TextureFormat::Rgba32Float,
        );

        // Color
        row.spawn(ImageBundle {
            style: Style {
                width: Val::Px(16.),
                height: Val::Px(16.),
                margin: UiRect::all(Val::Px(2.)),
                ..default()
            },
            image: UiImage::new(asset_server.add(image)),
            ..default()
        });
        // Element name
        row.spawn(TextBundle {
            style: Style {
                margin: UiRect::vertical(Val::Auto),
                ..default()
            },
            text: Text::from_section(
                name,
                TextStyle {
                    font: fonts.get_font("Roboto"),
                    font_size: 16.,
                    color: Color::WHITE,
                },
            ),
            ..default()
        });
    });
}
