use crate::{tags::TagValue, world::World};
use bevy::{ecs::world, prelude::*, render::render_resource::Extent3d};

use super::{elements, Fonts, Panel, UICaptureMouse};

enum EditorTool {
    Single,
    Rectangle,
    Eraser
}

#[derive(Resource)]
struct EditorState {
    tool: Option<EditorTool>,
    element: Option<usize>
}

#[derive(Component)]
struct EditorToolButton(EditorTool);

pub struct RulePanelPlugin;
impl Plugin for RulePanelPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(EditorState {
            tool: None,
            element: None,
        });
    }
}

pub fn spawn_editor_panel(
    info_panel: &mut ChildBuilder<'_, '_, '_>,
    fonts: &Fonts,
    world: &Res<World>,
    asset_server: &Res<AssetServer>
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
            Panel("Editor".to_string()),
            UICaptureMouse,
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
                    "Editor",
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
                    list.spawn(TextBundle {
                        text: Text::from_section(
                            "Tools",
                            TextStyle {
                                font: fonts.get_font("Roboto"),
                                font_size: 16.,
                                color: Color::WHITE,
                            },
                        ),
                        ..default()
                    });
                    list.spawn(NodeBundle {
                        style: Style {
                            flex_direction: FlexDirection::Row,
                            height: Val::Auto,
                            overflow: Overflow::clip_y(),
                            ..default()
                        },
                        ..default()
                    }).with_children(|tools| {
                        add_tool_button(tools, EditorTool::Single, "editor_icon_single.png", asset_server);
                        add_tool_button(tools, EditorTool::Rectangle, "editor_icon_rectangle.png", asset_server);
                        add_tool_button(tools, EditorTool::Eraser, "editor_icon_eraser.png", asset_server);
                    });
                    list.spawn(TextBundle {
                        text: Text::from_section(
                            "Elements",
                            TextStyle {
                                font: fonts.get_font("Roboto"),
                                font_size: 16.,
                                color: Color::WHITE,
                            },
                        ),
                        ..default()
                    });
                    for element in &world.elements.elements {
                        let color_tag = world.state.tags.get_index("color");
                        let color = match color_tag {
                            Some(color_tag) => element.tags[color_tag],
                            None => TagValue::Integer(0),
                        };
                        let color = if let TagValue::Integer(value) = color {
                            [
                                (value >> 16) as u8 as f32,
                                (value >> 8) as u8 as f32,
                                value as u8 as f32,
                                255 as f32,
                            ]
                        } else {
                            Color::PURPLE.as_rgba_f32()
                        };
                        add_element_button(list, &element.name, fonts, &color, asset_server)
                        
                    }
                });
        });
}

fn add_tool_button(tools: &mut ChildBuilder, tool: EditorTool, img_path: &'static str, asset_server: &Res<AssetServer>) {
    tools.spawn(NodeBundle {
        style: Style {
            height: Val::Px(32.),
            width: Val::Px(32.),
            margin: UiRect::all(Val::Px(2.)),
            ..default()
        },
        ..default()
    }).with_children(|button| {
        button.spawn(ImageBundle {
            image: UiImage::new(asset_server.load(img_path)),
            ..default()
        });
    });
}

fn add_element_button(
    list: &mut ChildBuilder,
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