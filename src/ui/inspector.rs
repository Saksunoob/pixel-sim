use crate::world::World;
use bevy::prelude::*;

use super::{Fonts, Panel};

pub struct InspectorPanelPlugin;
impl Plugin for InspectorPanelPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup);
        app.add_systems(Update, pixel_selector);
    }
}

#[derive(Component)]
struct PixelInspectOverlay;
#[derive(Component)]
struct PixelInspectHoverOverlay;

fn setup (mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((SpriteBundle {
        sprite: Sprite { custom_size: Some(Vec2::splat(2.)), ..default()},
        texture: asset_server.load("inspect_pixel_overlay.png"),
        ..default()
    }, PixelInspectOverlay));

    commands.spawn((SpriteBundle {
        sprite: Sprite { custom_size: Some(Vec2::splat(2.)), ..default()},
        texture: asset_server.load("inspect_pixel_overlay.png"),
        ..default()
    }, PixelInspectHoverOverlay));
}

pub fn spawn_inspector_panel(
    info_panel: &mut ChildBuilder<'_, '_, '_>,
    fonts: &Fonts,
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
            Panel("Inspector".to_string()),
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
                    "Inspector",
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
                        text: Text::from_sections([
                            TextSection::new("Position: ", TextStyle {
                                font: fonts.get_font("Roboto"),
                                font_size: 16.,
                                color: Color::WHITE,
                            }),
                            TextSection::new("Not Selected", TextStyle {
                                font: fonts.get_font("Roboto"),
                                font_size: 16.,
                                color: Color::GRAY,
                            }),
                            ]
                        ),
                        ..default()
                    });
                    list.spawn(TextBundle {
                        text: Text::from_section(
                            "Tags: ", 
                            TextStyle {
                                font: fonts.get_font("Roboto"),
                                font_size: 16.,
                                color: Color::WHITE,
                            }
                        ),
                        ..default()
                    });
                });
        });
}



fn pixel_selector(
    world: Res<World>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
    camera: Query<(&Camera, &GlobalTransform)>,
    mut hover_overlay: Query<(&mut Transform, &mut Visibility), With<PixelInspectHoverOverlay>>
) {
    let window = windows.single();
    let mouse_pos = window.cursor_position();
    if mouse_pos.is_none() {
        return;
    }
    let (camera, camera_transform) = camera.single();
    let mouse_pos = camera.viewport_to_world_2d(camera_transform, mouse_pos.unwrap()).unwrap();

    let mouse_pixel_pos = (mouse_pos/Vec2::new(2., -2.)).floor().as_ivec2()+IVec2::splat(world.world_size-1)/2;

    let (mut transform, mut visibility) = hover_overlay.single_mut();
    transform.translation = Vec3::new(mouse_pos.x/2., mouse_pos.y/2., 0.).floor()*2.+Vec3::ONE;
    *visibility = Visibility::Visible;

    if mouse.just_pressed(MouseButton::Left) {
        println!("{}", mouse_pixel_pos);
    }
}   
