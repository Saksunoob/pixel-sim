use crate::world::World;
use bevy::prelude::*;

use super::{Fonts, Panel, UICaptureMouse, UIMouseCaptured};

pub struct InspectorPanelPlugin;
impl Plugin for InspectorPanelPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(InspectingPixel(None));
        app.add_systems(Startup, setup);
        app.add_systems(Update, (pixel_selector, update_inspector));
    }
}

#[derive(Component)]
struct PixelInspectOverlay;
#[derive(Component)]
struct PixelInspectHoverOverlay;

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        SpriteBundle {
            sprite: Sprite {
                custom_size: Some(Vec2::splat(2.)),
                ..default()
            },
            texture: asset_server.load("inspect_pixel_overlay.png"),
            visibility: Visibility::Hidden,
            ..default()
        },
        PixelInspectOverlay,
    ));

    commands.spawn((
        SpriteBundle {
            sprite: Sprite {
                custom_size: Some(Vec2::splat(2.)),
                ..default()
            },
            texture: asset_server.load("inspect_pixel_overlay.png"),
            visibility: Visibility::Hidden,
            ..default()
        },
        PixelInspectHoverOverlay,
    ));
}

#[derive(Resource)]
struct InspectingPixel(Option<IVec2>);

#[derive(Component)]
struct InspectorText;

pub fn spawn_inspector_panel(info_panel: &mut ChildBuilder<'_, '_, '_>, fonts: &Fonts) {
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
                    list.spawn((
                        TextBundle {
                            text: Text::from_sections([
                                TextSection::new(
                                    "Pixel: ",
                                    TextStyle {
                                        font: fonts.get_font("Roboto"),
                                        font_size: 16.,
                                        color: Color::WHITE,
                                    },
                                ),
                                TextSection::new(
                                    "Not Selected",
                                    TextStyle {
                                        font: fonts.get_font("Roboto"),
                                        font_size: 16.,
                                        color: Color::GRAY,
                                    },
                                ),
                            ]),
                            ..default()
                        },
                        InspectorText,
                    ));
                });
        });
}

fn update_inspector(
    inspecting: Res<InspectingPixel>,
    world: Res<World>,
    mut inspector_text: Query<&mut Text, With<InspectorText>>,
    fonts: Res<Fonts>,
) {
    let mut inspector_text = inspector_text.single_mut();
    inspector_text.sections = Vec::new();

    let mut add_section = |text: &str, indent: usize, color: Color| {
        inspector_text.sections.push(TextSection::new(
            format!("{}{text}\n", "   ".repeat(indent)),
            TextStyle {
                font: fonts.get_font("Roboto"),
                font_size: 16.,
                color,
            },
        ));
    };

    match inspecting.0 {
        Some(pos) => {
            add_section("Pixel: ", 0, Color::WHITE);
            add_section(
                &world.names[(pos.y * world.world_size + pos.x) as usize],
                1,
                Color::WHITE,
            );

            add_section("Position: ", 0, Color::WHITE);
            add_section(&format!("x: {}", pos.x), 1, Color::WHITE);
            add_section(&format!("y: {}", pos.y), 1, Color::WHITE);

            add_section("Tags: ", 0, Color::WHITE);

            for (name, value) in world
                .bits
                .iter()
                .map(|(name, space)| (name, space.get_tag(pos)))
            {
                add_section(
                    &format!("{}: {}", name, value.to_string(&world.elements)),
                    1,
                    Color::WHITE,
                );
            }
        }
        None => {
            add_section("Pixel: ", 0, Color::WHITE);
            add_section("Not Selected", 1, Color::GRAY);
        }
    }
}

fn pixel_selector(
    world: Res<World>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
    camera: Query<(&Camera, &GlobalTransform)>,
    panels: Query<(&Style, &Panel)>,
    mouse_captured: Res<UIMouseCaptured>,
    mut inspecting: ResMut<InspectingPixel>,
    mut hover_overlay: Query<
        (&mut Transform, &mut Visibility),
        (With<PixelInspectHoverOverlay>, Without<PixelInspectOverlay>),
    >,
    mut overlay: Query<(&mut Transform, &mut Visibility), With<PixelInspectOverlay>>,
) {
    let window = windows.single();
    let mouse_pos = window.cursor_position();

    let mut hover_overlay = hover_overlay.single_mut();
    let mut overlay = overlay.single_mut();

    let inspect_panel = panels.iter().find(|(_, panel)| panel.0 == "Inspector").unwrap();

    // Set overlay visibility
    if inspect_panel.0.display == Display::Flex {
        *hover_overlay.1 = Visibility::Visible;
        if inspecting.0.is_some() {
            *overlay.1 = Visibility::Visible;
        } else {
            *overlay.1 = Visibility::Hidden;
        }
    } else {
        *hover_overlay.1 = Visibility::Hidden;
        *overlay.1 = Visibility::Hidden;
    }

    // Check if mouse is on the simulaton
    if mouse_pos.is_none() || mouse_captured.0 {
        *hover_overlay.1 = Visibility::Hidden;
        return;
    }

    // Calculate mouse position as simulation pixels
    let (camera, camera_transform) = camera.single();
    let mouse_pos = camera
        .viewport_to_world_2d(camera_transform, mouse_pos.unwrap())
        .unwrap();

    let mouse_pixel_pos =
        (mouse_pos / Vec2::new(2., -2.)).floor().as_ivec2() + IVec2::splat(world.world_size) / 2;

    // Set set hover overlay position
    hover_overlay.0.translation =
        Vec3::new(mouse_pos.x / 2., mouse_pos.y / 2., 0.).floor() * 2. + Vec3::ONE;

    if mouse.just_pressed(MouseButton::Left) {
        // If click happened on the simulation
        if mouse_pixel_pos.x >= 0 && mouse_pixel_pos.y >= 0 && mouse_pixel_pos.x < world.world_size && mouse_pixel_pos.y < world.world_size {
            // Select clicked pixel
            inspecting.0 = Some(mouse_pixel_pos);
            println!("{}", mouse_pixel_pos);
            // Set selection overlay position
            overlay.0.translation =
            Vec3::new(mouse_pos.x / 2., mouse_pos.y / 2., 0.).floor() * 2. + Vec3::ONE;
        } else { // If click happened outside the simulation
            // Deselect
            inspecting.0 = None;
        }
        
    }
}
