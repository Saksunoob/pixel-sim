use std::borrow::Borrow;

use bevy::{prelude::*, utils::HashMap};

use crate::world::World;

pub mod rules;

pub struct UIPlugin;

#[derive(Resource, Clone)]
pub struct CheckBox(Handle<Image>, Handle<Image>);
impl CheckBox {
    pub fn get_handle(&self, checked: bool) -> Handle<Image> {
        if checked {
            self.1.clone()
        } else {
            self.0.clone()
        }
    }
}

#[derive(Resource, Clone)]
pub struct Fonts(HashMap<String, Handle<Font>>);

impl Fonts {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn add_font<S>(&mut self, font_name: S, handle: Handle<Font>)
    where
        S: ToString,
    {
        self.0.insert(font_name.to_string(), handle);
    }
    pub fn get_font<S>(&self, font_name: S) -> Handle<Font>
    where
        S: ToString + std::fmt::Display,
    {
        self.0
            .get(&font_name.to_string())
            .cloned()
            .unwrap_or_else(|| {
                warn!("Unable to get font \"{font_name}\"");
                Handle::<Font>::default()
            })
    }
}

#[derive(Component, Clone)]
pub struct PanelToggle(Panel);

#[derive(Component, Clone, PartialEq)]
pub struct Panel(String);

impl Plugin for UIPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_plugins(rules::RulePanelPlugin);
        app.add_systems(Startup, setup)
            .add_systems(Update, toggle_menu);
    }
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>, world: Res<World>) {
    let mut fonts = Fonts::new();
    fonts.add_font("Roboto", asset_server.load("Roboto-Black.ttf"));
    commands.insert_resource(fonts.clone());

    let checkbox = CheckBox(
        asset_server.load("checkbox_unchecked.png"),
        asset_server.load("checkbox_checked.png"),
    );
    commands.insert_resource(checkbox.clone());

    commands
        .spawn(NodeBundle {
            style: Style {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                justify_content: JustifyContent::SpaceBetween,
                ..default()
            },
            ..default()
        })
        .with_children(|ui| {
            // Info panel
            ui.spawn(NodeBundle {
                style: Style {
                    width: Val::Auto,
                    height: Val::Percent(100.0),
                    align_self: AlignSelf::End,
                    flex_direction: FlexDirection::Row,
                    ..default()
                },
                ..default()
            })
            .with_children(|info_panel| {
                rules::spawn_rules_panel(info_panel, &fonts, &world, &checkbox);

                // Icons
                info_panel
                    .spawn(NodeBundle {
                        style: Style {
                            width: Val::Px(32.),
                            height: Val::Percent(100.),
                            flex_direction: FlexDirection::Column,
                            ..default()
                        },
                        ..default()
                    })
                    .with_children(|icon_section| {
                        let spawn_icon =
                            |icon_section: &mut ChildBuilder<'_, '_, '_>,
                             name: &str,
                             icon: Handle<Image>| {
                                icon_section
                                    .spawn((
                                        NodeBundle {
                                            style: Style {
                                                overflow: Overflow::clip_x(),
                                                width: Val::Px(16.),
                                                height: Val::Px(32.),
                                                margin: UiRect::new(
                                                    Val::Px(0.),
                                                    Val::Px(0.),
                                                    Val::Px(2.),
                                                    Val::Px(2.),
                                                ),
                                                justify_content: JustifyContent::End,
                                                ..default()
                                            },
                                            ..default()
                                        },
                                        PanelToggle(Panel(name.to_string())),
                                    ))
                                    .with_children(|icon_section| {
                                        icon_section.spawn(ImageBundle {
                                            style: Style {
                                                width: Val::Px(32.),
                                                height: Val::Px(32.),
                                                ..default()
                                            },
                                            image: UiImage::new(icon),
                                            ..default()
                                        });
                                    });
                            };

                        spawn_icon(icon_section, "Rules", asset_server.load("rules_icon.png"));
                        spawn_icon(
                            icon_section,
                            "Elements",
                            asset_server.load("elements_icon.png"),
                        );
                        spawn_icon(
                            icon_section,
                            "PixelInfo",
                            asset_server.load("pixel_info_icon.png"),
                        );
                    });
            });
        });
}

pub fn is_in_square(pos: Vec2, center: Vec2, side_length: Vec2) -> bool {
    let dx = (pos.x - center.x).abs();
    let dy = (pos.y - center.y).abs();
    return dx <= side_length.x / 2. && dy <= side_length.y / 2.;
}

fn toggle_menu(
    mut toggle_buttons: Query<(&GlobalTransform, &mut Style, &PanelToggle), Without<Panel>>,
    mut panels: Query<(&mut Style, &Panel)>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
) {
    let window = windows.single();
    let mouse_pos = window.cursor_position();
    if mouse_pos.is_none() {
        return;
    }
    let mouse_pos = mouse_pos.unwrap();

    toggle_buttons
        .iter_mut()
        .for_each(|(transform, mut style, toggle)| {
            let panel = panels.iter_mut().find(|(_, panel)| **panel == toggle.0);
            let state = if let Some((style, _)) = panel.borrow() {
                style.display == Display::Flex
            } else {
                false
            };

            let width = if let Val::Px(width) = style.width {
                width
            } else {
                32.
            };
            if is_in_square(
                mouse_pos,
                transform.translation().xy(),
                Vec2::new(width, 32.),
            ) || state
            {
                style.width = Val::Px(32.);
            } else {
                style.width = Val::Px(16.);
            }

            if is_in_square(mouse_pos, transform.translation().xy(), Vec2::splat(32.))
                && mouse.just_pressed(MouseButton::Left)
            {
                if let Some((mut panel_style, _)) = panel {
                    match state {
                        true => panel_style.display = Display::None,
                        false => panel_style.display = Display::Flex,
                    }
                }
            }
        });
}
