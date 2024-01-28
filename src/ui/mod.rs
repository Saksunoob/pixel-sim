use std::{borrow::Borrow, f32::consts::PI};

use bevy::{prelude::*, utils::HashMap};

use crate::world::World;

mod elements;
mod inspector;
mod rules;

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

enum HorizontalSide {
    Left,
    Right,
}
enum VerticalSide {
    Top,
    Bottom,
}
enum AnimationType {
    EaseOutElastic,
}
#[derive(Component)]
pub struct PositionAnimation {
    animation_type: AnimationType,
    horizontal_side: HorizontalSide,
    vertical_side: VerticalSide,
    start: Vec2,
    end: Vec2,
    start_time: f32,
    duration: f32,
}
impl PositionAnimation {
    pub fn get_pos(&self, time: f32) -> Vec2 {
        match self.animation_type {
            AnimationType::EaseOutElastic => {
                const BOUNCES: f32 = (2. * PI) / 3.;
                let t = (time - self.start_time) / self.duration;

                if t <= 0. {
                    return self.start;
                }
                if t >= 1. {
                    return self.end;
                }

                let lerp = (2_f32).powf(-10. * t) * ((t * 10. - 0.75) * BOUNCES).sin() + 1.;
                self.start.lerp(self.end, lerp)
            }
        }
    }
}

#[derive(Component, Clone)]
pub struct PanelToggle(Panel);

#[derive(Component, Clone, PartialEq)]
pub struct Panel(String);

#[derive(Component)]
pub struct InfoPanel;

impl Plugin for UIPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.insert_resource(UIMouseCaptured(false));
        app.add_plugins((rules::RulePanelPlugin, inspector::InspectorPanelPlugin));
        app.add_systems(Startup, setup)
            .add_systems(PreUpdate, ui_capture_mouse)
            .add_systems(Update, (toggle_menu, apply_position_animation));
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
            ui.spawn((
                NodeBundle {
                    style: Style {
                        width: Val::Auto,
                        height: Val::Percent(100.0),
                        align_self: AlignSelf::End,
                        flex_direction: FlexDirection::Row,
                        ..default()
                    },
                    ..default()
                },
                InfoPanel,
                PositionAnimation {
                    animation_type: AnimationType::EaseOutElastic,
                    horizontal_side: HorizontalSide::Left,
                    vertical_side: VerticalSide::Top,
                    start: Vec2::ZERO,
                    end: Vec2::ZERO,
                    start_time: 0.,
                    duration: 0.1,
                },
            ))
            .with_children(|info_panel| {
                rules::spawn_rules_panel(info_panel, &fonts, &world, &checkbox);
                elements::spawn_elements_panel(info_panel, &fonts, &world, &asset_server);
                inspector::spawn_inspector_panel(info_panel, &fonts);

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
                                        UICaptureMouse,
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
                            "Inspector",
                            asset_server.load("inspector_icon.png"),
                        );
                    });
            });
        });
}

#[derive(Resource)]
struct UIMouseCaptured(bool);

#[derive(Component)]
struct UICaptureMouse;

fn ui_capture_mouse(
    mut mouse_captured: ResMut<UIMouseCaptured>,
    ui: Query<(&GlobalTransform, &Node), With<UICaptureMouse>>,
    windows: Query<&Window>,
) {
    mouse_captured.0 = false;
    let window = windows.single();
    let mouse_pos = window.cursor_position();
    if mouse_pos.is_none() {
        return;
    }
    let mouse_pos = mouse_pos.unwrap();

    for (transform, node) in ui.iter() {
        if node.logical_rect(transform).contains(mouse_pos) {
            mouse_captured.0 = true;
            break;
        }
    }
}

pub fn is_in_square(pos: Vec2, center: Vec2, side_length: Vec2) -> bool {
    let dx = (pos.x - center.x).abs();
    let dy = (pos.y - center.y).abs();
    return dx <= side_length.x / 2. && dy <= side_length.y / 2.;
}

fn toggle_menu(
    mut toggle_buttons: Query<(&GlobalTransform, &mut Style, &PanelToggle), Without<Panel>>,
    mut panels: Query<(&mut Style, &Panel)>,
    mut info_panel: Query<&mut PositionAnimation, With<InfoPanel>>,
    time: Res<Time>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
) {
    let window = windows.single();
    let mouse_pos = window.cursor_position();
    if mouse_pos.is_none() {
        return;
    }
    let mouse_pos = mouse_pos.unwrap();

    let mut panel_opened = None;
    let mut panel_open = false;

    toggle_buttons
        .iter_mut()
        .for_each(|(transform, mut style, toggle)| {
            let panel = panels.iter_mut().find(|(_, panel)| **panel == toggle.0);
            let state = if let Some((style, _)) = panel.borrow() {
                style.display == Display::Flex
            } else {
                false
            };

            panel_open = panel_open || state;

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
                if let Some((mut panel_style, panel)) = panel {
                    match state {
                        true => panel_style.display = Display::None,
                        false => {
                            panel_style.display = Display::Flex;
                            panel_opened = Some(panel.0.clone())
                        }
                    }
                }
            }
        });

    match panel_opened {
        Some(opened_panel) => {
            panels.iter_mut().for_each(|mut panel| {
                if panel.1 .0 != opened_panel {
                    panel.0.display = Display::None;
                }
            });
            if !panel_open {
                let mut info_panel = info_panel.single_mut();
                info_panel.start = Vec2::new(-50., 0.);
                info_panel.end = Vec2::new(0., 0.);
                info_panel.start_time = time.elapsed_seconds();
            }
        }
        None => return,
    }
}

fn apply_position_animation(mut objects: Query<(&mut Style, &PositionAnimation)>, time: Res<Time>) {
    for (mut style, animation) in objects.iter_mut() {
        let pos = animation.get_pos(time.elapsed_seconds());
        match &animation.horizontal_side {
            HorizontalSide::Left => style.left = Val::Px(pos.x),
            HorizontalSide::Right => style.right = Val::Px(pos.x),
        }
        match &animation.vertical_side {
            VerticalSide::Top => style.top = Val::Px(pos.y),
            VerticalSide::Bottom => style.bottom = Val::Px(pos.y),
        }
    }
}
