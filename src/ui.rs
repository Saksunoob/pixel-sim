use bevy::{prelude::*, utils::HashMap};

use crate::{world::World, RuleType};

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
    pub fn get_font<S>(&self, font_name: S) -> Option<Handle<Font>>
    where
        S: ToString,
    {
        self.0.get(&font_name.to_string()).cloned()
    }
}

#[derive(Component, Clone)]
pub struct PanelToggle(Panel);

#[derive(Component, Clone, PartialEq)]
pub struct Panel(String);

impl Plugin for UIPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_systems(Startup, setup)
            .add_systems(Update, (handle_rule_clicks, handle_rule_text, toggle_menu));
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
                // Rules panel
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
                            border_color: BorderColor(Color::Rgba {
                                red: 0.2,
                                green: 0.2,
                                blue: 0.2,
                                alpha: 0.9,
                            }),
                            ..default()
                        },
                        Panel("Rules".to_string()),
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
                                "Rules",
                                TextStyle {
                                    font: fonts.get_font("Roboto").unwrap_or_default(),
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
                                for (index, rule) in world.ruleset.rules.iter().enumerate() {
                                    match rule {
                                        RuleType::Rule { name, .. } => spawn_list_rule(
                                            list,
                                            name,
                                            (index, 0),
                                            0,
                                            &fonts,
                                            &checkbox,
                                        ),
                                        RuleType::CompoundRule { name, rules, .. } => {
                                            spawn_list_rule(
                                                list,
                                                name,
                                                (index, 0),
                                                0,
                                                &fonts,
                                                &checkbox,
                                            );

                                            for (lower_index, rule) in rules.iter().enumerate() {
                                                spawn_list_rule(
                                                    list,
                                                    rule.get_name(),
                                                    (index, lower_index + 1),
                                                    1,
                                                    &fonts,
                                                    &checkbox,
                                                );
                                            }
                                        }
                                    }
                                }
                            });
                    });
                // Icons
                info_panel
                    .spawn(NodeBundle {
                        style: Style {
                            width: Val::Px(36.),
                            height: Val::Percent(100.),
                            flex_direction: FlexDirection::Column,
                            ..default()
                        },
                        ..default()
                    })
                    .with_children(|icon_section| {
                        // Rules Icon
                        icon_section.spawn((
                            ImageBundle {
                                style: Style {
                                    width: Val::Px(32.),
                                    height: Val::Px(32.),
                                    margin: UiRect::all(Val::Px(2.)),
                                    ..default()
                                },
                                image: UiImage::new(asset_server.load("rules_icon.png")),
                                ..default()
                            },
                            PanelToggle(Panel("Rules".to_string())),
                        ));
                        // Elements Icon
                        icon_section.spawn((
                            ImageBundle {
                                style: Style {
                                    width: Val::Px(32.),
                                    height: Val::Px(32.),
                                    margin: UiRect::all(Val::Px(2.)),
                                    ..default()
                                },
                                image: UiImage::new(asset_server.load("elements_icon.png")),
                                ..default()
                            },
                            PanelToggle(Panel("Elements".to_string())),
                        ));
                        // Pixel info Icon
                        icon_section.spawn((
                            ImageBundle {
                                style: Style {
                                    width: Val::Px(32.),
                                    height: Val::Px(32.),
                                    margin: UiRect::all(Val::Px(2.)),
                                    ..default()
                                },
                                image: UiImage::new(asset_server.load("pixel_info_icon.png")),
                                ..default()
                            },
                            PanelToggle(Panel("PixelInfo".to_string())),
                        ));
                    });
            });
        });
}

#[derive(Component)]
struct RuleListCheckbox((usize, usize));

#[derive(Component)]
struct RuleListText((usize, usize));

fn spawn_list_rule(
    list: &mut ChildBuilder<'_, '_, '_>,
    name: &str,
    index: (usize, usize),
    indent: u64,
    fonts: &Fonts,
    check_box: &CheckBox,
) {
    list.spawn(NodeBundle {
        style: Style {
            flex_direction: FlexDirection::Row,
            height: Val::Auto,
            margin: UiRect::left(Val::Px(10. * indent as f32)),
            ..default()
        },
        ..default()
    })
    .with_children(|row| {
        //Checkbox
        row.spawn((
            ImageBundle {
                style: Style {
                    width: Val::Px(16.),
                    height: Val::Px(16.),
                    margin: UiRect::all(Val::Px(2.)),
                    ..default()
                },
                image: UiImage::new(check_box.get_handle(true)),
                ..default()
            },
            RuleListCheckbox(index),
        ));
        // Rule name
        row.spawn((
            TextBundle {
                style: Style {
                    margin: UiRect::vertical(Val::Auto),
                    ..default()
                },
                text: Text::from_section(
                    name,
                    TextStyle {
                        font: fonts.get_font("Roboto").unwrap_or_default(),
                        font_size: 16.,
                        color: Color::WHITE,
                    },
                ),
                ..default()
            },
            RuleListText(index),
        ));
    });
}

fn handle_rule_clicks(
    mut check_boxes: Query<(&GlobalTransform, &mut UiImage, &RuleListCheckbox)>,
    checkbox_res: Res<CheckBox>,
    mut world: ResMut<World>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
) {
    let window = windows.single();
    let mouse_pos = window.cursor_position();
    if mouse_pos.is_none() || !mouse.just_pressed(MouseButton::Left) {
        return;
    }
    let mouse_pos = mouse_pos.unwrap();

    for (transform, mut image, check_box) in check_boxes.iter_mut() {
        if is_in_square(
            mouse_pos,
            transform.translation().truncate(),
            Vec2::splat(16.),
        ) {
            let rule = world.ruleset.get_index_mut(check_box.0).unwrap();
            if rule.self_enabled() {
                rule.set_enabled(false);
                image.texture = checkbox_res.get_handle(false);
            } else {
                rule.set_enabled(true);
                image.texture = checkbox_res.get_handle(true);
            }
        }
    }
}

fn is_in_square(pos: Vec2, center: Vec2, side_length: Vec2) -> bool {
    let dx = (pos.x - center.x).abs();
    let dy = (pos.y - center.y).abs();
    return dx <= side_length.x / 2. && dy <= side_length.y / 2.;
}

fn toggle_menu(
    toggle_buttons: Query<(&GlobalTransform, &PanelToggle)>,
    mut panels: Query<(&mut Style, &Panel)>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
) {
    let window = windows.single();
    let mouse_pos = window.cursor_position();
    if mouse_pos.is_none() || !mouse.just_pressed(MouseButton::Left) {
        return;
    }
    let mouse_pos = mouse_pos.unwrap();

    toggle_buttons.iter().for_each(|(transform, toggle)| {
        if is_in_square(mouse_pos, transform.translation().xy(), Vec2::splat(32.)) {
            let panel = panels.iter_mut().find(|(_, panel)| **panel == toggle.0);
            if let Some((mut style, _)) = panel {
                let state = style.display == Display::Flex;
                match state {
                    true => style.display = Display::None,
                    false => style.display = Display::Flex,
                }
            }
        }
    });
}

fn handle_rule_text(mut items: Query<(&mut Text, &RuleListText)>, mut world: ResMut<World>) {
    const ENABLED_COLOR: Color = Color::WHITE;
    const DISABLED_COLOR: Color = Color::GRAY;

    for (mut text, rule_index) in items.iter_mut() {
        let rule = world.ruleset.get_index_mut(rule_index.0).unwrap();

        match rule.enabled() {
            true => text
                .sections
                .iter_mut()
                .for_each(|section| section.style.color = ENABLED_COLOR),
            false => text
                .sections
                .iter_mut()
                .for_each(|section| section.style.color = DISABLED_COLOR),
        }
    }
}
