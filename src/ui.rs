use bevy::prelude::*;

use crate::{world::World, RuleType};

pub struct UIPlugin;

impl Plugin for UIPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_systems(Startup, setup)
            .add_systems(Update, handle_rule_clicks);
    }
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>, world: Res<World>) {
    let roboto = asset_server.load("Roboto-Black.ttf");

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
            // Simulation information
            ui.spawn(NodeBundle {
                style: Style {
                    width: Val::Percent(20.0),
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
            })
            // Rules panel
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
                            font: roboto.clone(),
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
                                RuleType::Rule { name, .. } => {
                                    spawn_list_rule(list, name, (index, 0), 0, &roboto)
                                }
                                RuleType::CompoundRule { name, rules, .. } => {
                                    spawn_list_rule(list, name, (index, 0), 0, &roboto);

                                    for (lower_index, rule) in rules.iter().enumerate() {
                                        spawn_list_rule(
                                            list,
                                            rule.get_name(),
                                            (index, lower_index + 1),
                                            1,
                                            &roboto,
                                        );
                                    }
                                }
                            }
                        }
                    });
            });
        });
}

#[derive(Component)]
struct RuleListCheckbox((usize, usize));

fn spawn_list_rule(
    list: &mut ChildBuilder<'_, '_, '_>,
    name: &str,
    index: (usize, usize),
    indent: u64,
    font: &Handle<Font>,
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
            NodeBundle {
                style: Style {
                    margin: UiRect::new(Val::Px(0.), Val::Px(4.), Val::Px(2.), Val::Px(2.)),
                    width: Val::Px(16.),
                    height: Val::Px(16.),
                    padding: UiRect::all(Val::Auto),
                    border: UiRect::all(Val::Px(2.)),
                    ..default()
                },
                border_color: BorderColor(Color::rgb(0.2, 0.2, 0.2)),
                background_color: BackgroundColor(Color::rgb(0.3, 0.3, 0.3)),
                ..default()
            },
            RuleListCheckbox(index),
        ))
        .with_children(|container| {
            container.spawn(TextBundle {
                style: Style {
                    margin: UiRect::all(Val::Auto),
                    ..default()
                },
                text: Text::from_section(
                    "X",
                    TextStyle {
                        font: font.clone(),
                        font_size: 16.,
                        color: Color::WHITE,
                    },
                ),
                ..default()
            });
        });

        row.spawn(TextBundle {
            style: Style {
                margin: UiRect::vertical(Val::Auto),
                ..default()
            },
            text: Text::from_section(
                name,
                TextStyle {
                    font: font.clone(),
                    font_size: 16.,
                    color: Color::WHITE,
                },
            ),
            ..default()
        });
    });
}

fn handle_rule_clicks(
    check_boxes: Query<(&GlobalTransform, &Children, &RuleListCheckbox)>,
    mut texts: Query<&mut Text>,
    mut world: ResMut<World>,
    mouse: Res<Input<MouseButton>>,
    windows: Query<&Window>,
) {
    let window = windows.single();
    let click_pos = match window.cursor_position() {
        Some(pos) => pos,
        None => Vec2::new(0., 0.),
    };
    for (transform, children, check_box) in check_boxes.iter() {
        if mouse.just_pressed(MouseButton::Left)
            && transform.translation().truncate().distance(click_pos) <= 10.
        {
            let child = children.first().unwrap();
            let mut text = texts.get_mut(*child).unwrap();
            let rule = world.ruleset.get_index_mut(check_box.0).unwrap();
            text.sections[0].value = if rule.enabled() {
                rule.set_enabled(false);
                " ".to_string()
            } else {
                rule.set_enabled(true);
                "X".to_string()
            };
        }
    }
}
