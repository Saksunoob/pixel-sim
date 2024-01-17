use bevy::prelude::*;

use crate::{world::World, RuleType};

pub struct UIPlugin;

impl Plugin for UIPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_systems(Startup, setup);
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
                        for rule in &world.ruleset.rules {
                            match rule {
                                RuleType::Rule { name, .. } => {
                                    list.spawn(TextBundle {
                                        style: Style { ..default() },
                                        text: Text::from_section(
                                            name,
                                            TextStyle {
                                                font: roboto.clone(),
                                                font_size: 16.,
                                                color: Color::WHITE,
                                            },
                                        ),
                                        ..default()
                                    });
                                }
                                RuleType::CompoundRule(name, rules) => {
                                    list.spawn(NodeBundle {
                                        style: Style {
                                            flex_direction: FlexDirection::Column,
                                            height: Val::Auto,
                                            ..default()
                                        },
                                        ..default()
                                    })
                                    .with_children(
                                        |section| {
                                            section.spawn(TextBundle {
                                                style: Style { ..default() },
                                                text: Text::from_section(
                                                    name,
                                                    TextStyle {
                                                        font: roboto.clone(),
                                                        font_size: 16.,
                                                        color: Color::WHITE,
                                                    },
                                                ),
                                                ..default()
                                            });

                                            for rule in rules {
                                                section.spawn(TextBundle {
                                                    style: Style {
                                                        margin: UiRect::left(Val::Px(10.)),
                                                        ..default()
                                                    },
                                                    text: Text::from_section(
                                                        rule.get_name(),
                                                        TextStyle {
                                                            font: roboto.clone(),
                                                            font_size: 16.,
                                                            color: Color::WHITE,
                                                        },
                                                    ),
                                                    ..default()
                                                });
                                            }
                                        },
                                    );
                                }
                            }
                        }
                    });
            });
        });
}
