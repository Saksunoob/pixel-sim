use crate::{world::World, RuleType};
use bevy::prelude::*;

use super::{CheckBox, Fonts, Panel, UICaptureMouse};

pub struct RulePanelPlugin;
impl Plugin for RulePanelPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, (handle_rule_clicks, handle_rule_text));
    }
}

pub fn spawn_rules_panel(
    info_panel: &mut ChildBuilder<'_, '_, '_>,
    fonts: &Fonts,
    world: &Res<World>,
    checkbox: &CheckBox,
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
            Panel("Rules".to_string()),
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
                    "Rules",
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
                    for (index, rule) in world.ruleset.rules.iter().enumerate() {
                        match rule {
                            RuleType::Rule { name, .. } => {
                                spawn_list_rule(list, name, (index, 0), 0, &fonts, &checkbox)
                            }
                            RuleType::CompoundRule { name, rules, .. } => {
                                spawn_list_rule(list, name, (index, 0), 0, &fonts, &checkbox);

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
}

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
                        font: fonts.get_font("Roboto"),
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

#[derive(Component)]
struct RuleListCheckbox((usize, usize));

#[derive(Component)]
struct RuleListText((usize, usize));

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
        if super::is_in_square(
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
