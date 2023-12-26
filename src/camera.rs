use bevy::{input::mouse::MouseWheel, prelude::*};

const ZOOM_LEVELS: [f32; 6] = [0.2, 0.25, 1. / 3., 0.5, 1., 2.];

pub struct CameraPlugin;
impl Plugin for CameraPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup)
            .add_systems(Update, (controls, scale_with_zoom));
    }
}

#[derive(Component)]
pub struct ScaleWithZoom(pub f32, pub f32);

#[derive(Component)]
pub struct PixelPerfectPos(Vec2);

fn scale_with_zoom(
    mut query: Query<(&mut Transform, &ScaleWithZoom)>,
    camera: Query<(&OrthographicProjection, &Camera)>,
) {
    let cams: Vec<&OrthographicProjection> = camera
        .iter()
        .filter_map(|(proj, cam)| match cam.is_active {
            true => Some(proj),
            false => None,
        })
        .collect();

    if cams.len() >= 1 {
        let scale = cams[0].scale;
        for (mut transform, scale_state) in query.iter_mut() {
            let clamped_scale = scale.clamp(scale_state.0, scale_state.1);
            transform.scale = Vec3::new(clamped_scale, clamped_scale, 1.);
        }
    }
}

fn setup(mut commands: Commands) {
    commands.spawn((Camera2dBundle::default(), PixelPerfectPos(Vec2::ZERO)));
}

fn get_zoom_level(y: f32, current: f32) -> f32 {
    if y == 0. {
        return current;
    }
    let index = ZOOM_LEVELS
        .iter()
        .enumerate()
        .find_map(|(i, level)| if *level == current { Some(i) } else { None })
        .unwrap_or(1);
    if y < 0. {
        return *ZOOM_LEVELS.get(index + 1).unwrap_or(&current);
    } else if index == 0 {
        return current;
    } else {
        return *ZOOM_LEVELS.get(index - 1).unwrap_or(&current);
    }
}

fn controls(
    mut cameras: Query<(
        &mut OrthographicProjection,
        &mut Transform,
        &mut PixelPerfectPos,
        &Camera,
    )>,
    input: Res<Input<KeyCode>>,
    mut mouse_wheel: EventReader<MouseWheel>,
    time: Res<Time>,
) {
    const MOVE_SPEED: f32 = 400.;

    for (mut projection, mut transform, mut pos, camera) in &mut cameras {
        if camera.is_active {
            for e in mouse_wheel.read() {
                projection.scale = get_zoom_level(e.y, projection.scale)
            }

            let move_amount = MOVE_SPEED * time.delta_seconds() * projection.scale.sqrt();

            if input.pressed(KeyCode::W) {
                pos.0.y += move_amount;
            }
            if input.pressed(KeyCode::S) {
                pos.0.y -= move_amount;
            }
            if input.pressed(KeyCode::A) {
                pos.0.x -= move_amount;
            }
            if input.pressed(KeyCode::D) {
                pos.0.x += move_amount;
            }

            transform.translation.x = pos.0.x.round();
            transform.translation.y = pos.0.y.round();
        }
    }
}
