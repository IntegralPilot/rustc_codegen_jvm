pub enum TrafficLight {
    Red,
    Yellow,
    Green,
}

pub fn get_light_action(light: TrafficLight) -> &'static str {
    match light {
        TrafficLight::Red => "Stop",
        TrafficLight::Yellow => "Caution",
        TrafficLight::Green => "Go",
    }
}

pub fn main() {
    assert!(get_light_action(TrafficLight::Red) == "Stop");
    assert!(get_light_action(TrafficLight::Yellow) == "Caution");
    assert!(get_light_action(TrafficLight::Green) == "Go");
}