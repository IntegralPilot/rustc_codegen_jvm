mod chemistry {
    pub fn shape(electron_domains: u32, molecular_domains: u32) -> &'static str {
        if electron_domains == 2 && molecular_domains == 2 {
            "linear"
        } else if electron_domains == 3 && molecular_domains == 3 {
            "trigonal planar"
        } else if electron_domains == 3 && molecular_domains == 2 {
            "bent"
        } else if electron_domains == 4 && molecular_domains == 4 {
            "tetrahedral"
        } else if electron_domains == 4 && molecular_domains == 3 {
            "trigonal pyramidal"
        } else if electron_domains == 4 && molecular_domains == 2 {
            "bent"
        } else {
            "unknown"
        }
    }

    pub fn label(code: u32) -> &'static str {
        if code == 1 {
            "chemistry"
        } else {
            "unknown"
        }
    }

    pub mod bonding {
        pub fn family(order: u32) -> &'static str {
            if order == 1 {
                "single"
            } else if order == 2 {
                "double"
            } else if order == 3 {
                "triple"
            } else {
                "unknown"
            }
        }
    }
}

mod maths {
    pub fn shape(sides: u32) -> &'static str {
        if sides == 3 {
            "triangle"
        } else if sides == 4 {
            "quadrilateral"
        } else if sides == 5 {
            "pentagon"
        } else if sides == 6 {
            "hexagon"
        } else if sides == 7 {
            "heptagon"
        } else if sides == 8 {
            "octagon"
        } else {
            "unknown"
        }
    }

    pub fn label(code: u32) -> &'static str {
        if code == 1 {
            "maths"
        } else {
            "unknown"
        }
    }
}

fn label(code: u32) -> &'static str {
    if code == 1 {
        "root"
    } else {
        "unknown"
    }
}

fn main() {
    let chem_shape = chemistry::shape(4, 3);
    assert!(chem_shape == "trigonal pyramidal");

    let maths_shape = maths::shape(5);
    assert!(maths_shape == "pentagon");

    let root_label = label(1);
    assert!(root_label == "root");

    let chem_label = chemistry::label(1);
    assert!(chem_label == "chemistry");

    let maths_label = maths::label(1);
    assert!(maths_label == "maths");

    let bond_family = chemistry::bonding::family(2);
    assert!(bond_family == "double");
}
