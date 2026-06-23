mod chemistry {
    pub struct Atom {
        pub protons: u32,
    }

    impl Atom {
        pub fn new(protons: u32) -> Atom {
            Atom { protons }
        }

        pub fn proton_count(self) -> u32 {
            self.protons
        }

        pub fn isotope(self, neutrons: u32) -> Isotope {
            Isotope {
                atom: self,
                neutrons,
            }
        }
    }

    pub struct Isotope {
        pub atom: Atom,
        pub neutrons: u32,
    }

    impl Isotope {
        pub fn mass_number(self) -> u32 {
            self.atom.protons + self.neutrons
        }
    }

    pub enum BondKind {
        Single,
        Double,
    }

    pub fn atom(protons: u32) -> Atom {
        Atom { protons }
    }

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
        if code == 1 { "chemistry" } else { "unknown" }
    }

    pub mod bonding {
        use super::{Atom, BondKind};

        pub struct Bond {
            pub left: Atom,
            pub right: Atom,
            pub kind: BondKind,
        }

        impl Bond {
            pub fn new(left: Atom, right: Atom, kind: BondKind) -> Bond {
                Bond { left, right, kind }
            }

            pub fn score(self) -> u32 {
                let base = self.left.protons + self.right.protons;
                match self.kind {
                    BondKind::Single => base,
                    BondKind::Double => base * 2,
                }
            }
        }

        pub fn make_bond(left: Atom, right: Atom, kind: BondKind) -> Bond {
            Bond { left, right, kind }
        }

        pub fn bond_score(bond: Bond) -> u32 {
            let base = bond.left.protons + bond.right.protons;
            match bond.kind {
                BondKind::Single => base,
                BondKind::Double => base * 2,
            }
        }

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
    use crate::chemistry;

    pub struct Point {
        pub x: i32,
        pub y: i32,
    }

    impl Point {
        pub fn new(x: i32, y: i32) -> Point {
            Point { x, y }
        }

        pub fn manhattan(self) -> i32 {
            self.x + self.y
        }

        pub fn translated(self, dx: i32, dy: i32) -> Point {
            Point {
                x: self.x + dx,
                y: self.y + dy,
            }
        }
    }

    pub struct Segment {
        pub start: Point,
        pub end: Point,
    }

    impl Segment {
        pub fn new(start: Point, end: Point) -> Segment {
            Segment { start, end }
        }

        pub fn horizontal_delta(self) -> i32 {
            self.end.x - self.start.x
        }
    }

    pub enum Axis {
        X,
        Y,
    }

    pub fn point(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    pub fn segment(start: Point, end: Point) -> Segment {
        Segment { start, end }
    }

    pub fn axis_value(point: Point, axis: Axis) -> i32 {
        match axis {
            Axis::X => point.x,
            Axis::Y => point.y,
        }
    }

    pub fn atom_offset(atom: chemistry::Atom, offset: i32) -> Point {
        Point {
            x: atom.protons as i32,
            y: offset,
        }
    }

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
        if code == 1 { "maths" } else { "unknown" }
    }
}

mod physics {
    pub struct Point {
        pub magnitude: i32,
    }

    impl Point {
        pub fn new(magnitude: i32) -> Point {
            Point { magnitude }
        }

        pub fn doubled(self) -> Point {
            Point {
                magnitude: self.magnitude * 2,
            }
        }

        pub fn as_math(self) -> crate::maths::Point {
            crate::maths::Point::new(self.magnitude, self.magnitude + 1)
        }
    }

    pub fn point(magnitude: i32) -> Point {
        Point { magnitude }
    }
}

struct Report {
    anchor: maths::Point,
    atom: chemistry::Atom,
    bond: chemistry::bonding::Bond,
}

fn label(code: u32) -> &'static str {
    if code == 1 { "root" } else { "unknown" }
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

    let lithium = chemistry::Atom::new(3);
    assert!(lithium.proton_count() == 3);
    assert!(chemistry::Atom::new(6).isotope(7).mass_number() == 13);

    let hydrogen = chemistry::atom(1);
    let oxygen = chemistry::atom(8);
    let water_bond = chemistry::bonding::make_bond(hydrogen, oxygen, chemistry::BondKind::Double);
    let bond_score = chemistry::bonding::bond_score(water_bond);
    assert!(bond_score == 18);

    let ionic_bond = chemistry::bonding::Bond::new(
        chemistry::Atom::new(11),
        chemistry::Atom::new(17),
        chemistry::BondKind::Single,
    );
    assert!(ionic_bond.score() == 28);

    let start = maths::Point::new(3, 4);
    assert!(maths::Point::new(5, 6).manhattan() == 11);
    let translated = maths::Point::new(1, 2).translated(5, 7);
    assert!(translated.x == 6);
    assert!(translated.y == 9);

    let end = maths::atom_offset(chemistry::atom(6), 9);
    let segment = maths::Segment::new(start, end);
    assert!(segment.start.x == 3);
    assert!(segment.end.x == 6);
    assert!(maths::Segment::new(maths::point(2, 0), maths::point(9, 0)).horizontal_delta() == 7);
    assert!(maths::axis_value(maths::point(6, 9), maths::Axis::Y) == 9);

    let physics_point = physics::Point::new(12).doubled();
    assert!(physics_point.magnitude == 24);
    let math_from_physics = physics::point(30).as_math();
    assert!(math_from_physics.x == 30);
    assert!(math_from_physics.y == 31);

    let report = Report {
        anchor: maths::point(10, 20),
        atom: chemistry::atom(7),
        bond: chemistry::bonding::make_bond(
            chemistry::atom(2),
            chemistry::atom(3),
            chemistry::BondKind::Single,
        ),
    };
    assert!(report.anchor.y == 20);
    assert!(report.atom.protons == 7);
    assert!(chemistry::bonding::bond_score(report.bond) == 5);
}
