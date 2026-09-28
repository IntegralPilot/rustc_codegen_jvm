// Installed as typst-shared's binary entry point by run.py.
pub mod export;
pub mod serial;
pub mod syntax;
pub mod terminal;
pub mod world;

pub use self::{export::*, serial::*, syntax::*, terminal::*};
pub use world::time::Now;

use std::path::Path;
use typst::layout::{Frame, FrameItem};
use typst::syntax::{FileId, RootedPath, VirtualPath, VirtualRoot};
use typst::utils::{LazyHash, SmallBitSet};
use typst_library::diag::{FileError, FileResult};
use world::{CompositeWorld, FilesCache, FontCollection, ReadCallback, stdlib};

#[derive(Clone, Copy, Debug)]
struct Source<'a> {
    root: &'a Path,
}

impl ReadCallback for Source<'_> {
    fn read(&self, id: FileId) -> FileResult<Vec<u8>> {
        let path = self.root.join(id.vpath().as_rootless_path());
        std::fs::read(&path).map_err(|error| FileError::from_io(error, &path))
    }
}

fn frame_text(frame: &Frame, output: &mut String) {
    for (_, item) in frame.items() {
        match item {
            FrameItem::Group(group) => frame_text(&group.frame, output),
            FrameItem::Text(text) => output.push_str(&text.text),
            _ => {}
        }
    }
}

fn frame_layout(frame: &Frame) -> Vec<serde_json::Value> {
    let round = |value: f64| (value * 1000.0).round() / 1000.0;
    frame
        .items()
        .filter_map(|(pos, item)| {
            let at = [round(pos.x.to_pt()), round(pos.y.to_pt())];
            match item {
                FrameItem::Group(group) => {
                    let t = group.transform;
                    Some(serde_json::json!({
                        "at": at,
                        "transform": [round(t.sx.get()), round(t.ky.get()),
                            round(t.kx.get()), round(t.sy.get()),
                            round(t.tx.to_pt()), round(t.ty.to_pt())],
                        "items": frame_layout(&group.frame),
                    }))
                }
                FrameItem::Text(text) => Some(serde_json::json!({
                    "at": at, "text": text.text, "size": round(text.size.to_pt()),
                })),
                _ => None,
            }
        })
        .collect()
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let input = std::fs::canonicalize(&args[1]).expect("document path");
    let output = Path::new(&args[2]);
    std::fs::create_dir_all(output).expect("create output directory");
    // Embedded fonts make the result independent of the runner's font catalog.
    let fonts = FontCollection::new(false, true, vec![]);
    let files = FilesCache::new(Source {
        root: input.parent().unwrap(),
    });
    let library = LazyHash::new(stdlib(SmallBitSet::new()));
    let world = CompositeWorld::new(
        Some(&files),
        Some(&fonts),
        Some(&library),
        Some(FileId::new(RootedPath::new(
            VirtualRoot::Project,
            VirtualPath::new(input.file_name().unwrap().to_str().unwrap()).unwrap(),
        ))),
        Some(Now::Fixed {
            millis: 0,
            nanos: 0,
        }),
        0,
    );

    let index = std::cell::Cell::new(0);
    let result = export::compile::compile_paged(&world, 0, i32::MAX, |page| {
        let mut text = String::new();
        frame_text(&page.frame, &mut text);

        let pixmap = typst_render::render(page, 2.0);
        assert!(
            pixmap
                .data()
                .chunks_exact(4)
                .any(|pixel| { pixel[3] != 0 && pixel[..3] != [255, 255, 255] }),
            "rendered page is blank",
        );
        let png = pixmap.encode_png().expect("encode rendered page");
        let number = index.get() + 1;
        index.set(number);
        std::fs::write(output.join(format!("page-{number}.png")), &png)
            .expect("write rendered page");
        serde_json::json!({
            "text": text, "width": pixmap.width(), "height": pixmap.height(),
            "layout": frame_layout(&page.frame),
        })
    });
    assert!(result.warnings.is_empty(), "{:#?}", result.warnings);
    let pages = result.output.expect("compile document");
    assert!(!pages.is_empty(), "expected at least one page");
    std::fs::write(
        output.join("pages.json"),
        serde_json::to_vec_pretty(&pages).unwrap(),
    )
    .unwrap();
    println!("Typst document passed: {} page(s)", pages.len());
}
