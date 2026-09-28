// Installed as typst-shared's binary entry point by run.py.
pub mod export;
pub mod serial;
pub mod syntax;
pub mod terminal;
pub mod world;

pub use self::{export::*, serial::*, syntax::*, terminal::*};
pub use world::time::Now;

use typst::layout::{Frame, FrameItem};
use typst::syntax::{FileId, RootedPath, VirtualPath, VirtualRoot};
use typst::utils::{LazyHash, SmallBitSet};
use typst_library::diag::FileResult;
use world::{CompositeWorld, FilesCache, FontCollection, ReadCallback, stdlib};

const CONTENT: &str = "Hello, world!";

#[derive(Clone, Copy, Debug)]
struct Source;

impl ReadCallback for Source {
    fn read(&self, _id: FileId) -> FileResult<Vec<u8>> {
        Ok(CONTENT.as_bytes().to_vec())
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

fn main() {
    // Embedded fonts make the result independent of the runner's font catalog.
    let fonts = FontCollection::new(false, true, vec![]);
    let files = FilesCache::new(Source);
    let library = LazyHash::new(stdlib(SmallBitSet::new()));
    let world = CompositeWorld::new(
        Some(&files),
        Some(&fonts),
        Some(&library),
        Some(FileId::new(RootedPath::new(
            VirtualRoot::Project,
            VirtualPath::new("/main.typ").unwrap(),
        ))),
        Some(Now::System),
        0,
    );

    let result = export::compile::compile_paged(&world, 0, i32::MAX, |page| {
        let mut text = String::new();
        frame_text(&page.frame, &mut text);
        assert_eq!(text, CONTENT, "unexpected laid-out text");

        let pixmap = typst_render::render(page, 2.0);
        assert!(
            pixmap
                .data()
                .chunks_exact(4)
                .any(|pixel| { pixel[3] != 0 && pixel[..3] != [255, 255, 255] }),
            "rendered page is blank",
        );
        let png = pixmap.encode_png().expect("encode rendered page");
        std::fs::write("page.png", &png).expect("write rendered page");
    });
    assert!(result.warnings.is_empty(), "{:#?}", result.warnings);
    let pages = result.output.expect("compile Hello, world!");
    assert_eq!(pages.len(), 1, "expected exactly one page");
    println!("Typst Hello World passed");
}
