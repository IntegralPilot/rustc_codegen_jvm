pub trait BatchObserver {
    fn accept(&mut self, processed: u32) -> bool;
}

pub enum PipelineResult {
    Success(u32),
    Rejected(i32),
}

pub fn process_batch(
    batch_size: u32,
    transform: &dyn Fn(u32) -> u32,
    observer: &mut dyn BatchObserver,
) -> PipelineResult {
    let processed = transform(batch_size);
    if observer.accept(processed) {
        PipelineResult::Success(processed)
    } else {
        PipelineResult::Rejected(-1)
    }
}

pub async fn confirm_batch(result: PipelineResult) -> PipelineResult {
    result
}
