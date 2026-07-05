use serde::Serialize;
use std::{
    env,
    fs::OpenOptions,
    io::Write,
    path::PathBuf,
    sync::OnceLock,
    time::{Duration, Instant},
};

const PATH_ENV: &str = "RCGJ_INSTRUMENT_PATH";
const TEST_ENV: &str = "RCGJ_INSTRUMENT_TEST";
const MODE_ENV: &str = "RCGJ_INSTRUMENT_MODE";

#[derive(Debug)]
struct Recorder {
    path: PathBuf,
    test: Option<String>,
    mode: Option<String>,
}

#[derive(Serialize)]
struct TimingEvent<'a> {
    schema_version: u8,
    kind: &'a str,
    stage: &'a str,
    crate_name: Option<&'a str>,
    item: Option<&'a str>,
    seconds: f64,
    millis: f64,
    pid: u32,
    test: Option<&'a str>,
    mode: Option<&'a str>,
}

static RECORDER: OnceLock<Option<Recorder>> = OnceLock::new();

pub(crate) struct Timer {
    kind: &'static str,
    stage: &'static str,
    crate_name: Option<String>,
    item: Option<String>,
    start: Instant,
}

impl Timer {
    pub(crate) fn phase(stage: &'static str, crate_name: Option<&str>) -> Self {
        Self::new("phase", stage, crate_name, None)
    }

    pub(crate) fn function(stage: &'static str, crate_name: Option<&str>, item: &str) -> Self {
        Self::new("function", stage, crate_name, Some(item))
    }

    fn new(
        kind: &'static str,
        stage: &'static str,
        crate_name: Option<&str>,
        item: Option<&str>,
    ) -> Self {
        Self {
            kind,
            stage,
            crate_name: crate_name.map(ToOwned::to_owned),
            item: item.map(ToOwned::to_owned),
            start: Instant::now(),
        }
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        record_duration(
            self.kind,
            self.stage,
            self.crate_name.as_deref(),
            self.item.as_deref(),
            self.start.elapsed(),
        );
    }
}

fn recorder() -> Option<&'static Recorder> {
    RECORDER
        .get_or_init(|| {
            let path = env::var_os(PATH_ENV)?;
            if path.as_os_str().is_empty() {
                return None;
            }
            Some(Recorder {
                path: PathBuf::from(path),
                test: env::var(TEST_ENV).ok(),
                mode: env::var(MODE_ENV).ok(),
            })
        })
        .as_ref()
}

pub(crate) fn record_duration(
    kind: &'static str,
    stage: &'static str,
    crate_name: Option<&str>,
    item: Option<&str>,
    duration: Duration,
) {
    let Some(recorder) = recorder() else {
        return;
    };

    if let Some(parent) = recorder.path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    let event = TimingEvent {
        schema_version: 1,
        kind,
        stage,
        crate_name,
        item,
        seconds: duration.as_secs_f64(),
        millis: duration.as_secs_f64() * 1000.0,
        pid: std::process::id(),
        test: recorder.test.as_deref(),
        mode: recorder.mode.as_deref(),
    };

    let Ok(mut line) = serde_json::to_string(&event) else {
        return;
    };
    line.push('\n');

    if let Ok(mut file) = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&recorder.path)
    {
        let _ = file.write_all(line.as_bytes());
    }
}
