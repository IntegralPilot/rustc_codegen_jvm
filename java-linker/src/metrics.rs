use crate::*;

pub(crate) fn json_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

#[derive(Debug)]
pub(crate) struct DuplicateClassMetric {
    pub(crate) class: String,
    pub(crate) fragments: usize,
    pub(crate) input_bytes: usize,
}

#[derive(Debug)]
pub(crate) struct LinkerMetrics {
    pub(crate) input_fragments: usize,
    pub(crate) input_fragment_bytes: usize,
    pub(crate) unique_class_names: usize,
    pub(crate) duplicate_class_names: usize,
    pub(crate) duplicate_fragments: usize,
    pub(crate) top_duplicate_classes: Vec<DuplicateClassMetric>,
    pub(crate) merged_classes: usize,
    pub(crate) merged_class_bytes: usize,
    pub(crate) library_jars: usize,
    pub(crate) library_jar_bytes: u64,
    pub(crate) output_jar_bytes: u64,
}

impl LinkerMetrics {
    pub(crate) fn enabled() -> bool {
        env::var_os("RCGJ_METRICS_DIR").is_some_and(|directory| !directory.is_empty())
    }

    #[cfg(test)]
    pub(crate) fn from_inputs(classes: &[ClassInfo], input_jar_files: &[String]) -> Self {
        Self {
            input_fragments: classes.len(),
            input_fragment_bytes: classes.iter().map(|class| class.data.len()).sum(),
            unique_class_names: 0,
            duplicate_class_names: 0,
            duplicate_fragments: 0,
            top_duplicate_classes: Vec::new(),
            merged_classes: 0,
            merged_class_bytes: 0,
            library_jars: input_jar_files.len(),
            library_jar_bytes: input_jar_files
                .iter()
                .filter_map(|path| fs::metadata(path).ok().map(|metadata| metadata.len()))
                .sum(),
            output_jar_bytes: 0,
        }
    }

    #[cfg(test)]
    pub(crate) fn record_fragment_groups(&mut self, groups: &[Vec<ClassInfo>]) {
        self.unique_class_names = groups.len();
        self.duplicate_class_names = groups.iter().filter(|group| group.len() > 1).count();
        self.duplicate_fragments = groups
            .iter()
            .map(|group| group.len().saturating_sub(1))
            .sum();
        self.top_duplicate_classes = groups
            .iter()
            .filter(|group| group.len() > 1)
            .map(|group| DuplicateClassMetric {
                class: group[0].jar_entry_name.clone(),
                fragments: group.len(),
                input_bytes: group.iter().map(|fragment| fragment.data.len()).sum(),
            })
            .collect();
        self.top_duplicate_classes
            .sort_by_key(|metric| std::cmp::Reverse((metric.fragments, metric.input_bytes)));
        self.top_duplicate_classes.truncate(24);
    }

    pub(crate) fn from_index(index: &inputs::Index, libraries: &[String]) -> Self {
        let groups = &index.groups;
        let mut duplicates = groups
            .iter()
            .filter(|g| g.fragments.len() > 1)
            .map(|g| DuplicateClassMetric {
                class: g.name.clone(),
                fragments: g.fragments.len(),
                input_bytes: g.bytes,
            })
            .collect::<Vec<_>>();
        duplicates.sort_by_key(|m| std::cmp::Reverse((m.fragments, m.input_bytes)));
        duplicates.truncate(24);
        Self {
            input_fragments: groups.iter().map(|g| g.fragments.len()).sum(),
            input_fragment_bytes: groups.iter().map(|g| g.bytes).sum(),
            unique_class_names: groups.len(),
            duplicate_class_names: groups.iter().filter(|g| g.fragments.len() > 1).count(),
            duplicate_fragments: groups.iter().map(|g| g.fragments.len() - 1).sum(),
            top_duplicate_classes: duplicates,
            merged_classes: 0,
            merged_class_bytes: 0,
            library_jars: libraries.len(),
            library_jar_bytes: libraries
                .iter()
                .filter_map(|p| fs::metadata(p).ok())
                .map(|m| m.len())
                .sum(),
            output_jar_bytes: 0,
        }
    }

    pub(crate) fn write(&self, output_jar: &str) -> io::Result<Option<PathBuf>> {
        let Some(directory) = env::var_os("RCGJ_METRICS_DIR") else {
            return Ok(None);
        };
        if directory.is_empty() {
            return Ok(None);
        }
        let directory = PathBuf::from(directory);
        fs::create_dir_all(&directory)?;
        let path = directory.join(format!("{}-linker.json", std::process::id()));
        let mut writer = BufWriter::new(fs::File::create(&path)?);
        writeln!(writer, "{{")?;
        writeln!(writer, "  \"schema_version\": 2,")?;
        writeln!(writer, "  \"kind\": \"linker_work_metrics\",")?;
        writeln!(writer, "  \"pid\": {},", std::process::id())?;
        writeln!(writer, "  \"output_jar\": {},", json_string(output_jar))?;
        writeln!(writer, "  \"input_fragments\": {},", self.input_fragments)?;
        writeln!(
            writer,
            "  \"input_fragment_bytes\": {},",
            self.input_fragment_bytes
        )?;
        writeln!(
            writer,
            "  \"unique_class_names\": {},",
            self.unique_class_names
        )?;
        writeln!(
            writer,
            "  \"duplicate_class_names\": {},",
            self.duplicate_class_names
        )?;
        writeln!(
            writer,
            "  \"duplicate_fragments\": {},",
            self.duplicate_fragments
        )?;
        writeln!(writer, "  \"merged_classes\": {},", self.merged_classes)?;
        writeln!(
            writer,
            "  \"merged_class_bytes\": {},",
            self.merged_class_bytes
        )?;
        writeln!(writer, "  \"library_jars\": {},", self.library_jars)?;
        writeln!(
            writer,
            "  \"library_jar_bytes\": {},",
            self.library_jar_bytes
        )?;
        writeln!(writer, "  \"output_jar_bytes\": {},", self.output_jar_bytes)?;
        writeln!(writer, "  \"top_duplicate_classes\": [")?;
        for (index, metric) in self.top_duplicate_classes.iter().enumerate() {
            writeln!(
                writer,
                "    {{\"class\": {}, \"fragments\": {}, \"input_bytes\": {}}}{}",
                json_string(&metric.class),
                metric.fragments,
                metric.input_bytes,
                if index + 1 == self.top_duplicate_classes.len() {
                    ""
                } else {
                    ","
                }
            )?;
        }
        writeln!(writer, "  ]")?;
        writeln!(writer, "}}")?;
        Ok(Some(path))
    }
}
