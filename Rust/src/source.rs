use std::path::Path;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceText {
	text: String,
}

impl SourceText {
	pub fn as_str(&self) -> &str {
		&self.text
	}

	pub fn format_diagnostic(&self, category: &str, position: usize, message: &str) -> String {
		self.format_diagnostic_with_source_name(category, position, message, None)
	}

	pub fn format_diagnostic_with_source_name(
		&self,
		category: &str,
		position: usize,
		message: &str,
		source_name: Option<&str>,
	) -> String {
		let (line, column) = self.line_and_column(position);
		let source_name = source_name.map(short_display_name).unwrap_or_else(|| String::from("<source>"));

		format!("{category} in {source_name}:{line}:{column}: {message}")
	}

	pub fn line_and_column(&self, position: usize) -> (usize, usize) {
		let position = position.min(self.text.len());
		let mut line = 1;
		let mut line_start = 0;

		for (index, ch) in self.text.char_indices() {
			if index >= position {
				break;
			}

			if ch == '\n' {
				line += 1;
				line_start = index + ch.len_utf8();
			}
		}

		let column = self.text[line_start..position].chars().count() + 1;
		(line, column)
	}

	pub fn line_starts(&self) -> Vec<usize> {
		let mut starts = vec![0];

		for (index, ch) in self.text.char_indices() {
			if ch == '\n' {
				starts.push(index + ch.len_utf8());
			}
		}

		starts
	}

	pub fn line_text(&self, position: usize) -> &str {
		let position = position.min(self.text.len());
		let line_start = self.text[..position].rfind('\n').map_or(0, |index| index + 1);
		let line_end = self.text[position..]
			.find('\n')
			.map_or(self.text.len(), |offset| position + offset);

		self.text[line_start..line_end].strip_suffix('\r').unwrap_or(&self.text[line_start..line_end])
	}

	pub fn new(text: impl Into<String>) -> Self {
		Self {
			text: text.into(),
		}
	}

}

fn short_display_name(source_name: &str) -> String {
	let path = Path::new(source_name);

	if let Ok(current_dir) = std::env::current_dir()
		&& let Ok(relative_path) = path.strip_prefix(&current_dir) {
		let display = relative_path.display().to_string();

		if !display.is_empty() {
			return display;
		}
	}

	source_name.to_string()
}

#[cfg(test)]
mod tests {
	use super::SourceText;

	#[test]
	fn formats_diagnostic_with_line_and_column() {
		let source = SourceText::new("foo\nbar\nbaz");
		let diagnostic = source.format_diagnostic("Parse error", 4, "Example message.");

		assert_eq!(
			diagnostic,
			"Parse error in <source>:2:1: Example message."
		);
	}

	#[test]
	fn formats_diagnostic_with_source_name() {
		let source = SourceText::new("foo\nbar\nbaz");
		let diagnostic = source.format_diagnostic_with_source_name(
			"Compile error",
			4,
			"Example message.",
			Some("Examples/basic.tablo"),
		);

		assert_eq!(
			diagnostic,
			"Compile error in Examples/basic.tablo:2:1: Example message."
		);
	}
}
