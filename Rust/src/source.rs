#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceText {
	text: String,
}

impl SourceText {
	pub fn as_str(&self) -> &str {
		&self.text
	}

	pub fn format_diagnostic(&self, category: &str, position: usize, message: &str) -> String {
		let (line, column) = self.line_and_column(position);
		let line_text = self.line_text(position);
		let gutter_width = line.to_string().len().max(1);
		let gutter_padding = " ".repeat(gutter_width);
		let caret_padding = " ".repeat(column.saturating_sub(1));

		format!(
			"{category} at line {line}, column {column}: {message}\n{gutter_padding} |\n{line:>gutter_width$} | {line_text}\n{gutter_padding} | {caret_padding}^"
		)
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

#[cfg(test)]
mod tests {
	use super::SourceText;

	#[test]
	fn formats_diagnostic_with_line_and_column() {
		let source = SourceText::new("foo\nbar\nbaz");
		let diagnostic = source.format_diagnostic("Parse error", 4, "Example message.");

		assert_eq!(
			diagnostic,
			"Parse error at line 2, column 1: Example message.\n  |\n2 | bar\n  | ^"
		);
	}
}
