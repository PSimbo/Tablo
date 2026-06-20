use std::path::Path;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedIdentifier {
	pub end: usize,
	pub quoted: bool,
	pub value: String,
}

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

	pub fn offset_from_line_and_column(&self, line: usize, column: usize) -> usize {
		let mut current_line = 1;
		let mut current_column = 1;

		for (index, ch) in self.text.char_indices() {
			if current_line == line && current_column == column {
				return index;
			}

			if ch == '\n' {
				current_line += 1;
				current_column = 1;
			}
			else {
				current_column += 1;
			}
		}

		self.text.len()
	}

	pub fn offset_from_lsp_position(&self, line: u32, character: u32) -> Option<usize> {
		let line_starts = self.line_starts();
		let line_start = *line_starts.get(line as usize)?;
		let line_text = self.line_text(line_start);
		let mut offset = line_start;
		let mut remaining = character as usize;

		for ch in line_text.chars() {
			if remaining == 0 {
				break;
			}
			offset += ch.len_utf8();
			remaining -= 1;
		}

		Some(offset)
	}

}

pub fn find_matching_paren(text: &str, open_paren_index: usize) -> Option<usize> {
	let mut depth = 0;
	let mut index = open_paren_index;

	while index < text.len() {
		if let Some(comment_end) = skip_comment(text, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(text, index) {
			index = string_end;
			continue;
		}

		let ch = text[index..].chars().next()?;
		if ch == '(' {
			depth += 1;
		}
		else if ch == ')' {
			depth -= 1;
			if depth == 0 {
				return Some(index);
			}
		}

		index += ch.len_utf8();
	}

	None
}

pub fn is_identifier_char(ch: char) -> bool {
	ch.is_ascii_alphanumeric() || ch == '_'
}

pub fn read_identifier(text: &str, start: usize) -> Option<ParsedIdentifier> {
	let ch = text[start..].chars().next()?;
	if ch == '"' {
		let mut value = String::new();
		let mut index = start + 1;

		while index < text.len() {
			let current = text[index..].chars().next()?;
			if current == '"' {
				let next_index = index + 1;
				if text[next_index..].starts_with('"') {
					value.push('"');
					index = next_index + 1;
					continue;
				}

				return Some(ParsedIdentifier {
					end: next_index,
					quoted: true,
					value,
				});
			}

			value.push(current);
			index += current.len_utf8();
		}

		return None;
	}

	if !(ch.is_ascii_alphabetic() || ch == '_') {
		return None;
	}

	let mut value = String::new();
	value.push(ch);
	let mut index = start + ch.len_utf8();

	while index < text.len() {
		let current = text[index..].chars().next()?;
		if !is_identifier_char(current) {
			break;
		}

		value.push(current);
		index += current.len_utf8();
	}

	Some(ParsedIdentifier {
		end: index,
		quoted: false,
		value,
	})
}

pub fn skip_comment(text: &str, start: usize) -> Option<usize> {
	if text[start..].starts_with("//") {
		let end = text[start..].find('\n').map(|offset| start + offset + 1).unwrap_or(text.len());
		return Some(end);
	}

	if text[start..].starts_with("/*") {
		let end = text[start + 2..].find("*/").map(|offset| start + 2 + offset + 2).unwrap_or(text.len());
		return Some(end);
	}

	None
}

pub fn skip_string_literal(text: &str, start: usize) -> Option<usize> {
	if !text[start..].starts_with('\'') {
		return None;
	}

	let mut index = start + 1;
	while index < text.len() {
		let ch = text[index..].chars().next()?;
		index += ch.len_utf8();
		if ch == '\'' {
			return Some(index);
		}
	}

	Some(text.len())
}

pub fn skip_whitespace(text: &str, start: usize) -> usize {
	let mut index = start;
	while index < text.len() {
		let Some(ch) = text[index..].chars().next() else {
			break;
		};
		if !ch.is_whitespace() {
			break;
		}
		index += ch.len_utf8();
	}
	index
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
	use super::find_matching_paren;
	use super::read_identifier;
	use super::skip_comment;
	use super::skip_string_literal;
	use super::skip_whitespace;

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

	#[test]
	fn finds_matching_paren_while_skipping_comments_and_strings() {
		let text = "call(/* ) */ foo(')'))";
		assert_eq!(find_matching_paren(text, 4), Some(text.len() - 1));
	}

	#[test]
	fn reads_quoted_identifier() {
		assert_eq!(
			read_identifier("\"user\"\"id#\"", 0),
			Some(super::ParsedIdentifier {
				end: 11,
				quoted: true,
				value: String::from("user\"id#"),
			}),
		);
	}

	#[test]
	fn reads_unquoted_identifier() {
		assert_eq!(
			read_identifier("foo_bar9", 0),
			Some(super::ParsedIdentifier {
				end: 8,
				quoted: false,
				value: String::from("foo_bar9"),
			}),
		);
	}

	#[test]
	fn resolves_offset_from_line_and_column() {
		let source = SourceText::new("a\nbc\n");
		assert_eq!(source.offset_from_line_and_column(2, 2), 3);
	}

	#[test]
	fn resolves_offset_from_lsp_position() {
		let source = SourceText::new("a\nbc\n");
		assert_eq!(source.offset_from_lsp_position(1, 1), Some(3));
	}

	#[test]
	fn skips_comment_text() {
		assert_eq!(skip_comment("// comment\nnext", 0), Some(11));
	}

	#[test]
	fn skips_string_literal_text() {
		assert_eq!(skip_string_literal("'abc'", 0), Some(5));
	}

	#[test]
	fn skips_whitespace_text() {
		assert_eq!(skip_whitespace(" \t\nabc", 0), 3);
	}
}
