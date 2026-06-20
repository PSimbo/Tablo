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

pub fn local_name_span(source: &SourceText, declaration_position: usize, name: &str) -> Option<(usize, usize)> {
	let text = source.as_str();
	let declaration_position = declaration_position.min(text.len());
	let line_end = text[declaration_position..]
		.find('\n')
		.map_or(text.len(), |offset| declaration_position + offset);
	let line_text = &text[declaration_position..line_end];
	let mut search_start = 0;

	while search_start <= line_text.len() {
		let Some(found_offset) = line_text[search_start..].find(name) else {
			return None;
		};
		let mut start = declaration_position + search_start + found_offset;
		let mut end = start + name.len();
		let quoted_start = start.checked_sub(1);
		let quoted_end = text.get(end..).and_then(|suffix| suffix.chars().next());

		if quoted_start.and_then(|index| text.get(index..start)) == Some("\"")
			&& quoted_end == Some('"') {
			start -= 1;
			end += 1;
		}

		let previous = text[..start].chars().next_back();
		let next = text[end..].chars().next();
		let is_identifier_match = !previous.is_some_and(is_identifier_char)
			&& !next.is_some_and(is_identifier_char);

		if is_identifier_match {
			return Some((start, end));
		}

		search_start += found_offset + name.len();
	}

	None
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

pub fn source_offset_for_position(text: &str, line: u32, character: u32) -> Option<usize> {
	let source = SourceText::new(text);
	source.offset_from_lsp_position(line, character)
}

pub fn token_span_at_position(source: &SourceText, position: usize) -> Option<(usize, usize)> {
	let text = source.as_str();
	let position = position.min(text.len());
	let current = text.get(position..)?.chars().next()?;
	let line_end = text[position..]
		.find('\n')
		.map_or(text.len(), |offset| position + offset);

	if current == '"' {
		let closing_quote_offset = text[position + 1..line_end].find('"')?;
		let end = position + 1 + closing_quote_offset + 1;
		return Some((position, end));
	}

	if current == '\'' {
		let closing_quote_offset = text[position + 1..line_end].find('\'')?;
		let end = position + 1 + closing_quote_offset + 1;
		return Some((position, end));
	}

	if current == '@' {
		let token_start = position + current.len_utf8();
		let mut end = token_start;

		for (offset, ch) in text[token_start..line_end].char_indices() {
			if !is_temporal_literal_char(ch) {
				break;
			}

			end = token_start + offset + ch.len_utf8();
		}

		return Some((position, end));
	}

	if current == '-'
		&& text[position + current.len_utf8()..].chars().next().is_some_and(|ch| ch.is_ascii_digit()) {
		let token_start = position + current.len_utf8();
		let mut end = token_start;

		for (offset, ch) in text[token_start..line_end].char_indices() {
			if !is_numeric_literal_char(ch) {
				break;
			}

			end = token_start + offset + ch.len_utf8();
		}

		return Some((position, end));
	}

	if is_numeric_literal_char(current) {
		let mut end = position;

		for (offset, ch) in text[position..line_end].char_indices() {
			if !is_numeric_literal_char(ch) {
				break;
			}

			end = position + offset + ch.len_utf8();
		}

		return Some((position, end));
	}

	if is_identifier_char(current) {
		let mut end = position;

		for (offset, ch) in text[position..line_end].char_indices() {
			if !is_identifier_char(ch) {
				break;
			}

			end = position + offset + ch.len_utf8();
		}

		return Some((position, end));
	}

	None
}

fn is_numeric_literal_char(ch: char) -> bool {
	ch.is_ascii_digit() || ch == '.'
}

fn is_temporal_literal_char(ch: char) -> bool {
	ch.is_ascii_alphanumeric() || matches!(ch, ':' | '-' | '+' | '.' | 'T' | 'Z')
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
	use super::local_name_span;
	use super::read_identifier;
	use super::skip_comment;
	use super::skip_string_literal;
	use super::skip_whitespace;
	use super::source_offset_for_position;
	use super::token_span_at_position;

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
	fn finds_local_name_span_for_double_quoted_identifier() {
		let source = SourceText::new("var \"field-name\": int = 1;");
		assert_eq!(local_name_span(&source, 0, "field-name"), Some((4, 16)));
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
	fn resolves_source_offset_for_position_helper() {
		assert_eq!(source_offset_for_position("a\nbc\n", 1, 1), Some(3));
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

	#[test]
	fn token_span_at_position_covers_temporal_literal() {
		let source = SourceText::new("value = @2026-06-20;");
		assert_eq!(token_span_at_position(&source, 8), Some((8, 19)));
	}
}
