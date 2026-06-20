use std::collections::BTreeMap;
use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use serde::Deserialize;
use serde_json::Value as JsonValue;
use serde_json::json;

use tablo::TabloError;
use tablo::local_usage_with_source_name;
use tablo::local_usage_with_source_name_and_schema;
use tablo::runtime_config::read_schema_catalog_from_runtime_config_path;
use tablo::semantic::ssa::LocalDeclarationKind;
use tablo::semantic::ssa::ProgramLocalUsage;
use tablo::source::SourceText;

fn main() {
	let stdin = io::stdin();
	let stdout = io::stdout();
	let mut reader = BufReader::new(stdin.lock());
	let mut writer = stdout.lock();
	let mut server = LspServer::new();

	if let Err(error) = server.run(&mut reader, &mut writer) {
		eprintln!("tablolsp: {error}");
		std::process::exit(1);
	}
}

#[derive(Deserialize)]
struct DidChangeTextDocumentParams {
	#[serde(rename = "contentChanges")]
	content_changes: Vec<TextDocumentContentChangeEvent>,
	#[serde(rename = "textDocument")]
	text_document: VersionedTextDocumentIdentifier,
}

#[derive(Deserialize)]
struct DidCloseTextDocumentParams {
	#[serde(rename = "textDocument")]
	text_document: TextDocumentIdentifier,
}

#[derive(Deserialize)]
struct DidOpenTextDocumentParams {
	#[serde(rename = "textDocument")]
	text_document: VersionedTextDocumentItem,
}

struct LspServer {
	open_documents: BTreeMap<String, OpenDocument>,
	shutdown_requested: bool,
}

struct OpenDocument {
	text: String,
	version: i64,
}

#[derive(Deserialize)]
struct TextDocumentContentChangeEvent {
	text: String,
}

#[derive(Deserialize)]
struct TextDocumentIdentifier {
	uri: String,
}

#[derive(Deserialize)]
struct VersionedTextDocumentIdentifier {
	uri: String,
	version: i64,
}

#[derive(Deserialize)]
struct VersionedTextDocumentItem {
	text: String,
	uri: String,
	version: i64,
}

impl LspServer {
	fn document_diagnostics(&self, uri: &str, source: &str) -> Result<Vec<JsonValue>, String> {
		let Some(file_path) = file_path_from_document_uri(uri) else {
			return Ok(Vec::new());
		};
		let file_name = file_path.display().to_string();
		let mut diagnostics = trailing_whitespace_diagnostics(source);

		let validation_result = if let Some(config_path) = discover_project_config_path(&file_path) {
			match read_schema_catalog_from_runtime_config_path(&config_path) {
				Ok(schema_catalog) => local_usage_with_source_name_and_schema(source, file_name, &schema_catalog),
				Err(error) => {
					diagnostics.push(diagnostic_json(
						uri,
						&SourceText::new(source),
						0,
						1,
						&format!(
							"Failed to load schema configuration from `{}`: {}",
							config_path.display(),
							error.message,
						),
					));
					return Ok(diagnostics);
				}
			}
		}
		else {
			local_usage_with_source_name(source, file_name)
		};

		match validation_result {
			Ok(local_usage) => {
				diagnostics.extend(unused_variable_diagnostics(uri, source, &local_usage));
				Ok(diagnostics)
			}
			Err(error) => {
				diagnostics.push(tablo_error_to_diagnostic(uri, source, error));
				Ok(diagnostics)
			}
		}
	}

	fn handle_did_change<W: Write>(&mut self, writer: &mut W, params: JsonValue) -> Result<(), String> {
		let params: DidChangeTextDocumentParams = serde_json::from_value(params)
			.map_err(|error| format!("Invalid didChange params: {error}"))?;
		let Some(last_change) = params.content_changes.last() else {
			return Ok(());
		};

		self.open_documents.insert(
			params.text_document.uri.clone(),
			OpenDocument {
				text: last_change.text.clone(),
				version: params.text_document.version,
			},
		);
		self.publish_diagnostics(writer, &params.text_document.uri)
	}

	fn handle_did_close<W: Write>(&mut self, writer: &mut W, params: JsonValue) -> Result<(), String> {
		let params: DidCloseTextDocumentParams = serde_json::from_value(params)
			.map_err(|error| format!("Invalid didClose params: {error}"))?;
		self.open_documents.remove(&params.text_document.uri);
		self.publish_empty_diagnostics(writer, &params.text_document.uri)
	}

	fn handle_did_open<W: Write>(&mut self, writer: &mut W, params: JsonValue) -> Result<(), String> {
		let params: DidOpenTextDocumentParams = serde_json::from_value(params)
			.map_err(|error| format!("Invalid didOpen params: {error}"))?;
		self.open_documents.insert(
			params.text_document.uri.clone(),
			OpenDocument {
				text: params.text_document.text,
				version: params.text_document.version,
			},
		);
		self.publish_diagnostics(writer, &params.text_document.uri)
	}

	fn handle_message<W: Write>(&mut self, writer: &mut W, message: JsonValue) -> Result<bool, String> {
		let method = message.get("method").and_then(JsonValue::as_str);
		let id = message.get("id").cloned();
		let params = message.get("params").cloned().unwrap_or(JsonValue::Null);

		match method {
			Some("initialize") => {
				if let Some(id) = id {
					write_lsp_message(writer, json!({
						"jsonrpc": "2.0",
						"id": id,
						"result": {
							"serverInfo": {
								"name": "tablolsp",
								"version": "0.1.0"
							},
							"capabilities": {
								"positionEncoding": "utf-16",
								"textDocumentSync": {
									"openClose": true,
									"change": 1,
									"save": false
								}
							}
						}
					}))?;
				}

				Ok(false)
			}
			Some("initialized") => Ok(false),
			Some("textDocument/didOpen") => {
				self.handle_did_open(writer, params)?;
				Ok(false)
			}
			Some("textDocument/didChange") => {
				self.handle_did_change(writer, params)?;
				Ok(false)
			}
			Some("textDocument/didClose") => {
				self.handle_did_close(writer, params)?;
				Ok(false)
			}
			Some("textDocument/didSave") => Ok(false),
			Some("shutdown") => {
				self.shutdown_requested = true;

				if let Some(id) = id {
					write_lsp_message(writer, json!({
						"jsonrpc": "2.0",
						"id": id,
						"result": null
					}))?;
				}

				Ok(false)
			}
			Some("exit") => {
				if !self.shutdown_requested {
					return Err(String::from("Received `exit` before `shutdown`."));
				}

				Ok(true)
			}
			Some(method_name) => {
				if let Some(id) = id {
					write_lsp_message(writer, json!({
						"jsonrpc": "2.0",
						"id": id,
						"error": {
							"code": -32601,
							"message": format!("Method `{method_name}` is not yet supported.")
						}
					}))?;
				}

				Ok(false)
			}
			None => Err(String::from("Received JSON-RPC message without a `method` field.")),
		}
	}

	fn new() -> Self {
		Self {
			open_documents: BTreeMap::new(),
			shutdown_requested: false,
		}
	}

	fn publish_diagnostics<W: Write>(&self, writer: &mut W, uri: &str) -> Result<(), String> {
		let Some(document) = self.open_documents.get(uri) else {
			return self.publish_empty_diagnostics(writer, uri);
		};
		let diagnostics = self.document_diagnostics(uri, &document.text)?;

		write_lsp_message(writer, json!({
			"jsonrpc": "2.0",
			"method": "textDocument/publishDiagnostics",
			"params": {
				"uri": uri,
				"version": document.version,
				"diagnostics": diagnostics,
			}
		}))
	}

	fn publish_empty_diagnostics<W: Write>(&self, writer: &mut W, uri: &str) -> Result<(), String> {
		write_lsp_message(writer, json!({
			"jsonrpc": "2.0",
			"method": "textDocument/publishDiagnostics",
			"params": {
				"uri": uri,
				"diagnostics": [],
			}
		}))
	}

	fn run<R: BufRead, W: Write>(&mut self, reader: &mut R, writer: &mut W) -> Result<(), String> {
		loop {
			let Some(message) = read_lsp_message(reader)? else {
				return Ok(());
			};

			let should_exit = self.handle_message(writer, message)?;
			if should_exit {
				return Ok(());
			}
		}
	}
}

fn diagnostic_json(uri: &str, source: &SourceText, position: usize, severity: u32, message: &str) -> JsonValue {
	let (line, column) = source.line_and_column(position);
	let line_index = line.saturating_sub(1) as u32;
	let column_index = column.saturating_sub(1) as u32;
	let end_column = column_index + 1;
	let _ = uri;

	json!({
		"severity": severity,
		"source": "tablolsp",
		"message": message,
		"range": {
			"start": {
				"line": line_index,
				"character": column_index,
			},
			"end": {
				"line": line_index,
				"character": end_column,
			}
		}
	})
}

fn diagnostic_json_for_byte_span(
	source: &SourceText,
	start: usize,
	end: usize,
	severity: u32,
	message: &str,
) -> JsonValue {
	let (start_line, start_column) = source.line_and_column(start);
	let (end_line, end_column) = source.line_and_column(end);

	json!({
		"severity": severity,
		"source": "tablolsp",
		"message": message,
		"range": {
			"start": {
				"line": start_line.saturating_sub(1) as u32,
				"character": start_column.saturating_sub(1) as u32,
			},
			"end": {
				"line": end_line.saturating_sub(1) as u32,
				"character": end_column.saturating_sub(1) as u32,
			}
		}
	})
}

fn diagnostic_json_for_range(
	line_index: u32,
	start_column: u32,
	end_column: u32,
	severity: u32,
	message: &str,
) -> JsonValue {
	json!({
		"severity": severity,
		"source": "tablolsp",
		"message": message,
		"range": {
			"start": {
				"line": line_index,
				"character": start_column,
			},
			"end": {
				"line": line_index,
				"character": end_column,
			}
		}
	})
}

fn discover_project_config_path(file_path: &Path) -> Option<PathBuf> {
	let mut current = file_path.parent()?;

	loop {
		let candidate = current.join("tablo.toml");
		if candidate.is_file() {
			return Some(candidate);
		}

		current = current.parent()?;
	}
}

fn file_path_from_document_uri(uri: &str) -> Option<PathBuf> {
	let path = uri.strip_prefix("file://")?;

	if cfg!(windows) {
		Some(PathBuf::from(path.trim_start_matches('/')))
	}
	else {
		Some(PathBuf::from(path))
	}
}

fn is_identifier_char(ch: char) -> bool {
	ch.is_ascii_alphanumeric() || ch == '_'
}

fn is_numeric_literal_char(ch: char) -> bool {
	ch.is_ascii_digit() || ch == '.'
}

fn is_temporal_literal_char(ch: char) -> bool {
	ch.is_ascii_alphanumeric() || matches!(ch, ':' | '-' | '+' | '.' | 'T' | 'Z')
}

fn local_name_span(source: &SourceText, declaration_position: usize, name: &str) -> Option<(usize, usize)> {
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

fn parse_content_length(header_line: &str) -> Result<Option<usize>, String> {
	let Some((name, value)) = header_line.split_once(':') else {
		return Ok(None);
	};

	if !name.eq_ignore_ascii_case("Content-Length") {
		return Ok(None);
	}

	let length = value.trim().parse::<usize>()
		.map_err(|error| format!("Invalid Content-Length header `{header_line}`: {error}"))?;
	Ok(Some(length))
}

fn position_from_line_and_column(source: &str, line: usize, column: usize) -> usize {
	let mut current_line = 1;
	let mut current_column = 1;

	for (index, ch) in source.char_indices() {
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

	source.len()
}

fn read_lsp_message<R: BufRead>(reader: &mut R) -> Result<Option<JsonValue>, String> {
	let mut content_length: Option<usize> = None;

	loop {
		let mut header_line = String::new();
		let bytes_read = reader.read_line(&mut header_line)
			.map_err(|error| format!("Failed to read LSP header: {error}"))?;

		if bytes_read == 0 {
			if content_length.is_none() {
				return Ok(None);
			}

			return Err(String::from("Unexpected end of input while reading LSP headers."));
		}

		if header_line == "\r\n" {
			break;
		}

		if let Some(length) = parse_content_length(header_line.trim_end_matches(['\r', '\n']))? {
			content_length = Some(length);
		}
	}

	let content_length = content_length.ok_or(String::from("Missing Content-Length header in LSP message."))?;
	let mut content = vec![0_u8; content_length];
	reader.read_exact(&mut content)
		.map_err(|error| format!("Failed to read LSP message body: {error}"))?;

	let message = serde_json::from_slice::<JsonValue>(&content)
		.map_err(|error| format!("Failed to parse LSP message body as JSON: {error}"))?;
	Ok(Some(message))
}

fn source_token_span_at_position(source: &SourceText, position: usize) -> Option<(usize, usize)> {
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

fn tablo_error_to_diagnostic(uri: &str, source: &str, error: TabloError) -> JsonValue {
	let source = SourceText::new(source);
	let (position, message) = match error {
		TabloError::Compile(error) => (error.position, error.message),
		TabloError::Lex(error) => (error.position, error.message),
		TabloError::Parse(error) => (error.position, error.message),
		TabloError::ObjectFile(error) => (0, error.message),
		TabloError::Runtime(error) => (
			error.source_location
				.map(|location| {
					let (line, column) = (location.line() as usize, location.column() as usize);
					position_from_line_and_column(source.as_str(), line, column)
				})
				.unwrap_or(0),
			error.message,
		),
	};

	let _ = uri;

	if let Some((start, end)) = source_token_span_at_position(&source, position) {
		diagnostic_json_for_byte_span(&source, start, end, 1, &message)
	}
	else {
		diagnostic_json(uri, &source, position, 1, &message)
	}
}

fn trailing_whitespace_diagnostics(source: &str) -> Vec<JsonValue> {
	let mut diagnostics = Vec::new();

	for (line_index, line) in source.split('\n').enumerate() {
		let visible_line = line.strip_suffix('\r').unwrap_or(line);
		let trimmed = visible_line.trim_end_matches([' ', '\t']);

		if trimmed.len() == visible_line.len() {
			continue;
		}

		let start_column = trimmed.chars().count() as u32;
		let end_column = visible_line.chars().count() as u32;
		diagnostics.push(diagnostic_json_for_range(
			line_index as u32,
			start_column,
			end_column,
			2,
			"Line has trailing whitespace.",
		));
	}

	diagnostics
}

fn unused_variable_diagnostics(uri: &str, source: &str, local_usage: &ProgramLocalUsage) -> Vec<JsonValue> {
	let mut diagnostics = Vec::new();
	let source = SourceText::new(source);

	for function in &local_usage.functions {
		for local in &function.locals {
			let is_plain_local = matches!(
				local.declaration.kind,
				LocalDeclarationKind::Variable | LocalDeclarationKind::RecordPointerVariable
			);

			if !is_plain_local || !local.is_never_read() {
				continue;
			}

			let message = if local.has_writes_after_declaration() {
				format!("Local variable `{}` is assigned to but never read.", local.declaration.name)
			}
			else {
				format!("Local variable `{}` is never read.", local.declaration.name)
			};
			let _ = uri;

			if let Some((start, end)) = local_name_span(&source, local.declaration.position, &local.declaration.name) {
				diagnostics.push(diagnostic_json_for_byte_span(
					&source,
					start,
					end,
					2,
					&message,
				));
			}
			else {
				diagnostics.push(diagnostic_json(
					uri,
					&source,
					local.declaration.position,
					2,
					&message,
				));
			}
		}
	}

	diagnostics
}

fn write_lsp_message<W: Write>(writer: &mut W, message: JsonValue) -> Result<(), String> {
	let body = serde_json::to_vec(&message)
		.map_err(|error| format!("Failed to serialize LSP message: {error}"))?;
	let header = format!("Content-Length: {}\r\n\r\n", body.len());

	writer.write_all(header.as_bytes())
		.map_err(|error| format!("Failed to write LSP header: {error}"))?;
	writer.write_all(&body)
		.map_err(|error| format!("Failed to write LSP body: {error}"))?;
	writer.flush()
		.map_err(|error| format!("Failed to flush LSP output: {error}"))
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;
	use std::time::SystemTime;
	use std::time::UNIX_EPOCH;

	use serde_json::json;

	use super::LspServer;
	use super::discover_project_config_path;
	use super::file_path_from_document_uri;
	use super::local_name_span;
	use super::parse_content_length;
	use super::position_from_line_and_column;
	use super::source_token_span_at_position;
	use super::tablo_error_to_diagnostic;
	use tablo::compiler::CompileError;
	use tablo::source::SourceText;
	use tablo::TabloError;

	fn unique_temp_directory(name: &str) -> PathBuf {
		let nanos = SystemTime::now()
			.duration_since(UNIX_EPOCH)
			.unwrap()
			.as_nanos();
		std::env::temp_dir().join(format!("{name}_{nanos}"))
	}

	#[test]
	fn discovers_project_config_in_parent_directory() {
		let temp_dir = unique_temp_directory("tablolsp_project_config");
		let source_dir = temp_dir.join("src");
		let config_path = temp_dir.join("tablo.toml");
		let source_path = source_dir.join("main.tablo");
		std::fs::create_dir_all(&source_dir).unwrap();
		std::fs::write(&config_path, "[databases]\n").unwrap();
		std::fs::write(&source_path, "fn Main(args: [text]) int { return 0; }").unwrap();

		assert_eq!(discover_project_config_path(&source_path), Some(config_path.clone()));

		let _ = std::fs::remove_file(&source_path);
		let _ = std::fs::remove_file(&config_path);
		let _ = std::fs::remove_dir(&source_dir);
		let _ = std::fs::remove_dir(&temp_dir);
	}

	#[test]
	fn does_not_require_main_for_module_file_diagnostics() {
		let mut server = LspServer::new();
		let mut output = Vec::new();

		server.handle_did_open(
			&mut output,
			json!({
				"textDocument": {
					"uri": "file:///tmp/example.tablo",
					"version": 1,
					"text": "fn Helper() int { return 1; }"
				}
			}),
		).unwrap();

		let output = String::from_utf8(output).unwrap();
		assert!(!output.contains("Standalone Tablo programs must define `fn Main(args: [text]) int`."));
		assert!(output.contains("\"diagnostics\":[]"));
	}

	#[test]
	fn highlights_full_span_for_assignment_type_error_on_string_literal() {
		let source = "fn Main(args: [text]) int { var x: int = 'abc'; return 0; }";
		let diagnostic = tablo_error_to_diagnostic(
			"file:///tmp/example.tablo",
			source,
			TabloError::Compile(CompileError {
				message: String::from("Cannot assign a value of type `text` to a variable of type `int`."),
				position: 41,
			}),
		);
		let diagnostic = diagnostic.to_string();

		assert!(diagnostic.contains("Cannot assign a value of type `text` to a variable of type `int`."));
		assert!(diagnostic.contains("\"character\":41"));
		assert!(diagnostic.contains("\"character\":46"));
	}

	#[test]
	fn highlights_full_span_for_unknown_table_field_diagnostic() {
		let source = "with ExampleDb;\nfn Main(args: [text]) int { count Customers where MissingField == 1; return 0; }";
		let diagnostic = tablo_error_to_diagnostic(
			"file:///tmp/example.tablo",
			source,
			TabloError::Compile(CompileError {
				message: String::from("Field `MissingField` does not exist on table `Customers`."),
				position: 66,
			}),
		);
		let diagnostic = diagnostic.to_string();

		assert!(diagnostic.contains("Field `MissingField` does not exist on table `Customers`."));
		assert!(diagnostic.contains("\"character\":50"));
		assert!(diagnostic.contains("\"character\":62"));
	}

	#[test]
	fn ignores_non_content_length_header() {
		assert_eq!(parse_content_length("Content-Type: application/vscode-jsonrpc; charset=utf-8").unwrap(), None);
	}

	#[test]
	fn locates_identifier_token_span_at_position() {
		let source = SourceText::new("fn Main(args: [text]) int { fooBar; }");

		assert_eq!(source_token_span_at_position(&source, 28), Some((28, 34)));
	}

	#[test]
	fn locates_numeric_literal_token_span_at_position() {
		let source = SourceText::new("fn Main(args: [text]) int { 12.34; }");

		assert_eq!(source_token_span_at_position(&source, 28), Some((28, 33)));
	}

	#[test]
	fn locates_quoted_identifier_token_span_at_position() {
		let source = SourceText::new("fn Main(args: [text]) int { \"userid#\"; }");

		assert_eq!(source_token_span_at_position(&source, 28), Some((28, 37)));
	}

	#[test]
	fn locates_quoted_unused_variable_name_span() {
		let source = SourceText::new("fn Main(args: [text]) int { var \"userid#\": int = 1; return 0; }");

		assert_eq!(local_name_span(&source, 28, "userid#"), Some((32, 41)));
	}

	#[test]
	fn locates_string_literal_token_span_at_position() {
		let source = SourceText::new("fn Main(args: [text]) int { 'abc'; }");

		assert_eq!(source_token_span_at_position(&source, 28), Some((28, 33)));
	}

	#[test]
	fn locates_unused_variable_name_span() {
		let source = SourceText::new("fn Main(args: [text]) int { var x: int = 1; return 0; }");

		assert_eq!(local_name_span(&source, 28, "x"), Some((32, 33)));
	}

	#[test]
	fn parses_content_length_header() {
		assert_eq!(parse_content_length("Content-Length: 123").unwrap(), Some(123));
	}

	#[test]
	fn parses_file_uri_to_path() {
		assert_eq!(
			file_path_from_document_uri("file:///tmp/example.tablo"),
			Some(PathBuf::from("/tmp/example.tablo")),
		);
	}

	#[test]
	fn publishes_assigned_but_never_read_variable_lint() {
		let mut server = LspServer::new();
		let mut output = Vec::new();

		server.handle_did_open(
			&mut output,
			json!({
				"textDocument": {
					"uri": "file:///tmp/example.tablo",
					"version": 1,
					"text": "fn Main(args: [text]) int { var x: int = 1; x = 2; return 0; }"
				}
			}),
		).unwrap();

		let output = String::from_utf8(output).unwrap();
		assert!(output.contains("Local variable `x` is assigned to but never read."));
		assert!(output.contains("\"severity\":2"));
	}

	#[test]
	fn publishes_diagnostic_for_parse_error_on_open() {
		let mut server = LspServer::new();
		let mut output = Vec::new();

		server.handle_did_open(
			&mut output,
			json!({
				"textDocument": {
					"uri": "file:///tmp/example.tablo",
					"version": 1,
					"text": "fn Main(args: [text]) int { return ; }"
				}
			}),
		).unwrap();

		let output = String::from_utf8(output).unwrap();
		assert!(output.contains("textDocument/publishDiagnostics"));
		assert!(output.contains("\"diagnostics\":[{"));
	}

	#[test]
	fn publishes_lint_and_compile_diagnostic_together() {
		let mut server = LspServer::new();
		let mut output = Vec::new();

		server.handle_did_open(
			&mut output,
			json!({
				"textDocument": {
					"uri": "file:///tmp/example.tablo",
					"version": 1,
					"text": "fn Main(args: [text]) int { return ; }  \n"
				}
			}),
		).unwrap();

		let output = String::from_utf8(output).unwrap();
		assert!(output.contains("Line has trailing whitespace."));
		assert!(output.contains("\"severity\":2"));
		assert!(output.contains("\"severity\":1"));
	}

	#[test]
	fn publishes_trailing_whitespace_lint() {
		let mut server = LspServer::new();
		let mut output = Vec::new();

		server.handle_did_open(
			&mut output,
			json!({
				"textDocument": {
					"uri": "file:///tmp/example.tablo",
					"version": 1,
					"text": "fn Main(args: [text]) int { return 0; }  \n"
				}
			}),
		).unwrap();

		let output = String::from_utf8(output).unwrap();
		assert!(output.contains("Line has trailing whitespace."));
		assert!(output.contains("\"severity\":2"));
	}

	#[test]
	fn publishes_unused_variable_lint() {
		let mut server = LspServer::new();
		let mut output = Vec::new();

		server.handle_did_open(
			&mut output,
			json!({
				"textDocument": {
					"uri": "file:///tmp/example.tablo",
					"version": 1,
					"text": "fn Main(args: [text]) int { var x: int = 1; return 0; }"
				}
			}),
		).unwrap();

		let output = String::from_utf8(output).unwrap();
		assert!(output.contains("Local variable `x` is never read."));
		assert!(output.contains("\"severity\":2"));
		assert!(output.contains("\"character\":32"));
		assert!(output.contains("\"character\":33"));
	}

	#[test]
	fn rejects_invalid_content_length_header() {
		let error = parse_content_length("Content-Length: nope").unwrap_err();

		assert!(error.contains("Invalid Content-Length header"));
	}

	#[test]
	fn resolves_position_from_line_and_column() {
		assert_eq!(position_from_line_and_column("a\nbc\n", 2, 2), 3);
	}
}
