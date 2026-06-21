use std::collections::BTreeMap;
use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;

use serde::Deserialize;
use serde_json::Value as JsonValue;
use serde_json::json;

use tablo::completion::CompletionItem;
use tablo::completion::CompletionItemKind;
use tablo::completion::collect_document_completion_items;
use tablo::completion::dedupe_completion_items;
use tablo::completion::default_completion_items;
use tablo::completion::member_completion_items;
use tablo::diagnostics::Diagnostic;
use tablo::diagnostics::diagnostic_at_start;
use tablo::diagnostics::diagnostic_for_tablo_error;
use tablo::diagnostics::trailing_whitespace_diagnostics;
use tablo::diagnostics::unused_variable_diagnostics;
use tablo::discover_project_config_path;
use tablo::local_usage_with_source_name;
use tablo::local_usage_with_source_name_and_schema;
use tablo::runtime_config::read_schema_catalog_from_runtime_config_path;
use tablo::source::SourceText;
use tablo::source::source_offset_for_position;
use tablo::utils::file_path_from_document_uri;

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
struct CompletionParams {
	position: Position,
	#[serde(rename = "textDocument")]
	text_document: TextDocumentIdentifier,
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

#[derive(Clone, Copy, Deserialize)]
struct Position {
	character: u32,
	line: u32,
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
	fn completion_items(&self, uri: &str, position: Position) -> Vec<JsonValue> {
		let prefix = self.completion_prefix(uri, position);
		let mut items = self.member_completion_items(uri, position).unwrap_or_else(|| {
			let mut items = default_completion_items();
			items.extend(self.document_completion_items(uri, position));
			items
		});
		dedupe_completion_items(&mut items);
		items.into_iter()
			.filter(|item| prefix.as_ref().is_none_or(|prefix| item.label.starts_with(prefix)))
			.map(completion_item_to_json)
			.collect()
	}

	fn completion_prefix(&self, uri: &str, position: Position) -> Option<String> {
		let document = self.open_documents.get(uri)?;
		let source = SourceText::new(&document.text);
		let line_starts = source.line_starts();
		let line_index = position.line as usize;
		let line_start = *line_starts.get(line_index)?;
		let line_text = source.line_text(line_start);
		let character_index = position.character as usize;
		let prefix_end = character_index.min(line_text.chars().count());
		let prefix_text = line_text.chars().take(prefix_end).collect::<String>();
		let suffix = prefix_text
			.rsplit(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '_'))
			.next()
			.unwrap_or("");

		if suffix.is_empty() {
			None
		}
		else {
			Some(suffix.to_string())
		}
	}

	fn document_completion_items(&self, uri: &str, position: Position) -> Vec<CompletionItem> {
		let Some(document) = self.open_documents.get(uri) else {
			return Vec::new();
		};
		let Some(cursor_offset) = source_offset_for_position(&document.text, position.line, position.character) else {
			return Vec::new();
		};
		let visible_text = &document.text[..cursor_offset];
		collect_document_completion_items(visible_text)
	}

	fn document_diagnostics(&self, uri: &str, source: &str) -> Result<Vec<Diagnostic>, String> {
		let Some(file_path) = file_path_from_document_uri(uri) else {
			return Ok(Vec::new());
		};
		let file_name = file_path.display().to_string();
		let mut diagnostics = trailing_whitespace_diagnostics(source);

		let validation_result = if let Some(config_path) = discover_project_config_path(&file_path) {
			match read_schema_catalog_from_runtime_config_path(&config_path) {
				Ok(schema_catalog) => local_usage_with_source_name_and_schema(source, file_name, &schema_catalog),
				Err(error) => {
					diagnostics.push(diagnostic_at_start(
						source,
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
				diagnostics.extend(unused_variable_diagnostics(source, &local_usage));
				Ok(diagnostics)
			}
			Err(error) => {
				diagnostics.push(diagnostic_for_tablo_error(source, error));
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
					write_lsp_message(writer, &json!({
						"jsonrpc": "2.0",
						"id": id,
						"result": {
							"serverInfo": {
								"name": "tablolsp",
								"version": "0.1.0"
							},
							"capabilities": {
								"positionEncoding": "utf-16",
								"completionProvider": {
									"resolveProvider": false,
									"triggerCharacters": [".", "_"]
								},
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
			Some("textDocument/completion") => {
				let params: CompletionParams = serde_json::from_value(params)
					.map_err(|error| format!("Invalid completion params: {error}"))?;
				let items = self.completion_items(&params.text_document.uri, params.position);

				if let Some(id) = id {
					write_lsp_message(writer, &json!({
						"jsonrpc": "2.0",
						"id": id,
						"result": items
					}))?;
				}

				Ok(false)
			}
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
					write_lsp_message(writer, &json!({
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
					write_lsp_message(writer, &json!({
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

	fn member_completion_items(&self, uri: &str, position: Position) -> Option<Vec<CompletionItem>> {
		let document = self.open_documents.get(uri)?;
		let cursor_offset = source_offset_for_position(&document.text, position.line, position.character)?;
		let file_path = file_path_from_document_uri(uri)?;
		let config_path = discover_project_config_path(&file_path)?;
		let schema_catalog = read_schema_catalog_from_runtime_config_path(&config_path).ok()?;
		member_completion_items(&document.text, cursor_offset, Some(&schema_catalog))
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
		let diagnostics = self.document_diagnostics(uri, &document.text)?
			.into_iter()
			.map(diagnostic_to_json)
			.collect::<Vec<_>>();

		write_lsp_message(writer, &json!({
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
		write_lsp_message(writer, &json!({
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

fn completion_item_kind_to_lsp(kind: CompletionItemKind) -> u32 {
	match kind {
		CompletionItemKind::Enum => 13,
		CompletionItemKind::EnumMember => 20,
		CompletionItemKind::Field => 5,
		CompletionItemKind::BuiltInFunction | CompletionItemKind::Function => 3,
		CompletionItemKind::Keyword => 14,
		CompletionItemKind::Literal => 21,
		CompletionItemKind::LocalSymbol | CompletionItemKind::Parameter => 6,
		CompletionItemKind::Object => 22,
		CompletionItemKind::Type => 25,
	}
}

fn completion_item_to_json(item: CompletionItem) -> JsonValue {
	json!({
		"label": item.label,
		"kind": completion_item_kind_to_lsp(item.kind),
		"detail": item.detail,
	})
}

fn diagnostic_to_json(diagnostic: Diagnostic) -> JsonValue {
	json!({
		"severity": diagnostic.severity,
		"source": "tablolsp",
		"message": diagnostic.message,
		"range": {
			"start": {
				"line": diagnostic.range.start.line,
				"character": diagnostic.range.start.character,
			},
			"end": {
				"line": diagnostic.range.end.line,
				"character": diagnostic.range.end.character,
			}
		}
	})
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

fn write_lsp_message<W: Write>(writer: &mut W, message: &JsonValue) -> Result<(), String> {
	let body = serde_json::to_vec(message)
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
	use serde_json::json;
	use serde_json::Value as JsonValue;

	use super::LspServer;
	use super::OpenDocument;
	use super::Position;
	use super::diagnostic_to_json;
	use super::discover_project_config_path;
	use tablo::TabloError;
	use tablo::compiler::CompileError;
	use tablo::diagnostics::diagnostic_for_tablo_error;
	use tablo::utils::unique_temp_directory;

	#[test]
	fn completion_items_include_document_symbols_before_cursor() {
		let mut server = LspServer::new();
		server.open_documents.insert(
			String::from("file:///tmp/example.tablo"),
			OpenDocument {
				text: String::from("fn Helper(value: int) int { var localValue: int = value; return localValue; }\nfn Main(args: [text]) int { Hel }"),
				version: 1,
			},
		);

		let items = server.completion_items(
			"file:///tmp/example.tablo",
			Position {
				line: 1,
				character: 32,
			},
		);
		let labels = items.into_iter()
			.filter_map(|item| item.get("label").and_then(JsonValue::as_str).map(str::to_string))
			.collect::<Vec<_>>();

		assert!(labels.contains(&String::from("Helper")));
		assert!(labels.contains(&String::from("localValue")));
		assert!(labels.contains(&String::from("value")));
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
		let diagnostic = diagnostic_to_json(diagnostic_for_tablo_error(
			source,
			TabloError::Compile(CompileError {
				message: String::from("Cannot assign a value of type `text` to a variable of type `int`."),
				position: 41,
			}),
		));
		let diagnostic = diagnostic.to_string();

		assert!(diagnostic.contains("Cannot assign a value of type `text` to a variable of type `int`."));
		assert!(diagnostic.contains("\"character\":41"));
		assert!(diagnostic.contains("\"character\":46"));
	}

	#[test]
	fn highlights_full_span_for_unknown_table_field_diagnostic() {
		let source = "with ExampleDb;\nfn Main(args: [text]) int { count Customers where MissingField == 1; return 0; }";
		let diagnostic = diagnostic_to_json(diagnostic_for_tablo_error(
			source,
			TabloError::Compile(CompileError {
				message: String::from("Field `MissingField` does not exist on table `Customers`."),
				position: 66,
			}),
		));
		let diagnostic = diagnostic.to_string();

		assert!(diagnostic.contains("Field `MissingField` does not exist on table `Customers`."));
		assert!(diagnostic.contains("\"character\":50"));
	}
}
