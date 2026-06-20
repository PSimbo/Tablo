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

#[derive(Clone)]
struct CompletionItemSpec {
	detail: String,
	kind: u32,
	label: String,
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

struct ParsedIdentifier {
	end: usize,
	quoted: bool,
	value: String,
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
		let mut items = default_completion_items();
		items.extend(self.document_completion_items(uri, position));
		dedupe_completion_items(&mut items);
		items.into_iter()
			.filter(|item| prefix.as_ref().is_none_or(|prefix| item.label.starts_with(prefix)))
			.map(|item| json!({
				"label": item.label,
				"kind": item.kind,
				"detail": item.detail,
			}))
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

	fn document_completion_items(&self, uri: &str, position: Position) -> Vec<CompletionItemSpec> {
		let Some(document) = self.open_documents.get(uri) else {
			return Vec::new();
		};
		let Some(cursor_offset) = source_offset_for_position(&document.text, position) else {
			return Vec::new();
		};
		let visible_text = &document.text[..cursor_offset];
		collect_document_completion_items(visible_text)
	}

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
					write_lsp_message(writer, json!({
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

fn built_in_completion_items() -> Vec<CompletionItemSpec> {
	vec![
		CompletionItemSpec { label: String::from("bool"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("contains"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("countof"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("date"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("day"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("dec"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("disp"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("displn"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("exists"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("float"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("format"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("hour"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("ilike"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("indexof"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("int"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("lastof"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("len"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("like"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("locked"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("max"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("min"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("minute"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("month"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("second"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("split"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("text"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("time"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("timestamp"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("timetz"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("timestamptz"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("trim"), kind: 3, detail: String::from("Built-in function") },
		CompletionItemSpec { label: String::from("year"), kind: 3, detail: String::from("Built-in function") },
	]
}

fn collect_document_completion_items(source: &str) -> Vec<CompletionItemSpec> {
	let mut items = Vec::new();
	let mut index = 0;

	while index < source.len() {
		if let Some(comment_end) = skip_comment(source, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(source, index) {
			index = string_end;
			continue;
		}

		let Some(identifier) = read_identifier(source, index) else {
			index += source[index..].chars().next().map(char::len_utf8).unwrap_or(1);
			continue;
		};

		if identifier.quoted {
			index = identifier.end;
			continue;
		}

		match identifier.value.as_str() {
			"fn" => {
				if let Some((name, next_index)) = parse_function_completion_items(source, identifier.end, &mut items) {
					items.push(CompletionItemSpec {
						label: name,
						kind: 3,
						detail: String::from("Function"),
					});
					index = next_index;
					continue;
				}
			}
			"obj" => {
				if let Some((name, next_index)) = parse_named_declaration(source, identifier.end) {
					items.push(CompletionItemSpec {
						label: name,
						kind: 22,
						detail: String::from("Object"),
					});
					index = next_index;
					continue;
				}
			}
			"enum" => {
				if let Some((name, next_index)) = parse_named_declaration(source, identifier.end) {
					items.push(CompletionItemSpec {
						label: name,
						kind: 13,
						detail: String::from("Enum"),
					});
					index = next_index;
					continue;
				}
			}
			"var" | "const" => {
				if let Some((name, next_index)) = parse_variable_completion_item(source, identifier.end) {
					items.push(CompletionItemSpec {
						label: name,
						kind: 6,
						detail: String::from("Local symbol"),
					});
					index = next_index;
					continue;
				}
			}
			"rec" => {
				if let Some((name, next_index)) = parse_record_completion_item(source, identifier.end) {
					items.push(CompletionItemSpec {
						label: name,
						kind: 6,
						detail: String::from("Local symbol"),
					});
					index = next_index;
					continue;
				}
			}
			_ => {}
		}

		index = identifier.end;
	}

	items
}

fn collect_parameter_completion_items(parameters: &str, items: &mut Vec<CompletionItemSpec>) {
	let mut index = 0;

	while index < parameters.len() {
		index = skip_whitespace(parameters, index);
		let Some(name) = read_identifier(parameters, index) else {
			index += parameters[index..].chars().next().map(char::len_utf8).unwrap_or(1);
			continue;
		};
		let colon_index = skip_whitespace(parameters, name.end);
		if parameters[colon_index..].chars().next() == Some(':') {
			items.push(CompletionItemSpec {
				label: name.value,
				kind: 6,
				detail: String::from("Parameter"),
			});
			index = colon_index + 1;
		}
		else {
			index = name.end;
		}
	}
}

fn dedupe_completion_items(items: &mut Vec<CompletionItemSpec>) {
	let mut seen = std::collections::BTreeSet::new();
	items.retain(|item| seen.insert(item.label.clone()));
}

fn default_completion_items() -> Vec<CompletionItemSpec> {
	let mut items = vec![
		CompletionItemSpec { label: String::from("and"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("any"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("asc"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("bool"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("break"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("by"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("const"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("continue"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("count"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("date"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("dec"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("delete"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("desc"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("else"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("enum"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("false"), kind: 21, detail: String::from("Literal") },
		CompletionItemSpec { label: String::from("find"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("first"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("float"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("fn"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("for"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("if"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("in"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("int"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("last"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("mut"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("not"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("null"), kind: 21, detail: String::from("Literal") },
		CompletionItemSpec { label: String::from("obj"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("or"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("order"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("pub"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("rec"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("return"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("text"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("time"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("timestamp"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("timestamptz"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("timetz"), kind: 25, detail: String::from("Type") },
		CompletionItemSpec { label: String::from("true"), kind: 21, detail: String::from("Literal") },
		CompletionItemSpec { label: String::from("update"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("var"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("where"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("while"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("with"), kind: 14, detail: String::from("Keyword") },
		CompletionItemSpec { label: String::from("xor"), kind: 14, detail: String::from("Keyword") },
	];
	items.extend(built_in_completion_items());
	dedupe_completion_items(&mut items);
	items
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

fn find_matching_paren(text: &str, open_paren_index: usize) -> Option<usize> {
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

fn parse_function_completion_items(
	source: &str,
	start: usize,
	items: &mut Vec<CompletionItemSpec>,
) -> Option<(String, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	let mut index = skip_whitespace(source, name.end);

	if source[index..].chars().next()? != '(' {
		return None;
	}

	let end_index = find_matching_paren(source, index)?;
	let parameters = &source[index + 1..end_index];
	collect_parameter_completion_items(parameters, items);

	index = end_index + 1;
	Some((name.value, index))
}

fn parse_named_declaration(source: &str, start: usize) -> Option<(String, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	Some((name.value, name.end))
}

fn parse_record_completion_item(source: &str, start: usize) -> Option<(String, usize)> {
	let mut index = skip_whitespace(source, start);
	if source[index..].starts_with("mut")
		&& !source[index + 3..].chars().next().is_some_and(is_identifier_char) {
		index = skip_whitespace(source, index + 3);
	}

	let name = read_identifier(source, index)?;
	Some((name.value, name.end))
}

fn parse_variable_completion_item(source: &str, start: usize) -> Option<(String, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	Some((name.value, name.end))
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

fn read_identifier(text: &str, start: usize) -> Option<ParsedIdentifier> {
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
		if !(current.is_ascii_alphanumeric() || current == '_') {
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

fn skip_comment(text: &str, start: usize) -> Option<usize> {
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

fn skip_string_literal(text: &str, start: usize) -> Option<usize> {
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

fn skip_whitespace(text: &str, start: usize) -> usize {
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

fn source_offset_for_position(text: &str, position: Position) -> Option<usize> {
	let source = SourceText::new(text);
	let line_starts = source.line_starts();
	let line_start = *line_starts.get(position.line as usize)?;
	let line_text = source.line_text(line_start);
	let mut offset = line_start;
	let mut remaining = position.character as usize;

	for ch in line_text.chars() {
		if remaining == 0 {
			break;
		}
		offset += ch.len_utf8();
		remaining -= 1;
	}

	Some(offset)
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
	use serde_json::Value as JsonValue;

	use super::LspServer;
	use super::OpenDocument;
	use super::Position;
	use super::default_completion_items;
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
	fn completion_items_dedupe_labels() {
		let items = default_completion_items();
		let date_count = items.iter().filter(|item| item.label == "date").count();
		let text_count = items.iter().filter(|item| item.label == "text").count();

		assert_eq!(date_count, 1);
		assert_eq!(text_count, 1);
	}

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
