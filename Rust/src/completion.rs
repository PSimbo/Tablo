use crate::builtins::BuiltInFunction;
use crate::schema::ResolvedTable;
use crate::schema::SchemaCatalog;
use crate::schema::SchemaDataType;
use crate::source::find_matching_paren;
use crate::source::is_identifier_char;
use crate::source::read_identifier;
use crate::source::skip_comment;
use crate::source::skip_string_literal;
use crate::source::skip_whitespace;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompletionItemKind {
	BuiltInFunction,
	Enum,
	EnumMember,
	Field,
	Function,
	Keyword,
	Literal,
	LocalSymbol,
	Object,
	Parameter,
	Type,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompletionItem {
	pub detail: &'static str,
	pub kind: CompletionItemKind,
	pub label: String,
}

pub fn collect_document_completion_items(source: &str) -> Vec<CompletionItem> {
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
			"const" | "var" => {
				if let Some((name, next_index)) = parse_variable_completion_item(source, identifier.end) {
					items.push(CompletionItem {
						label: name,
						kind: CompletionItemKind::LocalSymbol,
						detail: "Local symbol",
					});
					index = next_index;
					continue;
				}
			}
			"enum" => {
				if let Some((name, next_index)) = parse_named_declaration(source, identifier.end) {
					items.push(CompletionItem {
						label: name,
						kind: CompletionItemKind::Enum,
						detail: "Enum",
					});
					index = next_index;
					continue;
				}
			}
			"fn" => {
				if let Some((name, next_index)) = parse_function_completion_items(source, identifier.end, &mut items) {
					items.push(CompletionItem {
						label: name,
						kind: CompletionItemKind::Function,
						detail: "Function",
					});
					index = next_index;
					continue;
				}
			}
			"obj" => {
				if let Some((name, next_index)) = parse_named_declaration(source, identifier.end) {
					items.push(CompletionItem {
						label: name,
						kind: CompletionItemKind::Object,
						detail: "Object",
					});
					index = next_index;
					continue;
				}
			}
			"rec" => {
				if let Some((name, next_index)) = parse_record_completion_item(source, identifier.end) {
					items.push(CompletionItem {
						label: name,
						kind: CompletionItemKind::LocalSymbol,
						detail: "Local symbol",
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

pub fn dedupe_completion_items(items: &mut Vec<CompletionItem>) {
	let mut seen = std::collections::BTreeSet::new();
	items.retain(|item| seen.insert(item.label.clone()));
}

pub fn default_completion_items() -> Vec<CompletionItem> {
	let mut items = vec![
		CompletionItem { label: String::from("and"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("any"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("asc"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("bool"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("break"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("by"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("const"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("continue"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("count"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("date"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("dec"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("delete"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("desc"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("else"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("enum"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("false"), kind: CompletionItemKind::Literal, detail: "Literal" },
		CompletionItem { label: String::from("find"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("first"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("float"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("fn"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("for"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("if"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("in"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("int"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("last"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("mut"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("not"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("null"), kind: CompletionItemKind::Literal, detail: "Literal" },
		CompletionItem { label: String::from("obj"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("or"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("order"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("pub"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("rec"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("return"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("text"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("time"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("timestamp"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("timestamptz"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("timetz"), kind: CompletionItemKind::Type, detail: "Type" },
		CompletionItem { label: String::from("true"), kind: CompletionItemKind::Literal, detail: "Literal" },
		CompletionItem { label: String::from("update"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("var"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("where"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("while"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("with"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
		CompletionItem { label: String::from("xor"), kind: CompletionItemKind::Keyword, detail: "Keyword" },
	];
	items.extend(built_in_completion_items());
	dedupe_completion_items(&mut items);
	items
}

pub fn member_completion_items(
	source: &str,
	cursor_offset: usize,
	schema_catalog: Option<&SchemaCatalog>,
) -> Option<Vec<CompletionItem>> {
	let visible_text = &source[..cursor_offset.min(source.len())];
	let receiver_chain = extract_member_receiver_chain(visible_text)?;
	let active_databases = collect_active_databases(visible_text);
	let object_declarations = collect_object_declarations(source);
	let enum_declarations = collect_enum_declarations(source);
	let visible_bindings = collect_visible_bindings(
		visible_text,
		schema_catalog,
		&active_databases,
		&object_declarations,
	);
	let member_type = resolve_member_type(
		&receiver_chain,
		&visible_bindings,
		&object_declarations,
		schema_catalog,
		&active_databases,
	)?;

	if let Some(variants) = enum_declarations.get(&member_type) {
		return Some(variants.iter()
			.map(|variant| CompletionItem {
				label: variant.clone(),
				kind: CompletionItemKind::EnumMember,
				detail: "Enum member",
			})
			.collect());
	}

	if let Some(fields) = object_declarations.get(&member_type) {
		return Some(fields.iter()
			.map(|field| CompletionItem {
				label: field.name.clone(),
				kind: CompletionItemKind::Field,
				detail: "Object field",
			})
			.collect());
	}

	let schema_catalog = schema_catalog?;
	let resolved = resolve_type_name_to_table(&member_type, schema_catalog, &active_databases)?;
	Some(resolved.table().columns()
		.map(|column| CompletionItem {
			label: String::from(column.name()),
			kind: CompletionItemKind::Field,
			detail: "Record field",
		})
		.collect())
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ObjectFieldInfo {
	name: String,
	type_name: Option<String>,
}

struct ObjectFieldType {
	next_index: usize,
	type_name: Option<String>,
}

fn built_in_completion_items() -> Vec<CompletionItem> {
	BuiltInFunction::all().into_iter()
		.map(|built_in| CompletionItem {
			label: String::from(built_in.name()),
			kind: CompletionItemKind::BuiltInFunction,
			detail: "Built-in function",
		})
		.collect()
}

fn collect_active_databases(source: &str) -> Vec<String> {
	let mut databases = Vec::new();
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

		if identifier.quoted || identifier.value != "with" {
			index = identifier.end;
			continue;
		}

		index = identifier.end;
		loop {
			index = skip_whitespace(source, index);
			let Some(database) = read_identifier(source, index) else {
				break;
			};
			databases.push(database.value);
			index = skip_whitespace(source, database.end);
			match source[index..].chars().next() {
				Some(',') => {
					index += 1;
				}
				Some(';') => {
					index += 1;
					break;
				}
				_ => break,
			}
		}
	}

	databases.sort();
	databases.dedup_by(|lhs, rhs| lhs.eq_ignore_ascii_case(rhs));
	databases
}

fn collect_enum_declarations(source: &str) -> std::collections::BTreeMap<String, Vec<String>> {
	let mut declarations = std::collections::BTreeMap::new();

	for (name, body) in find_brace_declarations(source, "enum") {
		declarations.insert(name, extract_top_level_member_names(&body));
	}

	declarations
}

fn collect_function_parameter_bindings(
	parameters: &str,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
) -> std::collections::BTreeMap<String, String> {
	let mut bindings = std::collections::BTreeMap::new();
	let mut index = 0;

	while index < parameters.len() {
		index = skip_whitespace(parameters, index);
		let Some(name) = read_identifier(parameters, index) else {
			index += parameters[index..].chars().next().map(char::len_utf8).unwrap_or(1);
			continue;
		};
		let colon_index = skip_whitespace(parameters, name.end);
		if parameters[colon_index..].chars().next() != Some(':') {
			index = name.end;
			continue;
		}

		let type_start = skip_whitespace(parameters, colon_index + 1);
		let type_end = skip_expression(parameters, type_start, &[","]);
		let type_name = extract_reference_type_name(parameters[type_start..type_end].trim())
			.or_else(|| {
				let schema_catalog = schema_catalog?;
				resolve_record_pointer_parameter_type(
					parameters[type_start..type_end].trim(),
					schema_catalog,
					active_databases,
				).map(resolved_table_type_name)
			});

		if let Some(type_name) = type_name {
			bindings.insert(name.value, type_name);
		}
		index = type_end.saturating_add(1);
	}

	bindings
}

fn collect_object_declarations(source: &str) -> std::collections::BTreeMap<String, Vec<ObjectFieldInfo>> {
	let mut declarations = std::collections::BTreeMap::new();

	for (name, body) in find_brace_declarations(source, "obj") {
		collect_object_fields(&name, &body, &mut declarations);
	}

	declarations
}

fn collect_object_fields(
	type_name: &str,
	body: &str,
	objects: &mut std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
) {
	let mut fields = Vec::new();
	let mut depth: usize = 0;
	let mut index = 0;

	while index < body.len() {
		if let Some(comment_end) = skip_comment(body, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(body, index) {
			index = string_end;
			continue;
		}

		let Some(ch) = body[index..].chars().next() else {
			break;
		};

		match ch {
			'{' => {
				depth += 1;
				index += 1;
				continue;
			}
			'}' => {
				depth = depth.saturating_sub(1);
				index += 1;
				continue;
			}
			_ => {}
		}

		if depth == 0
			&& let Some(identifier) = read_identifier(body, index) {
			let colon_index = skip_whitespace(body, identifier.end);
			if body[colon_index..].chars().next() == Some(':') {
				let field_type = read_object_field_type(
					body,
					colon_index + 1,
					type_name,
					&identifier.value,
					objects,
				);
				fields.push(ObjectFieldInfo {
					name: identifier.value,
					type_name: field_type.type_name,
				});
				index = field_type.next_index;
				continue;
			}
		}

		index += ch.len_utf8();
	}

	dedupe_object_fields(&mut fields);
	objects.insert(String::from(type_name), fields);
}

fn collect_parameter_completion_items(parameters: &str, items: &mut Vec<CompletionItem>) {
	let mut index = 0;

	while index < parameters.len() {
		index = skip_whitespace(parameters, index);
		let Some(name) = read_identifier(parameters, index) else {
			index += parameters[index..].chars().next().map(char::len_utf8).unwrap_or(1);
			continue;
		};
		let colon_index = skip_whitespace(parameters, name.end);
		if parameters[colon_index..].chars().next() == Some(':') {
			items.push(CompletionItem {
				label: name.value,
				kind: CompletionItemKind::Parameter,
				detail: "Parameter",
			});
			index = colon_index + 1;
		}
		else {
			index = name.end;
		}
	}
}

fn collect_visible_bindings(
	source: &str,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
	object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
) -> std::collections::BTreeMap<String, String> {
	let mut scopes = vec![std::collections::BTreeMap::new()];
	let mut pending_scopes = Vec::new();
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

		let Some(ch) = source[index..].chars().next() else {
			break;
		};
		if ch == '{' {
			scopes.push(pending_scopes.pop().unwrap_or_default());
			index += 1;
			continue;
		}
		if ch == '}' {
			if scopes.len() > 1 {
				scopes.pop();
			}
			index += 1;
			continue;
		}

		let Some(identifier) = read_identifier(source, index) else {
			index += ch.len_utf8();
			continue;
		};

		if identifier.quoted {
			index = identifier.end;
			continue;
		}

		match identifier.value.as_str() {
			"const" | "var" => {
				if let Some((name, type_name, next_index)) = parse_variable_binding(source, identifier.end) {
					if let Some(scope) = scopes.last_mut() {
						scope.insert(name, type_name);
					}
					index = next_index;
					continue;
				}
			}
			"fn" => {
				if let Some((bindings, next_index)) = parse_function_bindings(source, identifier.end, schema_catalog, active_databases, object_declarations) {
					pending_scopes.push(bindings);
					index = next_index;
					continue;
				}
			}
			"for" => {
				if let Some((name, type_name, next_index)) = parse_for_binding(source, identifier.end, schema_catalog, active_databases, object_declarations, &merge_string_bindings(&scopes)) {
					pending_scopes.push(std::collections::BTreeMap::from([(name, type_name)]));
					index = next_index;
					continue;
				}
			}
			"if" => {
				if let Some((name, type_name, next_index)) = parse_if_binding(source, identifier.end, schema_catalog, active_databases, object_declarations, &merge_string_bindings(&scopes)) {
					pending_scopes.push(std::collections::BTreeMap::from([(name, type_name)]));
					index = next_index;
					continue;
				}
			}
			"rec" => {
				if let Some((name, type_name, next_index)) = parse_record_type_binding(source, identifier.end, schema_catalog, active_databases, object_declarations, &merge_string_bindings(&scopes)) {
					if let Some(scope) = scopes.last_mut() {
						scope.insert(name, type_name);
					}
					index = next_index;
					continue;
				}
			}
			_ => {}
		}

		index = identifier.end;
	}

	merge_string_bindings(&scopes)
}

fn dedupe_object_fields(fields: &mut Vec<ObjectFieldInfo>) {
	let mut seen = std::collections::BTreeSet::new();
	fields.retain(|field| seen.insert(field.name.clone()));
}

fn extract_member_receiver_chain(source: &str) -> Option<Vec<String>> {
	let trimmed = source.trim_end();
	let before_dot = trimmed.strip_suffix('.')?;
	let start = before_dot.char_indices()
		.rev()
		.find(|(_, ch)| !is_identifier_char(*ch) && *ch != '.' && !ch.is_whitespace())
		.map_or(0, |(index, ch)| index + ch.len_utf8());
	let candidate = before_dot[start..].trim_start();
	split_qualified_chain(candidate)
}

fn extract_object_construction_type_name(source: &str) -> Option<String> {
	let brace_index = source.find('{')?;
	if brace_index == 0 {
		return None;
	}

	let prefix = source[..brace_index].trim();
	if prefix.is_empty() {
		return None;
	}

	let chain = split_qualified_chain(prefix)?;
	Some(chain.join("."))
}

fn extract_reference_type_name(source: &str) -> Option<String> {
	let trimmed = source.trim().trim_end_matches('?').trim();
	if trimmed.is_empty() || trimmed.starts_with('[') || trimmed.starts_with('&') || trimmed.contains('|') {
		return None;
	}

	let parts = split_qualified_chain(trimmed)?;
	Some(parts.join("."))
}

fn extract_top_level_member_names(body: &str) -> Vec<String> {
	let mut members = Vec::new();
	let mut depth: usize = 0;
	let mut index = 0;

	while index < body.len() {
		if let Some(comment_end) = skip_comment(body, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(body, index) {
			index = string_end;
			continue;
		}

		let Some(ch) = body[index..].chars().next() else {
			break;
		};

		match ch {
			'{' => {
				depth += 1;
				index += 1;
				continue;
			}
			'}' => {
				depth = depth.saturating_sub(1);
				index += 1;
				continue;
			}
			_ => {}
		}

		if depth == 0
			&& let Some(identifier) = read_identifier(body, index) {
			members.push(identifier.value);
			index = identifier.end;
			continue;
		}

		index += ch.len_utf8();
	}

	members.sort();
	members.dedup();
	members
}

fn find_brace_declarations(source: &str, keyword: &str) -> Vec<(String, String)> {
	let mut declarations = Vec::new();
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

		if identifier.quoted || identifier.value != keyword {
			index = identifier.end;
			continue;
		}

		let name_index = skip_whitespace(source, identifier.end);
		let Some(name) = read_identifier(source, name_index) else {
			index = identifier.end;
			continue;
		};

		let brace_index = skip_whitespace(source, name.end);
		if source[brace_index..].chars().next() != Some('{') {
			index = name.end;
			continue;
		}

		let Some(end_index) = find_matching_brace(source, brace_index) else {
			index = brace_index + 1;
			continue;
		};

		declarations.push((name.value, String::from(&source[brace_index + 1..end_index])));
		index = end_index + 1;
	}

	declarations
}

fn find_matching_brace(text: &str, open_brace_index: usize) -> Option<usize> {
	let mut depth = 0;
	let mut index = open_brace_index;

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
		if ch == '{' {
			depth += 1;
		}
		else if ch == '}' {
			depth -= 1;
			if depth == 0 {
				return Some(index);
			}
		}

		index += ch.len_utf8();
	}

	None
}

fn find_type_end(source: &str, start: usize) -> usize {
	let mut index = start;
	let mut bracket_depth: usize = 0;

	while index < source.len() {
		if let Some(comment_end) = skip_comment(source, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(source, index) {
			index = string_end;
			continue;
		}

		let Some(ch) = source[index..].chars().next() else {
			break;
		};

		match ch {
			'[' => bracket_depth += 1,
			']' => bracket_depth = bracket_depth.saturating_sub(1),
			'=' | ';' | '\n' | '\r' if bracket_depth == 0 => break,
			_ => {}
		}

		index += ch.len_utf8();
	}

	index
}

fn infer_expression_type(
	source: &str,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
	object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
	visible_bindings: &std::collections::BTreeMap<String, String>,
) -> Option<String> {
	let source = strip_wrapping_parentheses(source.trim());
	if source.is_empty() {
		return None;
	}

	let schema_catalog = schema_catalog;
	if let Some(schema_catalog) = schema_catalog
		&& let Some(resolved) = resolve_record_pointer_expression(source, schema_catalog, active_databases) {
		return Some(resolved_table_type_name(resolved));
	}

	if let Some(type_name) = extract_object_construction_type_name(source) {
		return Some(type_name);
	}

	let chain = split_qualified_chain(source)?;
	resolve_member_type(&chain, visible_bindings, object_declarations, schema_catalog, active_databases)
}

fn merge_string_bindings(
	scopes: &[std::collections::BTreeMap<String, String>],
) -> std::collections::BTreeMap<String, String> {
	let mut merged = std::collections::BTreeMap::new();

	for scope in scopes {
		for (name, type_name) in scope {
			merged.insert(name.clone(), type_name.clone());
		}
	}

	merged
}

fn parse_for_binding(
	source: &str,
	start: usize,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
	object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
	visible_bindings: &std::collections::BTreeMap<String, String>,
) -> Option<(String, String, usize)> {
	let mut index = skip_whitespace(source, start);
	let rec_keyword = read_identifier(source, index)?;
	if rec_keyword.quoted || rec_keyword.value != "rec" {
		return None;
	}

	index = skip_whitespace(source, rec_keyword.end);
	if source[index..].starts_with("mut")
		&& !source[index + 3..].chars().next().is_some_and(is_identifier_char) {
		index = skip_whitespace(source, index + 3);
	}

	let name = read_identifier(source, index)?;
	index = skip_whitespace(source, name.end);
	let in_keyword = read_identifier(source, index)?;
	if in_keyword.quoted || in_keyword.value != "in" {
		return None;
	}

	let expression_start = skip_whitespace(source, in_keyword.end);
	let expression_end = skip_expression(source, expression_start, &["where", "order", "{"]);
	let type_name = infer_expression_type(
		source[expression_start..expression_end].trim(),
		schema_catalog,
		active_databases,
		object_declarations,
		visible_bindings,
	)?;

	Some((name.value, type_name, expression_end))
}

fn parse_function_bindings(
	source: &str,
	start: usize,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
	_object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
) -> Option<(std::collections::BTreeMap<String, String>, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	let mut index = skip_whitespace(source, name.end);
	if source[index..].chars().next()? != '(' {
		return None;
	}

	let end_index = find_matching_paren(source, index)?;
	let parameters = &source[index + 1..end_index];
	let bindings = collect_function_parameter_bindings(parameters, schema_catalog, active_databases);
	index = end_index + 1;
	Some((bindings, index))
}

fn parse_function_completion_items(
	source: &str,
	start: usize,
	items: &mut Vec<CompletionItem>,
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

fn parse_if_binding(
	source: &str,
	start: usize,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
	object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
	visible_bindings: &std::collections::BTreeMap<String, String>,
) -> Option<(String, String, usize)> {
	let mut index = skip_whitespace(source, start);
	let rec_keyword = read_identifier(source, index)?;
	if rec_keyword.quoted || rec_keyword.value != "rec" {
		return None;
	}

	index = skip_whitespace(source, rec_keyword.end);
	let name = read_identifier(source, index)?;
	index = skip_whitespace(source, name.end);
	if source[index..].chars().next()? != '=' {
		return None;
	}

	let expression_start = skip_whitespace(source, index + 1);
	let expression_end = skip_expression(source, expression_start, &["{"]);
	let type_name = infer_expression_type(
		source[expression_start..expression_end].trim(),
		schema_catalog,
		active_databases,
		object_declarations,
		visible_bindings,
	)?;

	Some((name.value, type_name, expression_end))
}

fn parse_named_declaration(source: &str, start: usize) -> Option<(String, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	Some((name.value, name.end))
}

fn parse_record_type_binding(
	source: &str,
	start: usize,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
	object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
	visible_bindings: &std::collections::BTreeMap<String, String>,
) -> Option<(String, String, usize)> {
	let mut index = skip_whitespace(source, start);
	if source[index..].starts_with("mut")
		&& !source[index + 3..].chars().next().is_some_and(is_identifier_char) {
		index = skip_whitespace(source, index + 3);
	}

	let name = read_identifier(source, index)?;
	index = skip_whitespace(source, name.end);
	if source[index..].chars().next()? != '=' {
		return None;
	}

	let expression_start = skip_whitespace(source, index + 1);
	let expression_end = skip_expression(source, expression_start, &[";"]);
	let type_name = infer_expression_type(
		source[expression_start..expression_end].trim(),
		schema_catalog,
		active_databases,
		object_declarations,
		visible_bindings,
	)?;

	Some((name.value, type_name, expression_end))
}

fn parse_variable_binding(source: &str, start: usize) -> Option<(String, String, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	let mut index = skip_whitespace(source, name.end);
	if source[index..].chars().next()? != ':' {
		return None;
	}

	index = skip_whitespace(source, index + 1);
	let type_end = find_type_end(source, index);
	let type_name = extract_reference_type_name(source[index..type_end].trim())?;
	Some((name.value, type_name, type_end))
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

fn read_object_field_type(
	body: &str,
	start: usize,
	parent_type_name: &str,
	field_name: &str,
	objects: &mut std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
) -> ObjectFieldType {
	let mut index = skip_whitespace(body, start);

	if body[index..].chars().next() == Some('{') {
		if let Some(end_index) = find_matching_brace(body, index) {
			let inline_type_name = format!("{parent_type_name}.{field_name}");
			collect_object_fields(&inline_type_name, &body[index + 1..end_index], objects);
			return ObjectFieldType {
				next_index: skip_object_field_tail(body, end_index + 1),
				type_name: Some(inline_type_name),
			};
		}

		return ObjectFieldType {
			next_index: body.len(),
			type_name: None,
		};
	}

	let type_start = index;
	let mut paren_depth: usize = 0;
	let mut bracket_depth: usize = 0;

	while index < body.len() {
		if let Some(comment_end) = skip_comment(body, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(body, index) {
			index = string_end;
			continue;
		}

		let Some(ch) = body[index..].chars().next() else {
			break;
		};

		match ch {
			'(' => paren_depth += 1,
			')' => paren_depth = paren_depth.saturating_sub(1),
			'[' => bracket_depth += 1,
			']' => bracket_depth = bracket_depth.saturating_sub(1),
			',' | '=' | '\n' | '\r' if paren_depth == 0 && bracket_depth == 0 => break,
			_ => {}
		}

		index += ch.len_utf8();
	}

	ObjectFieldType {
		next_index: skip_object_field_tail(body, index),
		type_name: extract_reference_type_name(body[type_start..index].trim()),
	}
}

fn resolve_member_type(
	chain: &[String],
	visible_bindings: &std::collections::BTreeMap<String, String>,
	object_declarations: &std::collections::BTreeMap<String, Vec<ObjectFieldInfo>>,
	schema_catalog: Option<&SchemaCatalog>,
	active_databases: &[String],
) -> Option<String> {
	let mut current_type = visible_bindings.get(chain.first()?).cloned()
		.or_else(|| Some(chain.first()?.clone()))?;

	for field_name in chain.iter().skip(1) {
		if let Some(fields) = object_declarations.get(&current_type) {
			let field = fields.iter().find(|candidate| candidate.name.eq_ignore_ascii_case(field_name))?;
			current_type = field.type_name.clone()?;
			continue;
		}

		let schema_catalog = schema_catalog?;
		let resolved = resolve_type_name_to_table(&current_type, schema_catalog, active_databases)?;
		let column = resolved.table().columns()
			.find(|candidate| candidate.name().eq_ignore_ascii_case(field_name))?;
		current_type = schema_data_type_name(column.data_type())?;
	}

	Some(current_type)
}

fn resolve_record_pointer_expression<'a>(
	source: &str,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<ResolvedTable<'a>> {
	let source = source.trim();
	if let Some(rest) = source.strip_prefix("find") {
		let mut index = skip_whitespace(rest, 0);
		if let Some(keyword) = read_identifier(rest, index)
			&& (keyword.value == "first" || keyword.value == "last") {
			index = skip_whitespace(rest, keyword.end);
		}
		return resolve_table_reference(rest[index..].trim_start(), schema_catalog, active_databases);
	}

	if let Some(rest) = source.strip_prefix("new") {
		return resolve_table_reference(rest.trim_start(), schema_catalog, active_databases);
	}

	None
}

fn resolve_record_pointer_parameter_type<'a>(
	source: &str,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<ResolvedTable<'a>> {
	let mut text = source.trim();
	if let Some(rest) = text.strip_prefix('&') {
		text = rest.trim_start();
	}
	let Some(rest) = text.strip_prefix("rec") else {
		return None;
	};
	let table_reference = rest.trim_start();
	resolve_table_reference(table_reference, schema_catalog, active_databases)
}

fn resolve_table_reference<'a>(
	source: &str,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<ResolvedTable<'a>> {
	let reference_end = skip_expression(source, 0, &["where", "order", "{", ";", ","]);
	let reference = source[..reference_end].trim();
	let parts = split_qualified_chain(reference)?;
	let active_database_refs = active_databases.iter().map(String::as_str).collect::<Vec<_>>();

	match parts.as_slice() {
		[table] => schema_catalog.resolve_table(&active_database_refs, table).ok(),
		[first, table] => {
			schema_catalog.resolve_database_table(&active_database_refs, first, table).ok()
				.or_else(|| schema_catalog.resolve_schema_table(&active_database_refs, first, table).ok())
		}
		[database, schema, table] => {
			schema_catalog.resolve_database_schema_table(&active_database_refs, database, schema, table).ok()
		}
		_ => None,
	}
}

fn resolve_type_name_to_table<'a>(
	type_name: &str,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<ResolvedTable<'a>> {
	let table_reference = type_name.strip_prefix("@record:")?;
	resolve_table_reference(table_reference, schema_catalog, active_databases)
}

fn resolved_table_type_name(resolved: ResolvedTable<'_>) -> String {
	let database_name = resolved.database().name();
	let schema_name = resolved.schema().name();
	let table_name = resolved.table().name();
	format!("@record:{database_name}.{schema_name}.{table_name}")
}

fn schema_data_type_name(data_type: &SchemaDataType) -> Option<String> {
	match data_type {
		SchemaDataType::Array(element_type) => Some(format!("[{}]", schema_data_type_name(element_type)?)),
		SchemaDataType::Bool => Some(String::from("bool")),
		SchemaDataType::Date => Some(String::from("date")),
		SchemaDataType::Dec | SchemaDataType::Float => Some(String::from("dec")),
		SchemaDataType::Int => Some(String::from("int")),
		SchemaDataType::Text => Some(String::from("text")),
		SchemaDataType::Time => Some(String::from("time")),
		SchemaDataType::TimeTz => Some(String::from("timetz")),
		SchemaDataType::Timestamp => Some(String::from("timestamp")),
		SchemaDataType::TimestampTz => Some(String::from("timestamptz")),
		_ => None,
	}
}

fn skip_expression(source: &str, start: usize, terminators: &[&str]) -> usize {
	let mut brace_depth: usize = 0;
	let mut bracket_depth: usize = 0;
	let mut paren_depth: usize = 0;
	let mut index = start;

	while index < source.len() {
		if let Some(comment_end) = skip_comment(source, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(source, index) {
			index = string_end;
			continue;
		}

		let Some(ch) = source[index..].chars().next() else {
			break;
		};

		match ch {
			'{' => {
				if brace_depth == 0 && terminators.contains(&"{") {
					break;
				}
				brace_depth += 1;
			}
			'}' => brace_depth = brace_depth.saturating_sub(1),
			'[' => bracket_depth += 1,
			']' => bracket_depth = bracket_depth.saturating_sub(1),
			'(' => paren_depth += 1,
			')' => paren_depth = paren_depth.saturating_sub(1),
			';' if brace_depth == 0 && bracket_depth == 0 && paren_depth == 0 && terminators.contains(&";") => break,
			',' if brace_depth == 0 && bracket_depth == 0 && paren_depth == 0 && terminators.contains(&",") => break,
			_ => {
				if brace_depth == 0 && bracket_depth == 0 && paren_depth == 0 {
					for terminator in terminators {
						if terminator.len() > 1
							&& source[index..].starts_with(terminator)
							&& !source[index + terminator.len()..].chars().next().is_some_and(is_identifier_char) {
							return index;
						}
					}
				}
			}
		}

		index += ch.len_utf8();
	}

	index
}

fn skip_object_field_tail(body: &str, start: usize) -> usize {
	let mut index = start;
	let mut brace_depth: usize = 0;
	let mut bracket_depth: usize = 0;
	let mut paren_depth: usize = 0;

	while index < body.len() {
		if let Some(comment_end) = skip_comment(body, index) {
			index = comment_end;
			continue;
		}

		if let Some(string_end) = skip_string_literal(body, index) {
			index = string_end;
			continue;
		}

		let Some(ch) = body[index..].chars().next() else {
			break;
		};

		match ch {
			'{' => brace_depth += 1,
			'}' => {
				if brace_depth == 0 {
					return index;
				}
				brace_depth -= 1;
			}
			'[' => bracket_depth += 1,
			']' => bracket_depth = bracket_depth.saturating_sub(1),
			'(' => paren_depth += 1,
			')' => paren_depth = paren_depth.saturating_sub(1),
			',' if brace_depth == 0 && bracket_depth == 0 && paren_depth == 0 => return index,
			_ => {}
		}

		index += ch.len_utf8();
	}

	index
}

fn split_qualified_chain(source: &str) -> Option<Vec<String>> {
	let mut parts = Vec::new();
	let mut index = 0;
	let text = source.trim();

	while index < text.len() {
		let identifier = read_identifier(text, index)?;
		parts.push(identifier.value);
		index = read_identifier(text, index)?.end;
		if index == text.len() {
			break;
		}
		if text[index..].chars().next()? != '.' {
			return None;
		}
		index += 1;
	}

	Some(parts)
}

fn strip_wrapping_parentheses(source: &str) -> &str {
	let mut current = source.trim();

	while current.starts_with('(') && current.ends_with(')') {
		let Some(end_index) = find_matching_paren(current, 0) else {
			break;
		};
		if end_index != current.len() - 1 {
			break;
		}

		current = current[1..current.len() - 1].trim();
	}

	current
}

#[cfg(test)]
mod tests {
	use crate::schema_fixture::read_schema_catalog_from_str;

	use super::collect_document_completion_items;
	use super::default_completion_items;
	use super::member_completion_items;

	#[test]
	fn completion_items_dedupe_labels() {
		let items = default_completion_items();
		let date_count = items.iter().filter(|item| item.label == "date").count();
		let text_count = items.iter().filter(|item| item.label == "text").count();

		assert_eq!(date_count, 1);
		assert_eq!(text_count, 1);
	}

	#[test]
	fn document_completion_items_include_document_symbols_before_cursor() {
		let source = "fn Helper(value: int) int { var localValue: int = value; return localValue; }\nfn Main(args: [text]) int { Hel";
		let items = collect_document_completion_items(source);
		let labels = items.into_iter().map(|item| item.label).collect::<Vec<_>>();

		assert!(labels.contains(&String::from("Helper")));
		assert!(labels.contains(&String::from("localValue")));
		assert!(labels.contains(&String::from("value")));
	}

	#[test]
	fn member_completion_items_include_enum_variants() {
		let source = "\
enum Status {
	Pending,
	Complete,
};

fn Main(args: [text]) int {
	Status.
";
		let items = member_completion_items(source, source.len(), None).unwrap();
		let labels = items.into_iter().map(|item| item.label).collect::<Vec<_>>();

		assert!(labels.contains(&String::from("Pending")));
		assert!(labels.contains(&String::from("Complete")));
	}

	#[test]
	fn member_completion_items_include_object_fields() {
		let source = "\
obj Config {
	Values: {
		FieldA: text,
		FieldB: int,
	}
};

fn Main(args: [text]) int {
	var config: Config = Config {};
	config.Values.
";
		let items = member_completion_items(source, source.len(), None).unwrap();
		let labels = items.into_iter().map(|item| item.label).collect::<Vec<_>>();

		assert!(labels.contains(&String::from("FieldA")));
		assert!(labels.contains(&String::from("FieldB")));
	}

	#[test]
	fn member_completion_items_include_record_pointer_fields() {
		let schema = read_schema_catalog_from_str(
			"database ExampleDb; schema Main implicit; create table Customers (Id int, Name text);",
		).unwrap();
		let source = "with ExampleDb;\nfn Main(args: [text]) int { rec tp = find first Customers where Id == 1; tp.";
		let items = member_completion_items(source, source.len(), Some(&schema)).unwrap();
		let labels = items.into_iter().map(|item| item.label).collect::<Vec<_>>();

		assert!(labels.contains(&String::from("Id")));
		assert!(labels.contains(&String::from("Name")));
	}
}
