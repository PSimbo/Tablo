use crate::builtins::BuiltInFunction;
use crate::schema::ResolvedTable;
use crate::schema::SchemaCatalog;
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

fn built_in_completion_items() -> Vec<CompletionItem> {
	BuiltInFunction::all().into_iter()
		.map(|built_in| CompletionItem {
			label: String::from(built_in.name()),
			kind: CompletionItemKind::BuiltInFunction,
			detail: "Built-in function",
		})
		.collect()
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
	let schema_catalog = schema_catalog?;
	let visible_text = &source[..cursor_offset.min(source.len())];
	let receiver = extract_member_receiver(visible_text)?;
	let active_databases = collect_active_databases(visible_text);
	let record_pointer_bindings = collect_visible_record_pointer_bindings(visible_text, schema_catalog, &active_databases);
	let resolved = record_pointer_bindings.get(&receiver)?;

	Some(resolved.table().columns()
		.map(|column| CompletionItem {
			label: String::from(column.name()),
			kind: CompletionItemKind::Field,
			detail: "Record field",
		})
		.collect())
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

fn collect_function_parameter_record_pointer_bindings<'a>(
	parameters: &str,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> std::collections::BTreeMap<String, ResolvedTable<'a>> {
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
		if let Some(resolved) = resolve_record_pointer_parameter_type(
			parameters[type_start..type_end].trim(),
			schema_catalog,
			active_databases,
		) {
			bindings.insert(name.value, resolved);
		}
		index = type_end.saturating_add(1);
	}

	bindings
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

fn collect_visible_record_pointer_bindings<'a>(
	source: &str,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> std::collections::BTreeMap<String, ResolvedTable<'a>> {
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
			"fn" => {
				if let Some((bindings, next_index)) = parse_function_record_pointer_bindings(source, identifier.end, schema_catalog, active_databases) {
					pending_scopes.push(bindings);
					index = next_index;
					continue;
				}
			}
			"if" => {
				if let Some((name, resolved, next_index)) = parse_if_record_binding(source, identifier.end, schema_catalog, active_databases) {
					pending_scopes.push(std::collections::BTreeMap::from([(name, resolved)]));
					index = next_index;
					continue;
				}
			}
			"for" => {
				if let Some((name, resolved, next_index)) = parse_for_record_binding(source, identifier.end, schema_catalog, active_databases) {
					pending_scopes.push(std::collections::BTreeMap::from([(name, resolved)]));
					index = next_index;
					continue;
				}
			}
			"rec" => {
				if let Some((name, resolved, next_index)) = parse_record_binding(source, identifier.end, schema_catalog, active_databases) {
					if let Some(scope) = scopes.last_mut() {
						scope.insert(name, resolved);
					}
					index = next_index;
					continue;
				}
			}
			_ => {}
		}

		index = identifier.end;
	}

	let mut merged = std::collections::BTreeMap::new();
	for scope in scopes {
		for (name, resolved) in scope {
			merged.insert(name, resolved);
		}
	}
	merged
}

fn extract_member_receiver(source: &str) -> Option<String> {
	let trimmed = source.trim_end();
	let before_dot = trimmed.strip_suffix('.')?;
	let start = before_dot.char_indices()
		.rev()
		.find(|(_, ch)| !is_identifier_char(*ch))
		.map_or(0, |(index, ch)| index + ch.len_utf8());

	if start >= before_dot.len() {
		return None;
	}

	let identifier = read_identifier(before_dot, start)?;
	(identifier.end == before_dot.len()).then_some(identifier.value)
}

fn parse_for_record_binding<'a>(
	source: &str,
	start: usize,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<(String, ResolvedTable<'a>, usize)> {
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
	let resolved = resolve_record_pointer_expression(
		source[expression_start..expression_end].trim(),
		schema_catalog,
		active_databases,
	)?;

	Some((name.value, resolved, expression_end))
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

fn parse_function_record_pointer_bindings<'a>(
	source: &str,
	start: usize,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<(std::collections::BTreeMap<String, ResolvedTable<'a>>, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	let mut index = skip_whitespace(source, name.end);
	if source[index..].chars().next()? != '(' {
		return None;
	}

	let end_index = find_matching_paren(source, index)?;
	let parameters = &source[index + 1..end_index];
	let bindings = collect_function_parameter_record_pointer_bindings(parameters, schema_catalog, active_databases);
	index = end_index + 1;
	Some((bindings, index))
}

fn parse_if_record_binding<'a>(
	source: &str,
	start: usize,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<(String, ResolvedTable<'a>, usize)> {
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
	let resolved = resolve_record_pointer_expression(
		source[expression_start..expression_end].trim(),
		schema_catalog,
		active_databases,
	)?;

	Some((name.value, resolved, expression_end))
}

fn parse_named_declaration(source: &str, start: usize) -> Option<(String, usize)> {
	let index = skip_whitespace(source, start);
	let name = read_identifier(source, index)?;
	Some((name.value, name.end))
}

fn parse_record_binding<'a>(
	source: &str,
	start: usize,
	schema_catalog: &'a SchemaCatalog,
	active_databases: &[String],
) -> Option<(String, ResolvedTable<'a>, usize)> {
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
	let resolved = resolve_record_pointer_expression(
		source[expression_start..expression_end].trim(),
		schema_catalog,
		active_databases,
	)?;

	Some((name.value, resolved, expression_end))
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
