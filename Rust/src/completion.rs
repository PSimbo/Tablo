use crate::builtins::BuiltInFunction;
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

#[cfg(test)]
mod tests {
	use super::collect_document_completion_items;
	use super::default_completion_items;

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
}
