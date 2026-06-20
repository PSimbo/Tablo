use crate::TabloError;
use crate::semantic::ssa::LocalDeclarationKind;
use crate::semantic::ssa::ProgramLocalUsage;
use crate::source::SourceText;
use crate::source::local_name_span;
use crate::source::token_span_at_position;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Diagnostic {
	pub message: String,
	pub range: DiagnosticRange,
	pub severity: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticPosition {
	pub character: u32,
	pub line: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticRange {
	pub start: DiagnosticPosition,
	pub end: DiagnosticPosition,
}

pub fn diagnostic_at_start(source: &str, severity: u32, message: &str) -> Diagnostic {
	let source = SourceText::new(source);
	diagnostic_at_position(&source, 0, severity, message)
}

pub fn diagnostic_for_tablo_error(source: &str, error: TabloError) -> Diagnostic {
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
					source.offset_from_line_and_column(line, column)
				})
				.unwrap_or(0),
			error.message,
		),
	};

	if let Some((start, end)) = token_span_at_position(&source, position) {
		diagnostic_for_byte_span(&source, start, end, 1, &message)
	}
	else {
		diagnostic_at_position(&source, position, 1, &message)
	}
}

pub fn trailing_whitespace_diagnostics(source: &str) -> Vec<Diagnostic> {
	let mut diagnostics = Vec::new();

	for (line_index, line) in source.split('\n').enumerate() {
		let visible_line = line.strip_suffix('\r').unwrap_or(line);
		let trimmed = visible_line.trim_end_matches([' ', '\t']);

		if trimmed.len() == visible_line.len() {
			continue;
		}

		let start_column = trimmed.chars().count() as u32;
		let end_column = visible_line.chars().count() as u32;
		diagnostics.push(diagnostic_for_columns(
			line_index as u32,
			start_column,
			end_column,
			2,
			"Line has trailing whitespace.",
		));
	}

	diagnostics
}

pub fn unused_variable_diagnostics(source: &str, local_usage: &ProgramLocalUsage) -> Vec<Diagnostic> {
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

			if let Some((start, end)) = local_name_span(&source, local.declaration.position, &local.declaration.name) {
				diagnostics.push(diagnostic_for_byte_span(
					&source,
					start,
					end,
					2,
					&message,
				));
			}
			else {
				diagnostics.push(diagnostic_at_position(
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

fn diagnostic_at_position(source: &SourceText, position: usize, severity: u32, message: &str) -> Diagnostic {
	let (line, column) = source.line_and_column(position);
	let line_index = line.saturating_sub(1) as u32;
	let column_index = column.saturating_sub(1) as u32;

	Diagnostic {
		message: String::from(message),
		range: DiagnosticRange {
			start: DiagnosticPosition {
				line: line_index,
				character: column_index,
			},
			end: DiagnosticPosition {
				line: line_index,
				character: column_index + 1,
			},
		},
		severity,
	}
}

fn diagnostic_for_byte_span(
	source: &SourceText,
	start: usize,
	end: usize,
	severity: u32,
	message: &str,
) -> Diagnostic {
	let (start_line, start_column) = source.line_and_column(start);
	let (end_line, end_column) = source.line_and_column(end);

	Diagnostic {
		message: String::from(message),
		range: DiagnosticRange {
			start: DiagnosticPosition {
				line: start_line.saturating_sub(1) as u32,
				character: start_column.saturating_sub(1) as u32,
			},
			end: DiagnosticPosition {
				line: end_line.saturating_sub(1) as u32,
				character: end_column.saturating_sub(1) as u32,
			},
		},
		severity,
	}
}

fn diagnostic_for_columns(
	line_index: u32,
	start_column: u32,
	end_column: u32,
	severity: u32,
	message: &str,
) -> Diagnostic {
	Diagnostic {
		message: String::from(message),
		range: DiagnosticRange {
			start: DiagnosticPosition {
				line: line_index,
				character: start_column,
			},
			end: DiagnosticPosition {
				line: line_index,
				character: end_column,
			},
		},
		severity,
	}
}

#[cfg(test)]
mod tests {
	use crate::TabloError;
	use crate::compiler::CompileError;

	use super::diagnostic_for_tablo_error;
	use super::trailing_whitespace_diagnostics;

	#[test]
	fn highlights_full_span_for_assignment_type_error_on_string_literal() {
		let source = "fn Main(args: [text]) int { var x: int = 'abc'; return 0; }";
		let diagnostic = diagnostic_for_tablo_error(
			source,
			TabloError::Compile(CompileError {
				message: String::from("Cannot assign a value of type `text` to a variable of type `int`."),
				position: 41,
			}),
		);

		assert_eq!(diagnostic.range.start.character, 41);
		assert_eq!(diagnostic.range.end.character, 46);
		assert_eq!(diagnostic.severity, 1);
	}

	#[test]
	fn reports_trailing_whitespace() {
		let diagnostics = trailing_whitespace_diagnostics("abc  \nxyz");

		assert_eq!(diagnostics.len(), 1);
		assert_eq!(diagnostics[0].range.start.line, 0);
		assert_eq!(diagnostics[0].range.start.character, 3);
		assert_eq!(diagnostics[0].range.end.character, 5);
	}
}
