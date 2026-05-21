#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceText {
	text: String,
}

impl SourceText {
	pub fn new(text: impl Into<String>) -> Self {
		Self {
			text: text.into(),
		}
	}

	pub fn as_str(&self) -> &str {
		&self.text
	}
}
