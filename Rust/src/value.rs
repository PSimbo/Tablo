use std::fmt::Display;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
	Integer(i64),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Integer(value) => write!(f, "{value}"),
		}
	}
}
