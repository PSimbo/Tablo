use std::fmt::{Display, Formatter};

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TabloVersion {
	pub major: u64,
	pub minor: u64,
	pub patch: u64,
}

impl TabloVersion {
	pub fn parse(version: &str) -> Result<Self, String> {
		let parts: Vec<_> = version.split('.').collect();
		if parts.len() != 3 || parts.iter().any(|part| {
			part.is_empty() || !part.bytes().all(|byte| byte.is_ascii_digit()) || (part.len() > 1 && part.starts_with('0'))
		}) {
			return Err(format!("Expected a Tablo major.minor.patch version, got `{version}`."));
		}
		let number = |part: &str| part.parse::<u64>().map_err(|_| format!("Tablo version component is too large: `{part}`."));
		Ok(Self { major: number(parts[0])?, minor: number(parts[1])?, patch: number(parts[2])? })
	}

	pub fn current() -> Self {
		Self::parse(env!("CARGO_PKG_VERSION")).expect("The Tablo package must use a major.minor.patch version")
	}

	pub fn accepts(self, artifact: Self) -> bool {
		artifact == (Self { major: 0, minor: 0, patch: 0 })
			|| (self.major == artifact.major && self.minor == artifact.minor && self.patch >= artifact.patch)
	}
}

impl Display for TabloVersion {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
		write!(formatter, "{}.{}.{}", self.major, self.minor, self.patch)
	}
}

/// Borrowed UTF-8 bytes. The caller retains ownership for the duration of a call.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct RuntimeArgument {
	pub data: *const u8,
	pub len: usize,
}
