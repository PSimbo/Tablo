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

	pub fn to_packed(self) -> Result<u32, String> {
		if self.major > u8::MAX as u64 || self.minor > u8::MAX as u64 || self.patch > u16::MAX as u64 {
			return Err(format!("Tablo version {self} exceeds the object header limits (255.255.65535)."));
		}
		Ok((self.major as u32) << 24 | (self.minor as u32) << 16 | self.patch as u32)
	}

	pub fn from_packed(version: u32) -> Self {
		Self { major: (version >> 24) as u64, minor: ((version >> 16) & 0xff) as u64, patch: (version & 0xffff) as u64 }
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

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn packs_version_components_without_truncation() {
		let version = TabloVersion { major: 0x12, minor: 0x34, patch: 0x5678 };
		assert_eq!(version.to_packed().unwrap(), 0x12345678);
		assert_eq!(version.to_packed().unwrap().to_le_bytes(), [0x78, 0x56, 0x34, 0x12]);
		assert_eq!(TabloVersion::from_packed(0x12345678), version);
		assert_eq!(TabloVersion::parse("255.255.65535").unwrap().to_packed().unwrap(), u32::MAX);
		for version in ["256.0.0", "0.256.0", "0.0.65536"] {
			assert!(TabloVersion::parse(version).unwrap().to_packed().is_err());
		}
	}
}
