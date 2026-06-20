use std::path::PathBuf;

pub fn file_path_from_document_uri(uri: &str) -> Option<PathBuf> {
	let path = uri.strip_prefix("file://")?;

	if cfg!(windows) {
		Some(PathBuf::from(path.trim_start_matches('/')))
	}
	else {
		Some(PathBuf::from(path))
	}
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;

	use super::file_path_from_document_uri;

	#[test]
	fn resolves_file_path_from_document_uri() {
		assert_eq!(
			file_path_from_document_uri("file:///tmp/example.tablo"),
			Some(PathBuf::from("/tmp/example.tablo")),
		);
	}
}
