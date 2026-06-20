use std::path::Path;
use std::path::PathBuf;

pub fn canonicalize_or_original(path: &Path) -> PathBuf {
	std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

pub fn existing_child_path(directory: &Path, file_name: &str) -> Option<PathBuf> {
	let path = directory.join(file_name);

	if path.is_file() {
		Some(path)
	}
	else {
		None
	}
}

pub fn file_path_from_document_uri(uri: &str) -> Option<PathBuf> {
	let path = uri.strip_prefix("file://")?;

	if cfg!(windows) {
		Some(PathBuf::from(path.trim_start_matches('/')))
	}
	else {
		Some(PathBuf::from(path))
	}
}

pub fn unique_temp_directory(name: &str) -> PathBuf {
	let process_id = std::process::id();
	let nanos = std::time::SystemTime::now()
		.duration_since(std::time::UNIX_EPOCH)
		.unwrap()
		.as_nanos();

	std::env::temp_dir().join(format!("{name}_{process_id}_{nanos}"))
}

pub fn unique_temp_path(name: &str, extension: &str) -> PathBuf {
	let mut path = unique_temp_directory(name);
	path.set_extension(extension);
	path
}

#[cfg(test)]
mod tests {
	use std::path::Path;
	use std::path::PathBuf;

	use super::canonicalize_or_original;
	use super::existing_child_path;
	use super::file_path_from_document_uri;
	use super::unique_temp_directory;
	use super::unique_temp_path;

	#[test]
	fn builds_unique_temp_path_with_extension() {
		let path = unique_temp_path("utils_unique_temp_path", "toml");
		assert_eq!(path.extension().and_then(|value| value.to_str()), Some("toml"));
	}

	#[test]
	fn canonicalize_or_original_falls_back_to_original_path() {
		let path = Path::new("/definitely/not/real");
		assert_eq!(canonicalize_or_original(path), path);
	}

	#[test]
	fn finds_existing_child_path() {
		let temp_dir = unique_temp_directory("utils_existing_child_path");
		let child_path = temp_dir.join("tablo.toml");
		std::fs::create_dir_all(&temp_dir).unwrap();
		std::fs::write(&child_path, "[databases]\n").unwrap();

		assert_eq!(existing_child_path(&temp_dir, "tablo.toml"), Some(child_path.clone()));

		let _ = std::fs::remove_file(&child_path);
		let _ = std::fs::remove_dir(&temp_dir);
	}

	#[test]
	fn resolves_file_path_from_document_uri() {
		assert_eq!(
			file_path_from_document_uri("file:///tmp/example.tablo"),
			Some(PathBuf::from("/tmp/example.tablo")),
		);
	}
}
