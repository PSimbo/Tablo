#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) enum TransactionCommand {
	Begin,
	Commit,
	ReleaseSavepoint(String),
	Rollback,
	RollbackToSavepoint(String),
	Savepoint(String),
}

#[derive(Default)]
pub(super) struct TransactionState {
	depth: usize,
}

impl TransactionState {
	pub fn commit(
		&mut self,
		target_depth: usize,
		savepoint_name: &str,
		mut execute: impl FnMut(TransactionCommand) -> Result<(), String>,
	) -> Result<(), String> {
		Self::require_positive_target_depth(target_depth)?;
		if self.depth < target_depth {
			return Ok(());
		}
		self.require_target_depth(target_depth)?;

		let command = if target_depth == 1 {
			TransactionCommand::Commit
		}
		else {
			TransactionCommand::ReleaseSavepoint(savepoint_name.to_string())
		};
		execute(command)?;
		self.depth -= 1;
		Ok(())
	}

	pub fn rollback(
		&mut self,
		target_depth: usize,
		savepoint_name: &str,
		mut execute: impl FnMut(TransactionCommand) -> Result<(), String>,
	) -> Result<(), String> {
		Self::require_positive_target_depth(target_depth)?;
		if self.depth < target_depth {
			return Ok(());
		}
		self.require_target_depth(target_depth)?;

		if target_depth == 1 {
			execute(TransactionCommand::Rollback)?;
		}
		else {
			execute(TransactionCommand::RollbackToSavepoint(savepoint_name.to_string()))?;
			execute(TransactionCommand::ReleaseSavepoint(savepoint_name.to_string()))?;
		}

		self.depth -= 1;
		Ok(())
	}

	pub fn synchronize(
		&mut self,
		transaction_names: &[String],
		mut execute: impl FnMut(TransactionCommand) -> Result<(), String>,
	) -> Result<(), String> {
		if self.depth > transaction_names.len() {
			return Err(format!(
				"Database transaction depth {} exceeds runtime transaction depth {}.",
				self.depth,
				transaction_names.len(),
			));
		}

		while self.depth < transaction_names.len() {
			let command = if self.depth == 0 {
				TransactionCommand::Begin
			}
			else {
				TransactionCommand::Savepoint(transaction_names[self.depth].clone())
			};
			execute(command)?;
			self.depth += 1;
		}

		Ok(())
	}

	fn require_positive_target_depth(target_depth: usize) -> Result<(), String> {
		if target_depth == 0 {
			Err(String::from("Database transaction target depth must be at least 1."))
		}
		else {
			Ok(())
		}
	}

	fn require_target_depth(&self, target_depth: usize) -> Result<(), String> {
		if self.depth == target_depth {
			Ok(())
		}
		else {
			Err(format!(
				"Database transaction depth {} does not match requested depth {target_depth}.",
				self.depth,
			))
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn transaction_names() -> Vec<String> {
		vec![String::from("outer"), String::from("inner")]
	}

	#[test]
	fn commits_nested_scopes_from_inner_to_outer() {
		let mut state = TransactionState::default();
		state.synchronize(&transaction_names(), |_| Ok(())).unwrap();
		let mut commands = Vec::new();

		state.commit(2, "inner", |command| {
			commands.push(command);
			Ok(())
		}).unwrap();
		state.commit(1, "outer", |command| {
			commands.push(command);
			Ok(())
		}).unwrap();

		assert_eq!(commands, vec![
			TransactionCommand::ReleaseSavepoint(String::from("inner")),
			TransactionCommand::Commit,
		]);
	}

	#[test]
	fn leaves_depth_unchanged_when_command_fails() {
		let mut state = TransactionState::default();
		let error = state.synchronize(&[String::from("outer")], |_| Err(String::from("failed"))).unwrap_err();

		assert_eq!(error, "failed");
		assert_eq!(state.depth, 0);
	}

	#[test]
	fn rejects_zero_target_depth() {
		let mut state = TransactionState::default();

		assert_eq!(
			state.commit(0, "", |_| Ok(())).unwrap_err(),
			"Database transaction target depth must be at least 1.",
		);
		assert_eq!(
			state.rollback(0, "", |_| Ok(())).unwrap_err(),
			"Database transaction target depth must be at least 1.",
		);
	}

	#[test]
	fn rolls_back_and_releases_nested_savepoint() {
		let mut state = TransactionState::default();
		state.synchronize(&transaction_names(), |_| Ok(())).unwrap();
		let mut commands = Vec::new();

		state.rollback(2, "inner", |command| {
			commands.push(command);
			Ok(())
		}).unwrap();
		state.rollback(1, "outer", |command| {
			commands.push(command);
			Ok(())
		}).unwrap();

		assert_eq!(commands, vec![
			TransactionCommand::RollbackToSavepoint(String::from("inner")),
			TransactionCommand::ReleaseSavepoint(String::from("inner")),
			TransactionCommand::Rollback,
		]);
	}

	#[test]
	fn synchronizes_late_session_through_all_active_scopes() {
		let mut state = TransactionState::default();
		let mut commands = Vec::new();

		state.synchronize(&transaction_names(), |command| {
			commands.push(command);
			Ok(())
		}).unwrap();

		assert_eq!(commands, vec![
			TransactionCommand::Begin,
			TransactionCommand::Savepoint(String::from("inner")),
		]);
	}
}
