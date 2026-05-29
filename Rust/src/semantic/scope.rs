use std::collections::BTreeMap;

// This abstraction manages lexical scope entry/exit together with shadowing.
// It is intentionally small today, but it gives later semantic-analysis work
// a better foundation than manipulating parallel collections directly.
pub struct ScopeStack<T> {
	bindings: BTreeMap<String, Vec<T>>,
	scope_stack: Vec<Vec<String>>,
}

impl<T> Default for ScopeStack<T> {
	fn default() -> Self {
		Self {
			bindings: BTreeMap::new(),
			scope_stack: Vec::new(),
		}
	}
}

impl<T> ScopeStack<T> {
	pub fn contains_in_current_scope(&self, name: &str) -> bool {
		let Some(scope) = self.scope_stack.last() else {
			return false;
		};

		scope.iter().any(|declared_name| declared_name == name)
	}

	pub fn declare(&mut self, name: String, binding: T) {
		self.bindings.entry(name.clone()).or_default().push(binding);

		if let Some(scope) = self.scope_stack.last_mut() {
			scope.push(name);
		}
	}

	pub fn enter_scope(&mut self) {
		self.scope_stack.push(Vec::new());
	}

	pub fn exit_scope(&mut self) {
		let Some(scope) = self.scope_stack.pop() else {
			return;
		};

		for name in scope.into_iter().rev() {
			let should_remove = if let Some(bindings) = self.bindings.get_mut(&name) {
				bindings.pop();
				bindings.is_empty()
			}
			else {
				false
			};

			if should_remove {
				self.bindings.remove(&name);
			}
		}
	}

	pub fn lookup(&self, name: &str) -> Option<&T> {
		self.bindings.get(name).and_then(|bindings| bindings.last())
	}
}

#[cfg(test)]
mod tests {
	use super::ScopeStack;

	#[test]
	fn handles_shadowing_across_nested_scopes() {
		let mut scopes = ScopeStack::default();

		scopes.enter_scope();
		scopes.declare(String::from("x"), 1);
		assert_eq!(scopes.lookup("x"), Some(&1));

		scopes.enter_scope();
		scopes.declare(String::from("x"), 2);
		assert_eq!(scopes.lookup("x"), Some(&2));

		scopes.exit_scope();
		assert_eq!(scopes.lookup("x"), Some(&1));
	}

	#[test]
	fn tracks_current_scope_membership_only() {
		let mut scopes = ScopeStack::default();

		scopes.enter_scope();
		scopes.declare(String::from("x"), 1);
		assert!(scopes.contains_in_current_scope("x"));

		scopes.enter_scope();
		assert!(!scopes.contains_in_current_scope("x"));
		scopes.declare(String::from("y"), 2);
		assert!(scopes.contains_in_current_scope("y"));
	}
}
