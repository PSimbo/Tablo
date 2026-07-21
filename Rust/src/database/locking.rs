use std::borrow::Cow;

use crate::query::{ RecordLockMode, SqlDialect, SqlQuery, SqlQueryResultShape };
use crate::value::Value;

use super::records::locked_record_pointer;

pub(super) fn lock_conflict_result(query: &SqlQuery) -> Option<Value> {
	match &query.result_shape {
		SqlQueryResultShape::RecordPointer(columns) => {
			Some(Value::RecordPointer(locked_record_pointer(query, columns)))
		}
		_ => None,
	}
}

pub(super) fn query_statement(query: &SqlQuery, transaction_active: bool) -> Cow<'_, str> {
	if !transaction_active || query.dialect == SqlDialect::Sqlite {
		return Cow::Borrowed(&query.statement);
	}

	let suffix = match query.lock_mode {
		RecordLockMode::None => return Cow::Borrowed(&query.statement),
		RecordLockMode::Update => " FOR UPDATE",
		RecordLockMode::UpdateNoWait => " FOR UPDATE NOWAIT",
	};

	Cow::Owned(format!("{}{suffix}", query.statement))
}

#[cfg(test)]
mod tests {
	use super::*;

	use crate::query::*;

	fn test_query(dialect: SqlDialect, lock_mode: RecordLockMode) -> SqlQuery {
		SqlQuery {
			database_name: String::from("ExampleDb"),
			dialect,
			group_by: vec![],
			lock_mode,
			parameters: vec![],
			result_shape: SqlQueryResultShape::IntegerScalar,
			schema_is_implicit: true,
			schema_name: String::from("Main"),
			statement: String::from("SELECT 1"),
			table_name: String::new(),
		}
	}

	#[test]
	fn applies_non_waiting_update_lock_inside_mysql_transaction() {
		let query = test_query(SqlDialect::MySql, RecordLockMode::UpdateNoWait);

		assert_eq!(query_statement(&query, true), "SELECT 1 FOR UPDATE NOWAIT");
	}

	#[test]
	fn applies_waiting_update_lock_inside_postgresql_transaction() {
		let query = test_query(SqlDialect::PostgreSql, RecordLockMode::Update);

		assert_eq!(query_statement(&query, true), "SELECT 1 FOR UPDATE");
	}

	#[test]
	fn does_not_apply_lock_outside_transaction() {
		let query = test_query(SqlDialect::PostgreSql, RecordLockMode::UpdateNoWait);

		assert_eq!(query_statement(&query, false), "SELECT 1");
	}

	#[test]
	fn does_not_apply_record_lock_to_sqlite_query() {
		let query = test_query(SqlDialect::Sqlite, RecordLockMode::Update);

		assert_eq!(query_statement(&query, true), "SELECT 1");
	}

	#[test]
	fn represents_single_record_lock_conflict_as_locked_existing_pointer() {
		let mut query = test_query(SqlDialect::PostgreSql, RecordLockMode::UpdateNoWait);
		query.result_shape = SqlQueryResultShape::RecordPointer(vec![]);
		query.table_name = String::from("Customers");

		let Some(Value::RecordPointer(record)) = lock_conflict_result(&query) else {
			panic!("Expected locked record-pointer result.");
		};

		assert!(record.exists);
		assert!(record.locked);
	}
}
