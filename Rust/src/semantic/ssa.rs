use std::collections::BTreeMap;
use std::collections::BTreeSet;

use crate::ast::AssignmentOperator;
use crate::ast::AssignmentTarget;
use crate::ast::BlockStatement;
use crate::ast::Expr;
use crate::ast::ForRecordStatement;
use crate::ast::ForStatement;
use crate::ast::FunctionDeclaration;
use crate::ast::FunctionParameter;
use crate::ast::IfCondition;
use crate::ast::IdentifierExpr;
use crate::ast::Program;
use crate::ast::RecordPointerDeclaration;
use crate::ast::Statement;
use crate::ast::VariableDeclaration;
use crate::ast::WhileStatement;

use super::analyzer::SemanticProgram;

pub type SsaBlockId = usize;
pub type SsaVersionId = usize;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LocalDeclarationKind {
	ForIterator,
	ForRecordIterator,
	IfRecordBinding,
	Parameter,
	RecordPointerVariable,
	Variable,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PendingOperation {
	Read {
		position: usize,
		slot: u32,
	},
	Write {
		kind: SsaWriteKind,
		position: usize,
		slot: u32,
		version: SsaVersionId,
	},
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SsaOperation {
	Read {
		position: usize,
		slot: u32,
		version: Option<SsaVersionId>,
	},
	Write {
		kind: SsaWriteKind,
		position: usize,
		previous_version: Option<SsaVersionId>,
		slot: u32,
		version: SsaVersionId,
	},
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SsaTerminator {
	Branch {
		else_block: SsaBlockId,
		position: usize,
		then_block: SsaBlockId,
	},
	End,
	Jump {
		target: SsaBlockId,
	},
	Return {
		position: usize,
	},
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SsaWriteKind {
	Assignment,
	ByRefCall,
	Declaration,
	IfBinding,
	LoopBinding,
	Mutation,
	Parameter,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct FunctionLocalUsage {
	pub function_name: String,
	pub locals: Vec<LocalUsage>,
}

impl FunctionLocalUsage {
	pub fn local_by_name(&self, name: &str) -> Option<&LocalUsage> {
		self.locals.iter().find(|local| local.declaration.name == name)
	}
}

struct FunctionSsaBuilder<'a> {
	blocks: Vec<PendingBlock>,
	function: &'a FunctionDeclaration,
	next_version_id: SsaVersionId,
	semantic_program: &'a SemanticProgram,
}

impl<'a> FunctionSsaBuilder<'a> {
	fn allocate_version(&mut self) -> SsaVersionId {
		let version = self.next_version_id;
		self.next_version_id += 1;
		version
	}

	fn append_predecessor(&mut self, block_id: SsaBlockId, predecessor: SsaBlockId) {
		if !self.blocks[block_id].predecessors.contains(&predecessor) {
			self.blocks[block_id].predecessors.push(predecessor);
		}
	}

	fn build(function: &'a FunctionDeclaration, semantic_program: &'a SemanticProgram) -> SsaFunction {
		let mut builder = Self {
			blocks: vec![PendingBlock::default()],
			function,
			next_version_id: 0,
			semantic_program,
		};

		for parameter in &function.parameters {
			if let Some(slot) = builder.semantic_program.declaration_slot(parameter.position) {
				builder.push_write(0, slot, parameter.position, SsaWriteKind::Parameter);
			}
		}

		let exit_block = builder.lower_statement_list(&function.body.statements, Some(0), None);

		if let Some(block_id) = exit_block
			&& builder.blocks[block_id].terminator.is_none() {
			builder.blocks[block_id].terminator = Some(SsaTerminator::End);
		}

		builder.finalize()
	}

	fn collect_local_slots(
		pending_blocks: &[PendingBlock],
		phi_nodes: &[BTreeMap<u32, PendingPhiNode>],
	) -> BTreeSet<u32> {
		let mut local_slots = BTreeSet::new();

		for block in pending_blocks {
			for operation in &block.operations {
				match operation {
					PendingOperation::Read { slot, .. } | PendingOperation::Write { slot, .. } => {
						local_slots.insert(*slot);
					}
				}
			}
		}

		for block_phis in phi_nodes {
			for slot in block_phis.keys() {
				local_slots.insert(*slot);
			}
		}

		local_slots
	}

	fn ensure_block(&mut self) -> SsaBlockId {
		let id = self.blocks.len();
		self.blocks.push(PendingBlock::default());
		id
	}

	fn finalize(mut self) -> SsaFunction {
		for block in &mut self.blocks {
			if block.terminator.is_none() {
				block.terminator = Some(SsaTerminator::End);
			}
		}

		let block_count = self.blocks.len();
		let mut phi_nodes: Vec<BTreeMap<u32, PendingPhiNode>> = vec![BTreeMap::new(); block_count];
		let mut input_versions: Vec<BTreeMap<u32, SsaVersionId>> = vec![BTreeMap::new(); block_count];
		let mut output_versions: Vec<BTreeMap<u32, SsaVersionId>> = vec![BTreeMap::new(); block_count];

		loop {
			let mut changed = false;

			for block_id in 0..block_count {
				let predecessors = self.blocks[block_id].predecessors.clone();
				let mut merged_inputs = BTreeMap::new();
				let mut required_phis = BTreeMap::new();
				let mut referenced_slots = BTreeSet::new();

				for predecessor in &predecessors {
					for slot in output_versions[*predecessor].keys() {
						referenced_slots.insert(*slot);
					}
				}

				for slot in referenced_slots {
					let mut inputs = BTreeMap::new();

					for predecessor in &predecessors {
						if let Some(version) = output_versions[*predecessor].get(&slot) {
							inputs.insert(*predecessor, *version);
						}
					}

					if inputs.is_empty() {
						continue;
					}

					let first_version = *inputs.values().next().unwrap();
					let requires_phi = inputs.len() != predecessors.len()
						|| inputs.values().any(|version| *version != first_version);

					if requires_phi {
						let phi = phi_nodes[block_id].entry(slot).or_insert_with(|| PendingPhiNode {
							inputs: BTreeMap::new(),
							slot,
							version: self.allocate_version(),
						});
						phi.inputs = inputs.clone();
						required_phis.insert(slot, phi.clone());
						merged_inputs.insert(slot, phi.version);
					}
					else {
						phi_nodes[block_id].remove(&slot);
						merged_inputs.insert(slot, first_version);
					}
				}

				if phi_nodes[block_id] != required_phis {
					phi_nodes[block_id] = required_phis;
					changed = true;
				}

				if input_versions[block_id] != merged_inputs {
					input_versions[block_id] = merged_inputs.clone();
					changed = true;
				}

				let mut current_versions = merged_inputs;

				for operation in &self.blocks[block_id].operations {
					if let PendingOperation::Write { slot, version, .. } = operation {
						current_versions.insert(*slot, *version);
					}
				}

				if output_versions[block_id] != current_versions {
					output_versions[block_id] = current_versions;
					changed = true;
				}
			}

			if !changed {
				break;
			}
		}

		let mut blocks = Vec::with_capacity(block_count);

		for block_id in 0..block_count {
			let pending_block = &self.blocks[block_id];
			let mut operations = Vec::with_capacity(pending_block.operations.len());
			let mut current_versions = input_versions[block_id].clone();
			let mut phi_block_nodes = phi_nodes[block_id].values().cloned().collect::<Vec<_>>();
			phi_block_nodes.sort_by_key(|phi| phi.slot);

			for operation in &pending_block.operations {
				match operation {
					PendingOperation::Read { position, slot } => {
						operations.push(SsaOperation::Read {
							position: *position,
							slot: *slot,
							version: current_versions.get(slot).copied(),
						});
					}
					PendingOperation::Write {
						kind,
						position,
						slot,
						version,
					} => {
						let previous_version = current_versions.get(slot).copied();
						operations.push(SsaOperation::Write {
							kind: *kind,
							position: *position,
							slot: *slot,
							previous_version,
							version: *version,
						});
						current_versions.insert(*slot, *version);
					}
				}
			}

			blocks.push(SsaBlock {
				id: block_id,
				operations,
				phi_nodes: phi_block_nodes.into_iter().map(|phi| SsaPhiNode {
					inputs: phi.inputs,
					slot: phi.slot,
					version: phi.version,
				}).collect(),
				predecessors: pending_block.predecessors.clone(),
				terminator: pending_block.terminator.clone().unwrap_or(SsaTerminator::End),
			});
		}

		SsaFunction {
			blocks,
			entry_block: 0,
			local_slots: Self::collect_local_slots(&self.blocks, &phi_nodes),
			name: self.function.name.clone(),
		}
	}

	fn lower_assignment_expression(&mut self, block_id: SsaBlockId, expression: &crate::ast::AssignmentExpr) {
		self.lower_expression(block_id, &expression.value);

		match &expression.target {
			AssignmentTarget::Identifier(identifier) => {
				if let Some(slot) = self.semantic_program.identifier_slot(identifier.position) {
					if expression.operator != AssignmentOperator::Assign {
						self.push_read(block_id, slot, identifier.position);
					}

					self.push_write(block_id, slot, expression.position, SsaWriteKind::Assignment);
				}
			}
			AssignmentTarget::Field(target) => {
				if let Some(slot) = self.semantic_program.identifier_slot(target.object.position) {
					self.push_read(block_id, slot, target.object.position);
					self.push_write(block_id, slot, expression.position, SsaWriteKind::Mutation);
				}
			}
			AssignmentTarget::Index(target) => {
				self.lower_expression(block_id, &target.index);

				if let Some(slot) = self.semantic_program.identifier_slot(target.array.position) {
					self.push_read(block_id, slot, target.array.position);
					self.push_write(block_id, slot, expression.position, SsaWriteKind::Mutation);
				}
			}
		}
	}

	fn lower_block_statement(
		&mut self,
		block: &BlockStatement,
		current_block: Option<SsaBlockId>,
		loop_targets: Option<LoopTargets>,
	) -> Option<SsaBlockId> {
		self.lower_statement_list(&block.statements, current_block, loop_targets)
	}

	fn lower_expression(&mut self, block_id: SsaBlockId, expression: &Expr) {
		match expression {
			Expr::Array(array) => {
				for element in &array.elements {
					self.lower_expression(block_id, element);
				}
			}
			Expr::Assignment(assignment) => self.lower_assignment_expression(block_id, assignment),
			Expr::Binary(binary) => {
				self.lower_expression(block_id, &binary.left);
				self.lower_expression(block_id, &binary.right);
			}
			Expr::Boolean(_)
			| Expr::Date(_)
			| Expr::Decimal(_)
			| Expr::Integer(_)
			| Expr::Null(_)
			| Expr::Text(_)
			| Expr::Time(_)
			| Expr::TimeTz(_)
			| Expr::Timestamp(_)
			| Expr::TimestampTz(_) => {}
			Expr::Call(call) => {
				for argument in &call.arguments {
					self.lower_expression(block_id, &argument.value);
				}

				if let Some(reference_slots) = self.semantic_program.call_argument_reference_slots(call.position) {
					for slot in reference_slots.iter().flatten() {
						self.push_write(block_id, *slot, call.position, SsaWriteKind::ByRefCall);
					}
				}
			}
			Expr::Count(count) => {
				if let Some(where_clause) = &count.where_clause {
					self.lower_expression(block_id, where_clause);
				}
			}
			Expr::FieldAccess(field_access) => {
				self.lower_expression(block_id, &field_access.object);
			}
			Expr::Find(find) => {
				if let Some(where_clause) = &find.where_clause {
					self.lower_expression(block_id, where_clause);
				}

				for item in &find.order_by {
					self.lower_expression(block_id, &item.expression);
				}
			}
			Expr::Identifier(identifier) => {
				if let Some(slot) = self.semantic_program.identifier_slot(identifier.position) {
					self.push_read(block_id, slot, identifier.position);
				}
			}
			Expr::Index(index) => {
				self.lower_expression(block_id, &index.array);
				self.lower_expression(block_id, &index.index);
			}
			Expr::New(_) => {}
			Expr::ObjectConstruction(construction) => {
				for field in &construction.fields {
					self.lower_expression(block_id, &field.value);
				}
			}
			Expr::Range(range) => {
				self.lower_expression(block_id, &range.start);
				self.lower_expression(block_id, &range.end);

				if let Some(step) = &range.step {
					self.lower_expression(block_id, step);
				}
			}
			Expr::Ternary(ternary) => {
				self.lower_expression(block_id, &ternary.condition);
				self.lower_expression(block_id, &ternary.true_branch);
				self.lower_expression(block_id, &ternary.false_branch);
			}
			Expr::Unary(unary) => {
				self.lower_expression(block_id, &unary.operand);
			}
		}
	}

	fn lower_for_record_statement(
		&mut self,
		block_id: SsaBlockId,
		for_statement: &ForRecordStatement,
	) -> Option<SsaBlockId> {
		let loop_header = self.ensure_block();
		let body_block = self.ensure_block();
		let exit_block = self.ensure_block();

		self.blocks[block_id].terminator = Some(SsaTerminator::Jump {
			target: loop_header,
		});
		self.append_predecessor(loop_header, block_id);

		if let Some(where_clause) = &for_statement.where_clause {
			self.lower_expression(loop_header, where_clause);
		}

		for item in &for_statement.order_by {
			self.lower_expression(loop_header, &item.expression);
		}

		self.blocks[loop_header].terminator = Some(SsaTerminator::Branch {
			else_block: exit_block,
			position: for_statement.position,
			then_block: body_block,
		});
		self.append_predecessor(body_block, loop_header);
		self.append_predecessor(exit_block, loop_header);

		if let Some(slot) = self.semantic_program.declaration_slot(for_statement.variable.position) {
			self.push_write(body_block, slot, for_statement.variable.position, SsaWriteKind::LoopBinding);
		}

		let body_exit = self.lower_block_statement(
			&for_statement.body,
			Some(body_block),
			Some(LoopTargets {
				break_target: exit_block,
				continue_target: loop_header,
			}),
		);

		if let Some(body_exit) = body_exit {
			self.blocks[body_exit].terminator = Some(SsaTerminator::Jump {
				target: loop_header,
			});
			self.append_predecessor(loop_header, body_exit);
		}

		Some(exit_block)
	}

	fn lower_for_statement(&mut self, block_id: SsaBlockId, for_statement: &ForStatement) -> Option<SsaBlockId> {
		let loop_header = self.ensure_block();
		let body_block = self.ensure_block();
		let exit_block = self.ensure_block();

		self.blocks[block_id].terminator = Some(SsaTerminator::Jump {
			target: loop_header,
		});
		self.append_predecessor(loop_header, block_id);
		self.lower_expression(loop_header, &for_statement.iterable);
		self.blocks[loop_header].terminator = Some(SsaTerminator::Branch {
			else_block: exit_block,
			position: for_statement.position,
			then_block: body_block,
		});
		self.append_predecessor(body_block, loop_header);
		self.append_predecessor(exit_block, loop_header);

		if let Some(slot) = self.semantic_program.declaration_slot(for_statement.variable.position) {
			self.push_write(body_block, slot, for_statement.variable.position, SsaWriteKind::LoopBinding);
		}

		let body_exit = self.lower_block_statement(
			&for_statement.body,
			Some(body_block),
			Some(LoopTargets {
				break_target: exit_block,
				continue_target: loop_header,
			}),
		);

		if let Some(body_exit) = body_exit {
			self.blocks[body_exit].terminator = Some(SsaTerminator::Jump {
				target: loop_header,
			});
			self.append_predecessor(loop_header, body_exit);
		}

		Some(exit_block)
	}

	fn lower_if_statement(&mut self, block_id: SsaBlockId, if_statement: &crate::ast::IfStatement, loop_targets: Option<LoopTargets>) -> Option<SsaBlockId> {
		match &if_statement.condition {
			IfCondition::Expression(condition) => {
				self.lower_expression(block_id, condition);
			}
			IfCondition::RecordPointerBinding(binding) => {
				self.lower_expression(block_id, &binding.initial_value);
			}
		}

		let then_block = self.ensure_block();
		let else_block = self.ensure_block();
		self.blocks[block_id].terminator = Some(SsaTerminator::Branch {
			else_block,
			position: if_statement.position,
			then_block,
		});
		self.append_predecessor(then_block, block_id);
		self.append_predecessor(else_block, block_id);

		if let IfCondition::RecordPointerBinding(binding) = &if_statement.condition
			&& let Some(slot) = self.semantic_program.declaration_slot(binding.position) {
			self.push_write(then_block, slot, binding.position, SsaWriteKind::IfBinding);
		}

		let then_exit = self.lower_block_statement(&if_statement.then_branch, Some(then_block), loop_targets);
		let else_exit = if let Some(else_branch) = &if_statement.else_branch {
			self.lower_statement(else_branch, Some(else_block), loop_targets)
		}
		else {
			Some(else_block)
		};

		match (then_exit, else_exit) {
			(None, None) => None,
			(Some(block), None) | (None, Some(block)) => Some(block),
			(Some(then_exit), Some(else_exit)) => {
				let join_block = self.ensure_block();
				self.blocks[then_exit].terminator = Some(SsaTerminator::Jump {
					target: join_block,
				});
				self.blocks[else_exit].terminator = Some(SsaTerminator::Jump {
					target: join_block,
				});
				self.append_predecessor(join_block, then_exit);
				self.append_predecessor(join_block, else_exit);
				Some(join_block)
			}
		}
	}

	fn lower_record_pointer_declaration(&mut self, block_id: SsaBlockId, declaration: &RecordPointerDeclaration) {
		self.lower_expression(block_id, &declaration.initial_value);

		if let Some(slot) = self.semantic_program.declaration_slot(declaration.position) {
			self.push_write(block_id, slot, declaration.position, SsaWriteKind::Declaration);
		}
	}

	fn lower_statement(
		&mut self,
		statement: &Statement,
		current_block: Option<SsaBlockId>,
		loop_targets: Option<LoopTargets>,
	) -> Option<SsaBlockId> {
		let Some(block_id) = current_block else {
			return None;
		};

		match statement {
			Statement::Block(block) => self.lower_block_statement(block, Some(block_id), loop_targets),
			Statement::Break(_statement) => {
				let Some(loop_targets) = loop_targets else {
					return None;
				};

				self.blocks[block_id].terminator = Some(SsaTerminator::Jump {
					target: loop_targets.break_target,
				});
				self.append_predecessor(loop_targets.break_target, block_id);
				None
			}
			Statement::Continue(_statement) => {
				let Some(loop_targets) = loop_targets else {
					return None;
				};

				self.blocks[block_id].terminator = Some(SsaTerminator::Jump {
					target: loop_targets.continue_target,
				});
				self.append_predecessor(loop_targets.continue_target, block_id);
				None
			}
			Statement::Create(create) => {
				if let Some(slot) = self.semantic_program.identifier_slot(create.target.position) {
					self.push_read(block_id, slot, create.target.position);
				}

				Some(block_id)
			}
			Statement::EnumDeclaration(_) => Some(block_id),
			Statement::Expression(expression) => {
				self.lower_expression(block_id, expression);
				Some(block_id)
			}
			Statement::For(for_statement) => self.lower_for_statement(block_id, for_statement),
			Statement::ForRecord(for_statement) => self.lower_for_record_statement(block_id, for_statement),
			Statement::FunctionDeclaration(_) => Some(block_id),
			Statement::If(if_statement) => self.lower_if_statement(block_id, if_statement, loop_targets),
			Statement::RecordPointerDeclaration(declaration) => {
				self.lower_record_pointer_declaration(block_id, declaration);
				Some(block_id)
			}
			Statement::Return(return_statement) => {
				if let Some(value) = &return_statement.value {
					self.lower_expression(block_id, value);
				}

				self.blocks[block_id].terminator = Some(SsaTerminator::Return {
					position: return_statement.position,
				});
				None
			}
			Statement::Use(_) => Some(block_id),
			Statement::VariableDeclaration(declaration) => {
				if let Some(initial_value) = &declaration.initial_value {
					self.lower_expression(block_id, initial_value);
				}

				if let Some(slot) = self.semantic_program.declaration_slot(declaration.position) {
					self.push_write(block_id, slot, declaration.position, SsaWriteKind::Declaration);
				}

				Some(block_id)
			}
			Statement::While(while_statement) => self.lower_while_statement(block_id, while_statement),
		}
	}

	fn lower_statement_list(
		&mut self,
		statements: &[Statement],
		mut current_block: Option<SsaBlockId>,
		loop_targets: Option<LoopTargets>,
	) -> Option<SsaBlockId> {
		for statement in statements {
			current_block = self.lower_statement(statement, current_block, loop_targets);
		}

		current_block
	}

	fn lower_while_statement(&mut self, block_id: SsaBlockId, while_statement: &WhileStatement) -> Option<SsaBlockId> {
		let condition_block = self.ensure_block();
		let body_block = self.ensure_block();
		let exit_block = self.ensure_block();

		self.blocks[block_id].terminator = Some(SsaTerminator::Jump {
			target: condition_block,
		});
		self.append_predecessor(condition_block, block_id);
		self.lower_expression(condition_block, &while_statement.condition);
		self.blocks[condition_block].terminator = Some(SsaTerminator::Branch {
			else_block: exit_block,
			position: while_statement.position,
			then_block: body_block,
		});
		self.append_predecessor(body_block, condition_block);
		self.append_predecessor(exit_block, condition_block);

		let body_exit = self.lower_block_statement(
			&while_statement.body,
			Some(body_block),
			Some(LoopTargets {
				break_target: exit_block,
				continue_target: condition_block,
			}),
		);

		if let Some(body_exit) = body_exit {
			self.blocks[body_exit].terminator = Some(SsaTerminator::Jump {
				target: condition_block,
			});
			self.append_predecessor(condition_block, body_exit);
		}

		Some(exit_block)
	}

	fn push_read(&mut self, block_id: SsaBlockId, slot: u32, position: usize) {
		self.blocks[block_id].operations.push(PendingOperation::Read {
			position,
			slot,
		});
	}

	fn push_write(&mut self, block_id: SsaBlockId, slot: u32, position: usize, kind: SsaWriteKind) {
		let version = self.allocate_version();
		self.blocks[block_id].operations.push(PendingOperation::Write {
			kind,
			position,
			slot,
			version,
		});
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalDeclarationInfo {
	pub kind: LocalDeclarationKind,
	pub name: String,
	pub position: usize,
	pub slot: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalUsage {
	pub declaration: LocalDeclarationInfo,
	pub read_positions: Vec<usize>,
	pub write_positions: Vec<usize>,
}

impl LocalUsage {
	pub fn has_reads(&self) -> bool {
		!self.read_positions.is_empty()
	}

	pub fn has_writes_after_declaration(&self) -> bool {
		self.write_positions.iter().any(|position| *position != self.declaration.position)
	}

	pub fn is_never_read(&self) -> bool {
		self.read_positions.is_empty()
	}
}

#[derive(Clone, Copy)]
struct LoopTargets {
	break_target: SsaBlockId,
	continue_target: SsaBlockId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PendingBlock {
	operations: Vec<PendingOperation>,
	predecessors: Vec<SsaBlockId>,
	terminator: Option<SsaTerminator>,
}

impl Default for PendingBlock {
	fn default() -> Self {
		Self {
			operations: Vec::new(),
			predecessors: Vec::new(),
			terminator: None,
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct PendingPhiNode {
	inputs: BTreeMap<SsaBlockId, SsaVersionId>,
	slot: u32,
	version: SsaVersionId,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ProgramLocalUsage {
	pub functions: Vec<FunctionLocalUsage>,
}

impl ProgramLocalUsage {
	pub fn function(&self, name: &str) -> Option<&FunctionLocalUsage> {
		self.functions.iter().find(|function| function.function_name == name)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SsaBlock {
	pub id: SsaBlockId,
	pub operations: Vec<SsaOperation>,
	pub phi_nodes: Vec<SsaPhiNode>,
	pub predecessors: Vec<SsaBlockId>,
	pub terminator: SsaTerminator,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SsaFunction {
	pub blocks: Vec<SsaBlock>,
	pub entry_block: SsaBlockId,
	pub local_slots: BTreeSet<u32>,
	pub name: String,
}

impl SsaFunction {
	pub fn block(&self, id: SsaBlockId) -> Option<&SsaBlock> {
		self.blocks.get(id)
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SsaPhiNode {
	pub inputs: BTreeMap<SsaBlockId, SsaVersionId>,
	pub slot: u32,
	pub version: SsaVersionId,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SsaProgram {
	pub functions: Vec<SsaFunction>,
}

impl SsaProgram {
	pub fn function(&self, name: &str) -> Option<&SsaFunction> {
		self.functions.iter().find(|function| function.name == name)
	}
}

pub fn analyze_program(program: &Program, semantic_program: &SemanticProgram) -> SsaProgram {
	let mut functions = Vec::new();

	for function in &program.functions {
		collect_function_ssa(function, semantic_program, &mut functions);
	}

	for statement in &program.statements {
		collect_statement_function_ssa(statement, semantic_program, &mut functions);
	}

	SsaProgram {
		functions,
	}
}

pub fn analyze_program_local_usage(program: &Program, semantic_program: &SemanticProgram) -> ProgramLocalUsage {
	let ssa_program = analyze_program(program, semantic_program);
	let mut functions = Vec::new();

	for function in &program.functions {
		collect_function_local_usage(function, semantic_program, &ssa_program, &mut functions);
	}

	for statement in &program.statements {
		collect_statement_function_local_usage(statement, semantic_program, &ssa_program, &mut functions);
	}

	ProgramLocalUsage {
		functions,
	}
}

fn analyze_function_local_usage(
	function: &FunctionDeclaration,
	semantic_program: &SemanticProgram,
	ssa_program: &SsaProgram,
) -> FunctionLocalUsage {
	let mut locals = collect_function_local_declarations(function, semantic_program);
	let Some(ssa_function) = ssa_program.function(&function.name) else {
		locals.sort_by_key(|local| local.declaration.position);
		return FunctionLocalUsage {
			function_name: function.name.clone(),
			locals,
		};
	};
	let mut locals_by_slot = BTreeMap::new();

	for (index, local) in locals.iter().enumerate() {
		locals_by_slot.insert(local.declaration.slot, index);
	}

	for block in &ssa_function.blocks {
		for operation in &block.operations {
			match operation {
				SsaOperation::Read {
					position,
					slot,
					..
				} => {
					if let Some(local_index) = locals_by_slot.get(slot) {
						locals[*local_index].read_positions.push(*position);
					}
				}
				SsaOperation::Write {
					position,
					slot,
					..
				} => {
					if let Some(local_index) = locals_by_slot.get(slot) {
						locals[*local_index].write_positions.push(*position);
					}
				}
			}
		}
	}

	for local in &mut locals {
		local.read_positions.sort_unstable();
		local.read_positions.dedup();
		local.write_positions.sort_unstable();
		local.write_positions.dedup();
	}

	locals.sort_by_key(|local| local.declaration.position);

	FunctionLocalUsage {
		function_name: function.name.clone(),
		locals,
	}
}

fn collect_function_local_declarations(
	function: &FunctionDeclaration,
	semantic_program: &SemanticProgram,
) -> Vec<LocalUsage> {
	let mut locals = Vec::new();

	for parameter in &function.parameters {
		collect_parameter_declaration(parameter, semantic_program, &mut locals);
	}

	collect_statement_list_local_declarations(&function.body.statements, semantic_program, &mut locals);
	locals
}

fn collect_function_local_usage(
	function: &FunctionDeclaration,
	semantic_program: &SemanticProgram,
	ssa_program: &SsaProgram,
	functions: &mut Vec<FunctionLocalUsage>,
) {
	functions.push(analyze_function_local_usage(function, semantic_program, ssa_program));

	for statement in &function.body.statements {
		collect_statement_function_local_usage(statement, semantic_program, ssa_program, functions);
	}
}

fn collect_function_ssa(function: &FunctionDeclaration, semantic_program: &SemanticProgram, functions: &mut Vec<SsaFunction>) {
	functions.push(FunctionSsaBuilder::build(function, semantic_program));

	for statement in &function.body.statements {
		collect_statement_function_ssa(statement, semantic_program, functions);
	}
}

fn collect_identifier_declaration(
	identifier: &IdentifierExpr,
	kind: LocalDeclarationKind,
	semantic_program: &SemanticProgram,
	locals: &mut Vec<LocalUsage>,
) {
	let Some(slot) = semantic_program.declaration_slot(identifier.position) else {
		return;
	};

	locals.push(LocalUsage {
		declaration: LocalDeclarationInfo {
			kind,
			name: identifier.name.clone(),
			position: identifier.position,
			slot,
		},
		read_positions: Vec::new(),
		write_positions: Vec::new(),
	});
}

fn collect_parameter_declaration(
	parameter: &FunctionParameter,
	semantic_program: &SemanticProgram,
	locals: &mut Vec<LocalUsage>,
) {
	let Some(slot) = semantic_program.declaration_slot(parameter.position) else {
		return;
	};

	locals.push(LocalUsage {
		declaration: LocalDeclarationInfo {
			kind: LocalDeclarationKind::Parameter,
			name: parameter.name.clone(),
			position: parameter.position,
			slot,
		},
		read_positions: Vec::new(),
		write_positions: Vec::new(),
	});
}

fn collect_record_pointer_declaration(
	declaration: &RecordPointerDeclaration,
	semantic_program: &SemanticProgram,
	locals: &mut Vec<LocalUsage>,
) {
	let Some(slot) = semantic_program.declaration_slot(declaration.position) else {
		return;
	};

	locals.push(LocalUsage {
		declaration: LocalDeclarationInfo {
			kind: LocalDeclarationKind::RecordPointerVariable,
			name: declaration.name.clone(),
			position: declaration.position,
			slot,
		},
		read_positions: Vec::new(),
		write_positions: Vec::new(),
	});
}

fn collect_statement_function_local_usage(
	statement: &Statement,
	semantic_program: &SemanticProgram,
	ssa_program: &SsaProgram,
	functions: &mut Vec<FunctionLocalUsage>,
) {
	match statement {
		Statement::Block(block) => {
			for statement in &block.statements {
				collect_statement_function_local_usage(statement, semantic_program, ssa_program, functions);
			}
		}
		Statement::For(for_statement) => {
			for statement in &for_statement.body.statements {
				collect_statement_function_local_usage(statement, semantic_program, ssa_program, functions);
			}
		}
		Statement::ForRecord(for_statement) => {
			for statement in &for_statement.body.statements {
				collect_statement_function_local_usage(statement, semantic_program, ssa_program, functions);
			}
		}
		Statement::FunctionDeclaration(function) => {
			collect_function_local_usage(function, semantic_program, ssa_program, functions);
		}
		Statement::If(if_statement) => {
			for statement in &if_statement.then_branch.statements {
				collect_statement_function_local_usage(statement, semantic_program, ssa_program, functions);
			}

			if let Some(else_branch) = &if_statement.else_branch {
				collect_statement_function_local_usage(else_branch, semantic_program, ssa_program, functions);
			}
		}
		Statement::While(while_statement) => {
			for statement in &while_statement.body.statements {
				collect_statement_function_local_usage(statement, semantic_program, ssa_program, functions);
			}
		}
		Statement::Break(_)
		| Statement::Continue(_)
		| Statement::Create(_)
		| Statement::EnumDeclaration(_)
		| Statement::Expression(_)
		| Statement::RecordPointerDeclaration(_)
		| Statement::Return(_)
		| Statement::Use(_)
		| Statement::VariableDeclaration(_) => {}
	}
}

fn collect_statement_function_ssa(statement: &Statement, semantic_program: &SemanticProgram, functions: &mut Vec<SsaFunction>) {
	match statement {
		Statement::Block(block) => {
			for statement in &block.statements {
				collect_statement_function_ssa(statement, semantic_program, functions);
			}
		}
		Statement::For(for_statement) => {
			for statement in &for_statement.body.statements {
				collect_statement_function_ssa(statement, semantic_program, functions);
			}
		}
		Statement::ForRecord(for_statement) => {
			for statement in &for_statement.body.statements {
				collect_statement_function_ssa(statement, semantic_program, functions);
			}
		}
		Statement::FunctionDeclaration(function) => collect_function_ssa(function, semantic_program, functions),
		Statement::If(if_statement) => {
			for statement in &if_statement.then_branch.statements {
				collect_statement_function_ssa(statement, semantic_program, functions);
			}

			if let Some(else_branch) = &if_statement.else_branch {
				collect_statement_function_ssa(else_branch, semantic_program, functions);
			}
		}
		Statement::While(while_statement) => {
			for statement in &while_statement.body.statements {
				collect_statement_function_ssa(statement, semantic_program, functions);
			}
		}
		Statement::Break(_)
		| Statement::Continue(_)
		| Statement::Create(_)
		| Statement::EnumDeclaration(_)
		| Statement::Expression(_)
		| Statement::RecordPointerDeclaration(_)
		| Statement::Return(_)
		| Statement::Use(_)
		| Statement::VariableDeclaration(_) => {}
	}
}

fn collect_statement_list_local_declarations(
	statements: &[Statement],
	semantic_program: &SemanticProgram,
	locals: &mut Vec<LocalUsage>,
) {
	for statement in statements {
		collect_statement_local_declarations(statement, semantic_program, locals);
	}
}

fn collect_statement_local_declarations(
	statement: &Statement,
	semantic_program: &SemanticProgram,
	locals: &mut Vec<LocalUsage>,
) {
	match statement {
		Statement::Block(block) => {
			collect_statement_list_local_declarations(&block.statements, semantic_program, locals);
		}
		Statement::For(for_statement) => {
			collect_identifier_declaration(
				&for_statement.variable,
				LocalDeclarationKind::ForIterator,
				semantic_program,
				locals,
			);
			collect_statement_list_local_declarations(&for_statement.body.statements, semantic_program, locals);
		}
		Statement::ForRecord(for_statement) => {
			collect_identifier_declaration(
				&for_statement.variable,
				LocalDeclarationKind::ForRecordIterator,
				semantic_program,
				locals,
			);
			collect_statement_list_local_declarations(&for_statement.body.statements, semantic_program, locals);
		}
		Statement::FunctionDeclaration(_) => {}
		Statement::If(if_statement) => {
			if let IfCondition::RecordPointerBinding(binding) = &if_statement.condition {
				let identifier = IdentifierExpr {
					name: binding.name.clone(),
					position: binding.position,
				};
				collect_identifier_declaration(
					&identifier,
					LocalDeclarationKind::IfRecordBinding,
					semantic_program,
					locals,
				);
			}

			collect_statement_list_local_declarations(&if_statement.then_branch.statements, semantic_program, locals);

			if let Some(else_branch) = &if_statement.else_branch {
				collect_statement_local_declarations(else_branch, semantic_program, locals);
			}
		}
		Statement::RecordPointerDeclaration(declaration) => {
			collect_record_pointer_declaration(declaration, semantic_program, locals);
		}
		Statement::VariableDeclaration(declaration) => {
			collect_variable_declaration(declaration, semantic_program, locals);
		}
		Statement::While(while_statement) => {
			collect_statement_list_local_declarations(&while_statement.body.statements, semantic_program, locals);
		}
		Statement::Break(_)
		| Statement::Continue(_)
		| Statement::Create(_)
		| Statement::EnumDeclaration(_)
		| Statement::Expression(_)
		| Statement::Return(_)
		| Statement::Use(_) => {}
	}
}

fn collect_variable_declaration(
	declaration: &VariableDeclaration,
	semantic_program: &SemanticProgram,
	locals: &mut Vec<LocalUsage>,
) {
	let Some(slot) = semantic_program.declaration_slot(declaration.position) else {
		return;
	};

	locals.push(LocalUsage {
		declaration: LocalDeclarationInfo {
			kind: LocalDeclarationKind::Variable,
			name: declaration.name.clone(),
			position: declaration.position,
			slot,
		},
		read_positions: Vec::new(),
		write_positions: Vec::new(),
	});
}

#[cfg(test)]
mod tests {
	use crate::semantic::analyzer::SemanticAnalyzer;
	use crate::source::SourceText;
	use crate::syntax::lexer::Lexer;
	use crate::syntax::parser::Parser;

	use super::LocalDeclarationKind;
	use super::SsaOperation;
	use super::SsaWriteKind;
	use super::analyze_program;
	use super::analyze_program_local_usage;

	fn analyze_source(source: &str) -> super::SsaProgram {
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();
		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		analyze_program(&program, &semantic_program)
	}

	fn analyze_source_local_usage(source: &str) -> super::ProgramLocalUsage {
		let program = parse_program(source);
		let mut analyzer = SemanticAnalyzer::new();
		let semantic_program = analyzer.analyze_standalone_program(&program).unwrap();
		analyze_program_local_usage(&program, &semantic_program)
	}

	fn parse_program(source: &str) -> crate::ast::Program {
		let mut lexer = Lexer::new(SourceText::new(source));
		let tokens = lexer.tokenize().unwrap();
		let mut parser = Parser::new(tokens);
		parser.parse_program().unwrap()
	}

	#[test]
	fn builds_sequential_versions_for_reassignments() {
		let ssa = analyze_source(
			"fn Main(args: [text]) int { var x: int = 1; x = 2; return x; }"
		);
		let function = ssa.function("Main").unwrap();
		let block = function.block(function.entry_block).unwrap();
		let mut writes = block.operations.iter()
			.filter_map(|operation| match operation {
				SsaOperation::Write { slot, version, .. } => Some((*slot, *version)),
				_ => None,
			})
			.collect::<Vec<_>>();
		let reads = block.operations.iter()
			.filter_map(|operation| match operation {
				SsaOperation::Read { slot, version, .. } => Some((*slot, *version)),
				_ => None,
			})
			.collect::<Vec<_>>();

		assert!(writes.len() >= 3);
		let (_, first_x_write) = writes.remove(1);
		let (_, second_x_write) = writes.remove(1);
		let (_, return_read) = reads.last().copied().unwrap();

		assert_ne!(first_x_write, second_x_write);
		assert_eq!(return_read, Some(second_x_write));
	}

	#[test]
	fn inserts_phi_node_for_if_else_reassignment() {
		let ssa = analyze_source(
			"fn Main(args: [text]) int { var x: int = 1; if true { x = 2; } else { x = 3; } return x; }"
		);
		let function = ssa.function("Main").unwrap();
		let join_block = function.blocks.iter()
			.find(|block| !block.phi_nodes.is_empty())
			.unwrap();
		let phi = &join_block.phi_nodes[0];
		let read = join_block.operations.iter()
			.find_map(|operation| match operation {
				SsaOperation::Read { version, .. } => Some(*version),
				_ => None,
			})
			.unwrap();

		assert_eq!(phi.inputs.len(), 2);
		assert_eq!(read, Some(phi.version));
	}

	#[test]
	fn inserts_phi_node_for_loop_carried_assignment() {
		let ssa = analyze_source(
			"fn Main(args: [text]) int { var x: int = 0; while x < 10 { x += 1; } return x; }"
		);
		let function = ssa.function("Main").unwrap();
		let return_read = function.blocks.iter()
			.flat_map(|block| block.operations.iter())
			.rev()
			.find_map(|operation| match operation {
				SsaOperation::Read {
					slot,
					version: Some(version),
					..
				} => Some((*slot, *version)),
				_ => None,
			})
			.unwrap();
		let loop_phi = function.blocks.iter()
			.flat_map(|block| block.phi_nodes.iter())
			.find(|phi| phi.slot == return_read.0 && phi.version == return_read.1)
			.unwrap();

		assert_eq!(loop_phi.inputs.len(), 2);
		assert!(loop_phi.inputs.values().all(|version| *version != loop_phi.version));
		assert!(function.blocks.iter()
			.flat_map(|block| block.operations.iter())
			.any(|operation| match operation {
				SsaOperation::Write {
					kind: SsaWriteKind::Assignment,
					slot,
					version,
					..
				} => *slot == loop_phi.slot && loop_phi.inputs.values().any(|input| input == version),
				_ => false,
			})
		);
	}

	#[test]
	fn reports_assigned_but_never_read_local_variable() {
		let usage = analyze_source_local_usage(
			"fn Main(args: [text]) int { var x: int = 1; x = 2; return 0; }"
		);
		let function = usage.function("Main").unwrap();
		let x = function.local_by_name("x").unwrap();

		assert!(x.is_never_read());
		assert!(x.has_writes_after_declaration());
		assert_eq!(x.write_positions.len(), 2);
	}

	#[test]
	fn reports_read_local_variable() {
		let usage = analyze_source_local_usage(
			"fn Main(args: [text]) int { var x: int = 1; return x; }"
		);
		let function = usage.function("Main").unwrap();
		let x = function.local_by_name("x").unwrap();

		assert!(x.has_reads());
		assert!(!x.is_never_read());
	}

	#[test]
	fn reports_unused_local_variable() {
		let usage = analyze_source_local_usage(
			"fn Main(args: [text]) int { var x: int = 1; return 0; }"
		);
		let function = usage.function("Main").unwrap();
		let x = function.local_by_name("x").unwrap();

		assert_eq!(x.declaration.kind, LocalDeclarationKind::Variable);
		assert!(x.is_never_read());
		assert_eq!(x.write_positions.len(), 1);
	}
}
