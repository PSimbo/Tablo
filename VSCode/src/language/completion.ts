import * as vscode from "vscode";
import * as fs from "node:fs";
import * as path from "node:path";

type CompletionContextData = {
	activeDatabases: string[];
	enums: Map<string, string[]>;
	objects: Map<string, ObjectFieldInfo[]>;
	recordPointerTypes: Map<string, string>;
	schemaTables: SchemaTableInfo[];
	symbols: vscode.CompletionItem[];
	variableTypes: Map<string, string>;
};

type ObjectFieldInfo = {
	name: string;
	typeName?: string;
};

type SchemaFieldInfo = {
	name: string;
	typeName?: string;
};

type SchemaTableInfo = {
	databaseName: string;
	fields: SchemaFieldInfo[];
	implicitSchema: boolean;
	schemaName: string;
	tableName: string;
	typeName: string;
};

type VisibleBindings = Map<string, string>;

const IDENTIFIER_PATTERN = /"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*/y;
const QUALIFIED_MEMBER_PATTERN = /((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)(?:\.(?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*))*)\.((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)?)$/;
const FIND_TABLE_PATTERN = /^find(?:\s+(?:first|last))?\s+((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)(?:\.(?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*))*)/u;

const KEYWORD_ITEMS = [
	keyword("and"),
	keyword("asc"),
	keyword("break"),
	keyword("by"),
	keyword("const"),
	keyword("continue"),
	keyword("count"),
	keyword("desc"),
	keyword("else"),
	keyword("enum"),
	keyword("find"),
	keyword("first"),
	keyword("fn"),
	keyword("for"),
	keyword("if"),
	keyword("in"),
	keyword("last"),
	keyword("mut"),
	keyword("not"),
	keyword("obj"),
	keyword("or"),
	keyword("order"),
	keyword("rec"),
	keyword("return"),
	keyword("where"),
	keyword("while"),
	keyword("with"),
	keyword("xor"),
	literal("false"),
	literal("null"),
	literal("true"),
];

const TYPE_ITEMS = [
	typeItem("any"),
	typeItem("bool"),
	typeItem("date"),
	typeItem("dec"),
	typeItem("int"),
	typeItem("text"),
	typeItem("void"),
];

const BUILT_IN_ITEMS = [
	builtIn("bool", "Cast an enum to its `bool` backing value."),
	builtIn("date", "Cast an enum to its `date` backing value."),
	builtIn("dec", "Cast an enum to its `dec` backing value."),
	builtIn("disp", "Write text to stdout."),
	builtIn("displn", "Write text plus a trailing newline to stdout."),
	builtIn("exists", "Check whether a record pointer references a row."),
	builtIn("format", "Format an `int` or `dec` value as `text` according to a numeric format string."),
	builtIn("int", "Cast an enum to its `int` backing value."),
	builtIn("len", "Return the length of an array."),
	builtIn("locked", "Check whether a record pointer is locked."),
	builtIn("text", "Cast an enum to its `text` backing value."),
];

const SNIPPET_ITEMS = [
	snippet(
		"fn",
		"Function",
		"fn ${1:Name}(args: [text]) ${2:int} {\n\t$0\n}",
		"Insert a function declaration.",
	),
	snippet(
		"obj",
		"Object",
		"obj ${1:Name} {\n\t${2:Field}: ${3:text},\n};",
		"Insert an object declaration.",
	),
	snippet(
		"if",
		"If",
		"if ${1:condition} {\n\t$0\n}",
		"Insert an if statement.",
	),
	snippet(
		"for",
		"For",
		"for ${1:item} in ${2:iterable} {\n\t$0\n}",
		"Insert a for loop.",
	),
];

export function registerCompletionProvider(): vscode.Disposable {
	return vscode.languages.registerCompletionItemProvider(
		{ language: "tablo", scheme: "file" },
		{
			provideCompletionItems(document, position) {
				const completionContext = analyzeDocument(document);
				const memberItems = provideMemberCompletions(document, position, completionContext);

				if (memberItems) {
					return memberItems;
				}

				return [
					...SNIPPET_ITEMS,
					...KEYWORD_ITEMS,
					...TYPE_ITEMS,
					...BUILT_IN_ITEMS,
					...completionContext.symbols,
				];
			},
		},
		".",
		"_",
	);
}

function analyzeDocument(document: vscode.TextDocument): CompletionContextData {
	const text = document.getText();
	const enums = collectEnumDeclarations(text);
	const objects = collectObjectDeclarations(text);
	const activeDatabases = collectActiveDatabases(text);
	const schemaTables = loadSchemaTables(document);
	const variableTypes = collectVariableTypes(text);
	const recordPointerTypes = collectRecordPointerTypes(text, schemaTables, activeDatabases);

	for (const [name, typeName] of recordPointerTypes) {
		variableTypes.set(name, typeName);
	}
	const symbols = [
		...collectDocumentSymbols(text, "fn", vscode.CompletionItemKind.Function, "Function"),
		...collectDocumentSymbols(text, "obj", vscode.CompletionItemKind.Struct, "Object"),
		...collectDocumentSymbols(text, "enum", vscode.CompletionItemKind.Enum, "Enum"),
		...collectVariableSymbols(text),
	];

	return {
		activeDatabases,
		enums,
		objects,
		recordPointerTypes,
		schemaTables,
		symbols,
		variableTypes,
	};
}

function builtIn(name: string, documentation: string): vscode.CompletionItem {
	const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Function);
	item.detail = "Built-in function";
	item.documentation = new vscode.MarkdownString(documentation);
	return item;
}

function collectDocumentSymbols(
	text: string,
	keywordName: "enum" | "fn" | "obj",
	kind: vscode.CompletionItemKind,
	detail: string,
): vscode.CompletionItem[] {
	const regex = new RegExp(`\\b${keywordName}\\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)`, "g");
	const items = new Map<string, vscode.CompletionItem>();

	for (const match of text.matchAll(regex)) {
		const name = decodeIdentifier(match[1]);
		if (items.has(name)) {
			continue;
		}

		const item = new vscode.CompletionItem(name, kind);
		item.detail = detail;
		items.set(name, item);
	}

	return [...items.values()];
}

function collectEnumDeclarations(text: string): Map<string, string[]> {
	const enums = new Map<string, string[]>();

	for (const declaration of findBraceDeclarations(text, "enum")) {
		const variants = extractTopLevelMemberNames(declaration.body);
		enums.set(decodeIdentifier(declaration.name), variants);
	}

	return enums;
}

function collectObjectDeclarations(text: string): Map<string, ObjectFieldInfo[]> {
	const objects = new Map<string, ObjectFieldInfo[]>();

	for (const declaration of findBraceDeclarations(text, "obj")) {
		const typeName = decodeIdentifier(declaration.name);
		collectObjectFields(typeName, declaration.body, objects);
	}

	return objects;
}

function collectVariableSymbols(text: string): vscode.CompletionItem[] {
	const items = new Map<string, vscode.CompletionItem>();
	const regex = /\b(?:const|rec(?:\s+mut)?|var)\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)/g;

	for (const match of text.matchAll(regex)) {
		const name = decodeIdentifier(match[1]);
		if (items.has(name)) {
			continue;
		}

		const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Variable);
		item.detail = "Local symbol";
		items.set(name, item);
	}

	return [...items.values()];
}

function collectVariableTypes(text: string): Map<string, string> {
	const variableTypes = new Map<string, string>();

	for (const match of text.matchAll(/\b(?:const|var)\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^=;\n]+)/g)) {
		const name = decodeIdentifier(match[1]);
		const typeName = extractReferenceTypeName(match[2]);

		if (typeName) {
			variableTypes.set(name, typeName);
		}
	}

	for (const match of text.matchAll(/\bfn\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*\(([\s\S]*?)\)/g)) {
		const parameters = match[2];
		const parameterRegex = /("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^,\n)]+)/g;

		for (const parameterMatch of parameters.matchAll(parameterRegex)) {
			const name = decodeIdentifier(parameterMatch[1]);
			const typeName = extractReferenceTypeName(parameterMatch[2]);

			if (typeName) {
				variableTypes.set(name, typeName);
			}
		}
	}

	return variableTypes;
}

function collectActiveDatabases(text: string): string[] {
	const databases: string[] = [];

	for (const match of text.matchAll(/\bwith\s+([^;]+);/g)) {
		for (const part of match[1].split(",")) {
			const trimmed = part.trim();
			if (!trimmed) {
				continue;
			}

			const identifier = readIdentifier(trimmed, 0);
			if (identifier && identifier.end === trimmed.length) {
				databases.push(identifier.value);
			}
		}
	}

	return dedupe(databases);
}

function collectRecordPointerTypes(
	text: string,
	schemaTables: SchemaTableInfo[],
	activeDatabases: string[],
): Map<string, string> {
	const recordPointerTypes = new Map<string, string>();

	for (const match of text.matchAll(/\brec(?:\s+mut)?\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*=\s*find(?:\s+(?:first|last))?\s+((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)(?:\.(?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*))*)/g)) {
		const name = decodeIdentifier(match[1]);
		const tableType = resolveTableReferenceType(splitQualifiedChain(match[2]), schemaTables, activeDatabases);

		if (tableType) {
			recordPointerTypes.set(name, tableType);
		}
	}

	for (const match of text.matchAll(/\bif\s+rec\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*=\s*find(?:\s+(?:first|last))?\s+((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)(?:\.(?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*))*)/g)) {
		const name = decodeIdentifier(match[1]);
		const tableType = resolveTableReferenceType(splitQualifiedChain(match[2]), schemaTables, activeDatabases);

		if (tableType) {
			recordPointerTypes.set(name, tableType);
		}
	}

	for (const match of text.matchAll(/\bfor\s+rec(?:\s+mut)?\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s+in\s+((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)(?:\.(?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*))*)/g)) {
		const name = decodeIdentifier(match[1]);
		const tableType = resolveTableReferenceType(splitQualifiedChain(match[2]), schemaTables, activeDatabases);

		if (tableType) {
			recordPointerTypes.set(name, tableType);
		}
	}

	return recordPointerTypes;
}

function collectVisibleBindings(
	text: string,
	cursorOffset: number,
	context: CompletionContextData,
): VisibleBindings {
	const visibleText = text.slice(0, cursorOffset);
	const scopes: VisibleBindings[] = [new Map()];
	const pendingScopes: VisibleBindings[] = [];
	let index = 0;

	while (index < visibleText.length) {
		const char = visibleText[index];

		if (char === "\"" || char === "'") {
			index = skipQuoted(visibleText, index, char);
			continue;
		}

		if (char === "{") {
			scopes.push(pendingScopes.shift() ?? new Map());
			index += 1;
			continue;
		}

		if (char === "}") {
			if (scopes.length > 1) {
				scopes.pop();
			}
			index += 1;
			continue;
		}

		const identifier = readIdentifier(visibleText, index);
		if (!identifier) {
			index += 1;
			continue;
		}

		switch (identifier.value) {
			case "fn": {
				const parsed = parseFunctionScopeBindings(visibleText, identifier.end, context);
				if (parsed) {
					pendingScopes.push(parsed.bindings);
					index = parsed.nextIndex;
					continue;
				}
				break;
			}
			case "var":
			case "const": {
				const parsed = parseVariableBinding(visibleText, identifier.end);
				if (parsed) {
					scopes.at(-1)?.set(parsed.name, parsed.typeName);
					index = parsed.nextIndex;
					continue;
				}
				break;
			}
			case "rec": {
				const parsed = parseRecordBinding(visibleText, identifier.end, context, mergedVisibleBindings(scopes));
				if (parsed) {
					scopes.at(-1)?.set(parsed.name, parsed.typeName);
					index = parsed.nextIndex;
					continue;
				}
				break;
			}
			case "if": {
				const parsed = parseIfRecBinding(visibleText, identifier.end, context, mergedVisibleBindings(scopes));
				if (parsed) {
					pendingScopes.push(new Map([[parsed.name, parsed.typeName]]));
					index = parsed.nextIndex;
					continue;
				}
				break;
			}
			case "for": {
				const parsed = parseForRecordBinding(visibleText, identifier.end, context);
				if (parsed) {
					pendingScopes.push(new Map([[parsed.name, parsed.typeName]]));
					index = parsed.nextIndex;
					continue;
				}
				break;
			}
		}

		index = identifier.end;
	}

	return mergedVisibleBindings(scopes);
}

function mergedVisibleBindings(scopes: VisibleBindings[]): VisibleBindings {
	const merged = new Map<string, string>();

	for (const scope of scopes) {
		for (const [name, typeName] of scope) {
			merged.set(name, typeName);
		}
	}

	return merged;
}

function parseFunctionScopeBindings(
	text: string,
	start: number,
	context: CompletionContextData,
): { bindings: VisibleBindings; nextIndex: number } | undefined {
	let index = skipWhitespace(text, start);
	const functionName = readIdentifier(text, index);
	if (!functionName) {
		return undefined;
	}

	index = skipWhitespace(text, functionName.end);
	if (text[index] !== "(") {
		return undefined;
	}

	const endIndex = findMatchingParen(text, index);
	if (endIndex < 0) {
		return undefined;
	}

	const bindings = new Map<string, string>();
	const parameters = text.slice(index + 1, endIndex);
	for (const match of parameters.matchAll(/("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^,\n)]+)/g)) {
		const name = decodeIdentifier(match[1]);
		const typeName = extractReferenceTypeName(match[2]);
		if (typeName) {
			bindings.set(name, typeName);
		}
	}

	return {
		bindings,
		nextIndex: endIndex + 1,
	};
}

function parseVariableBinding(
	text: string,
	start: number,
): { name: string; nextIndex: number; typeName: string } | undefined {
	let index = skipWhitespace(text, start);
	const name = readIdentifier(text, index);
	if (!name) {
		return undefined;
	}

	index = skipWhitespace(text, name.end);
	if (text[index] !== ":") {
		return undefined;
	}

	index = skipWhitespace(text, index + 1);
	const typeEnd = findTypeEnd(text, index);
	const typeName = extractReferenceTypeName(text.slice(index, typeEnd));
	if (!typeName) {
		return undefined;
	}

	return {
		name: name.value,
		nextIndex: typeEnd,
		typeName,
	};
}

function parseRecordBinding(
	text: string,
	start: number,
	context: CompletionContextData,
	visibleBindings: VisibleBindings,
): { name: string; nextIndex: number; typeName: string } | undefined {
	let index = skipWhitespace(text, start);
	if (text.startsWith("mut", index) && !isIdentifierChar(text[index + 3] ?? "")) {
		index = skipWhitespace(text, index + 3);
	}

	const name = readIdentifier(text, index);
	if (!name) {
		return undefined;
	}

	index = skipWhitespace(text, name.end);
	if (text[index] !== "=") {
		return undefined;
	}

	const expressionStart = skipWhitespace(text, index + 1);
	const expressionEnd = skipDelimitedExpression(text, expressionStart, [";"]);
	const typeName = inferExpressionType(text.slice(expressionStart, expressionEnd), context, visibleBindings);
	if (!typeName) {
		return undefined;
	}

	return {
		name: name.value,
		nextIndex: expressionEnd,
		typeName,
	};
}

function parseIfRecBinding(
	text: string,
	start: number,
	context: CompletionContextData,
	visibleBindings: VisibleBindings,
): { name: string; nextIndex: number; typeName: string } | undefined {
	let index = skipWhitespace(text, start);
	if (!text.startsWith("rec", index) || isIdentifierChar(text[index + 3] ?? "")) {
		return undefined;
	}

	index = skipWhitespace(text, index + 3);
	const name = readIdentifier(text, index);
	if (!name) {
		return undefined;
	}

	index = skipWhitespace(text, name.end);
	if (text[index] !== "=") {
		return undefined;
	}

	const expressionStart = skipWhitespace(text, index + 1);
	const expressionEnd = skipDelimitedExpression(text, expressionStart, ["{"]);
	const typeName = inferExpressionType(text.slice(expressionStart, expressionEnd), context, visibleBindings);
	if (!typeName) {
		return undefined;
	}

	return {
		name: name.value,
		nextIndex: expressionEnd,
		typeName,
	};
}

function parseForRecordBinding(
	text: string,
	start: number,
	context: CompletionContextData,
): { name: string; nextIndex: number; typeName: string } | undefined {
	let index = skipWhitespace(text, start);
	if (!text.startsWith("rec", index) || isIdentifierChar(text[index + 3] ?? "")) {
		return undefined;
	}

	index = skipWhitespace(text, index + 3);
	if (text.startsWith("mut", index) && !isIdentifierChar(text[index + 3] ?? "")) {
		index = skipWhitespace(text, index + 3);
	}

	const name = readIdentifier(text, index);
	if (!name) {
		return undefined;
	}

	index = skipWhitespace(text, name.end);
	if (!text.startsWith("in", index) || isIdentifierChar(text[index + 2] ?? "")) {
		return undefined;
	}

	const tableStart = skipWhitespace(text, index + 2);
	const tableEnd = skipDelimitedExpression(text, tableStart, ["where", "order", "group", "limit", "{"]);
	const tableType = resolveTableReferenceType(splitQualifiedChain(text.slice(tableStart, tableEnd).trim()), context.schemaTables, context.activeDatabases);
	if (!tableType) {
		return undefined;
	}

	return {
		name: name.value,
		nextIndex: tableEnd,
		typeName: tableType,
	};
}

function inferExpressionType(
	expressionSource: string,
	context: CompletionContextData,
	visibleBindings: VisibleBindings,
): string | undefined {
	let source = expressionSource.trim();
	if (!source) {
		return undefined;
	}

	source = stripWrappingParentheses(source);

	const findMatch = source.match(FIND_TABLE_PATTERN);
	if (findMatch) {
		return resolveTableReferenceType(splitQualifiedChain(findMatch[1]), context.schemaTables, context.activeDatabases);
	}

	const objectConstructionType = extractObjectConstructionTypeName(source);
	if (objectConstructionType) {
		return objectConstructionType;
	}

	const identifierMatch = readIdentifier(source, 0);
	if (!identifierMatch) {
		return undefined;
	}

	const chain = splitQualifiedChain(source);
	if (chain.length === 0) {
		return undefined;
	}

	return resolveMemberType(chain, context, visibleBindings);
}

function extractObjectConstructionTypeName(source: string): string | undefined {
	const braceIndex = source.indexOf("{");
	if (braceIndex <= 0) {
		return undefined;
	}

	const prefix = source.slice(0, braceIndex).trim();
	if (!prefix) {
		return undefined;
	}

	const chain = splitQualifiedChain(prefix);
	if (chain.length === 0) {
		return undefined;
	}

	return chain.join(".");
}

function stripWrappingParentheses(source: string): string {
	let current = source.trim();

	while (current.startsWith("(") && current.endsWith(")")) {
		const endIndex = findMatchingParen(current, 0);
		if (endIndex !== current.length - 1) {
			break;
		}

		current = current.slice(1, -1).trim();
	}

	return current;
}

function findTypeEnd(text: string, start: number): number {
	let index = start;
	let bracketDepth = 0;

	while (index < text.length) {
		const char = text[index];
		if (char === "\"" || char === "'") {
			index = skipQuoted(text, index, char);
			continue;
		}

		if (char === "[") {
			bracketDepth += 1;
		}
		else if (char === "]") {
			bracketDepth = Math.max(0, bracketDepth - 1);
		}
		else if (bracketDepth === 0 && (char === "=" || char === ";" || char === "\n" || char === "\r")) {
			break;
		}

		index += 1;
	}

	return index;
}

function skipDelimitedExpression(text: string, start: number, terminators: string[]): number {
	let index = start;
	let braceDepth = 0;
	let bracketDepth = 0;
	let parenDepth = 0;

	while (index < text.length) {
		const char = text[index];
		if (char === "\"" || char === "'") {
			index = skipQuoted(text, index, char);
			continue;
		}

		if (char === "{") {
			if (braceDepth === 0 && terminators.includes("{")) {
				break;
			}
			braceDepth += 1;
			index += 1;
			continue;
		}
		if (char === "}") {
			braceDepth = Math.max(0, braceDepth - 1);
			index += 1;
			continue;
		}
		if (char === "[") {
			bracketDepth += 1;
			index += 1;
			continue;
		}
		if (char === "]") {
			bracketDepth = Math.max(0, bracketDepth - 1);
			index += 1;
			continue;
		}
		if (char === "(") {
			parenDepth += 1;
			index += 1;
			continue;
		}
		if (char === ")") {
			parenDepth = Math.max(0, parenDepth - 1);
			index += 1;
			continue;
		}

		if (braceDepth === 0 && bracketDepth === 0 && parenDepth === 0) {
			if (terminators.includes(";") && char === ";") {
				break;
			}

			for (const terminator of terminators) {
				if (terminator.length > 1 && text.startsWith(terminator, index) && !isIdentifierChar(text[index + terminator.length] ?? "")) {
					return index;
				}
			}
		}

		index += 1;
	}

	return index;
}

function extractParenthesizedMemberBaseExpression(linePrefix: string): string | undefined {
	const trimmed = linePrefix.replace(/\s+$/u, "");
	const dotIndex = trimmed.lastIndexOf(".");
	if (dotIndex < 0) {
		return undefined;
	}

	const memberFragment = trimmed.slice(dotIndex + 1);
	if (memberFragment && !readIdentifier(memberFragment, 0)) {
		return undefined;
	}

	const base = trimmed.slice(0, dotIndex).trimEnd();
	if (!base.endsWith(")")) {
		return undefined;
	}

	const openIndex = findMatchingOpenParen(base);
	if (openIndex < 0) {
		return undefined;
	}

	return base.slice(openIndex + 1, -1);
}

function findMatchingOpenParen(text: string): number {
	let depth = 0;

	for (let index = text.length - 1; index >= 0; index -= 1) {
		const char = text[index];
		if (char === ")") {
			depth += 1;
		}
		else if (char === "(") {
			depth -= 1;
			if (depth === 0) {
				return index;
			}
		}
	}

	return -1;
}

function isIdentifierChar(char: string): boolean {
	return /[A-Za-z0-9_]/u.test(char);
}

function loadSchemaTables(document: vscode.TextDocument): SchemaTableInfo[] {
	const configPath = resolveProjectConfigPath(document);
	if (!configPath) {
		return [];
	}

	try {
		const configContents = fs.readFileSync(configPath, "utf8");
		const schemaMappings = parseSchemaMappings(configContents);
		const baseDirectory = path.dirname(configPath);
		const tables: SchemaTableInfo[] = [];

		for (const [databaseName, configuredPath] of schemaMappings) {
			const resolvedPath = path.isAbsolute(configuredPath)
				? configuredPath
				: path.resolve(baseDirectory, configuredPath);

			if (!fs.existsSync(resolvedPath)) {
				continue;
			}

			const schemaSource = fs.readFileSync(resolvedPath, "utf8");
			tables.push(...parseSchemaTables(schemaSource, databaseName));
		}

		return tables;
	}
	catch {
		return [];
	}
}

function resolveProjectConfigPath(document: vscode.TextDocument): string | undefined {
	const configuredPath = vscode.workspace.getConfiguration("tablo").get<string>("debug.projectConfigPath")?.trim();
	const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);

	if (configuredPath) {
		if (path.isAbsolute(configuredPath)) {
			return configuredPath;
		}

		if (workspaceFolder) {
			return path.resolve(workspaceFolder.uri.fsPath, configuredPath);
		}

		return path.resolve(path.dirname(document.uri.fsPath), configuredPath);
	}

	if (workspaceFolder) {
		const defaultPath = path.join(workspaceFolder.uri.fsPath, "tablo.toml");
		if (fs.existsSync(defaultPath)) {
			return defaultPath;
		}
	}

	const searchRoots = [
		workspaceFolder?.uri.fsPath,
	].filter((value): value is string => Boolean(value));

	return findNearestProjectConfig(path.dirname(document.uri.fsPath), searchRoots);
}

function findNearestProjectConfig(startDirectory: string, searchRoots: string[]): string | undefined {
	let currentDirectory = path.resolve(startDirectory);
	const normalizedRoots = searchRoots.map((root) => path.resolve(root));

	while (true) {
		const candidate = path.join(currentDirectory, "tablo.toml");
		if (fs.existsSync(candidate)) {
			return candidate;
		}

		const parentDirectory = path.dirname(currentDirectory);
		if (parentDirectory === currentDirectory) {
			return undefined;
		}

		if (normalizedRoots.includes(currentDirectory) && currentDirectory !== path.resolve(startDirectory)) {
			return undefined;
		}

		currentDirectory = parentDirectory;
	}
}

function parseSchemaMappings(contents: string): Map<string, string> {
	const mappings = new Map<string, string>();
	let inSchemasSection = false;

	for (const rawLine of contents.split(/\r?\n/u)) {
		const line = rawLine.trim();
		if (!line || line.startsWith("#")) {
			continue;
		}

		if (line.startsWith("[")) {
			inSchemasSection = line === "[schemas]";
			continue;
		}

		if (!inSchemasSection) {
			continue;
		}

		const match = line.match(/^("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*=\s*"([^"]*)"$/u);
		if (!match) {
			continue;
		}

		mappings.set(decodeIdentifier(match[1]), match[2]);
	}

	return mappings;
}

function parseSchemaTables(source: string, fallbackDatabaseName: string): SchemaTableInfo[] {
	const tables: SchemaTableInfo[] = [];
	let currentDatabaseName = fallbackDatabaseName;
	let currentSchemaName = "Main";
	let currentSchemaImplicit = false;

	const databaseRegex = /\bdatabase\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*;/gi;
	for (const match of source.matchAll(databaseRegex)) {
		currentDatabaseName = decodeIdentifier(match[1]);
		break;
	}

	const declarationRegex = /\bschema\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*(implicit)?\s*;|\bcreate\s+table\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)\s*\(/gi;
	let match: RegExpExecArray | null;
	while ((match = declarationRegex.exec(source)) !== null) {
		if (match[1]) {
			currentSchemaName = decodeIdentifier(match[1]);
			currentSchemaImplicit = Boolean(match[2]);
			continue;
		}

		const tableName = decodeIdentifier(match[3]);
		const openParenIndex = source.indexOf("(", match.index);
		if (openParenIndex < 0) {
			continue;
		}

		const closeParenIndex = findMatchingParen(source, openParenIndex);
		if (closeParenIndex < 0) {
			continue;
		}

		tables.push({
			databaseName: currentDatabaseName,
			fields: parseSchemaFields(source.slice(openParenIndex + 1, closeParenIndex)),
			implicitSchema: currentSchemaImplicit,
			schemaName: currentSchemaName,
			tableName,
			typeName: `@record:${currentDatabaseName}.${currentSchemaName}.${tableName}`,
		});

		declarationRegex.lastIndex = closeParenIndex + 1;
	}

	return tables;
}

function parseSchemaFields(body: string): SchemaFieldInfo[] {
	const fields: SchemaFieldInfo[] = [];
	let index = 0;

	while (index < body.length) {
		index = skipWhitespace(body, index);
		const identifier = readIdentifier(body, index);
		if (!identifier) {
			index += 1;
			continue;
		}

		index = skipWhitespace(body, identifier.end);
		let typeEnd = index;
		while (typeEnd < body.length && body[typeEnd] !== "," && body[typeEnd] !== "\n" && body[typeEnd] !== "\r") {
			typeEnd += 1;
		}

		fields.push({
			name: identifier.value,
			typeName: schemaFieldTypeToTabloType(body.slice(index, typeEnd)),
		});

		index = typeEnd + 1;
	}

	return dedupeSchemaFields(fields);
}

function schemaFieldTypeToTabloType(source: string): string | undefined {
	const match = source.trim().match(/^(\[[^\]]+\]|[A-Za-z_][A-Za-z0-9_]*)/u);
	if (!match) {
		return undefined;
	}

	return extractReferenceTypeName(match[1]);
}

function dedupeSchemaFields(fields: SchemaFieldInfo[]): SchemaFieldInfo[] {
	const seen = new Set<string>();
	const result: SchemaFieldInfo[] = [];

	for (const field of fields) {
		if (seen.has(field.name)) {
			continue;
		}

		seen.add(field.name);
		result.push(field);
	}

	return result;
}

function findMatchingParen(text: string, openParenIndex: number): number {
	let depth = 0;

	for (let i = openParenIndex; i < text.length; i += 1) {
		const char = text[i];

		if (char === "\"" || char === "'") {
			i = skipQuoted(text, i, char) - 1;
			continue;
		}

		if (char === "(") {
			depth += 1;
		}
		else if (char === ")") {
			depth -= 1;
			if (depth === 0) {
				return i;
			}
		}
	}

	return -1;
}

function resolveTableReferenceType(
	components: string[],
	schemaTables: SchemaTableInfo[],
	activeDatabases: string[],
): string | undefined {
	if (components.length === 0 || components.length > 3) {
		return undefined;
	}

	const allowedTables = activeDatabases.length === 0
		? schemaTables
		: schemaTables.filter((table) => activeDatabases.some((database) => sameName(database, table.databaseName)));

	if (components.length === 1) {
		const matches = allowedTables.filter((table) => sameName(components[0], table.tableName));
		return matches.length === 1 ? matches[0].typeName : undefined;
	}

	if (components.length === 2) {
		const schemaQualified = allowedTables.filter((table) =>
			sameName(components[0], table.schemaName) && sameName(components[1], table.tableName),
		);
		if (schemaQualified.length === 1) {
			return schemaQualified[0].typeName;
		}

		const databaseQualified = allowedTables.filter((table) =>
			sameName(components[0], table.databaseName) && sameName(components[1], table.tableName),
		);
		if (databaseQualified.length === 1) {
			return databaseQualified[0].typeName;
		}

		return undefined;
	}

	const exact = allowedTables.find((table) =>
		sameName(components[0], table.databaseName)
		&& sameName(components[1], table.schemaName)
		&& sameName(components[2], table.tableName),
	);
	return exact?.typeName;
}

function sameName(left: string, right: string): boolean {
	return left.localeCompare(right, undefined, { sensitivity: "accent" }) === 0;
}

function decodeIdentifier(identifier: string): string {
	if (identifier.startsWith("\"") && identifier.endsWith("\"")) {
		return identifier.slice(1, -1).replace(/""/g, "\"");
	}

	return identifier;
}

function extractReferenceTypeName(source: string): string | undefined {
	const trimmed = source.trim().replace(/\?+$/u, "").trim();
	if (!trimmed || trimmed.startsWith("[") || trimmed.includes("|") || trimmed.startsWith("&")) {
		return undefined;
	}

	return decodeIdentifier(trimmed);
}

function collectObjectFields(
	typeName: string,
	body: string,
	objects: Map<string, ObjectFieldInfo[]>,
): void {
	const fields: ObjectFieldInfo[] = [];
	let depth = 0;
	let i = 0;

	while (i < body.length) {
		const char = body[i];

		if (char === "\"" || char === "'") {
			i = skipQuoted(body, i, char);
			continue;
		}

		if (char === "{") {
			depth += 1;
			i += 1;
			continue;
		}

		if (char === "}") {
			depth = Math.max(0, depth - 1);
			i += 1;
			continue;
		}

		if (depth === 0) {
			const identifier = readIdentifier(body, i);
			if (identifier) {
				const afterIdentifier = skipWhitespace(body, identifier.end);
				if (body[afterIdentifier] === ":") {
					const typeInfo = readObjectFieldType(body, afterIdentifier + 1, typeName, identifier.value, objects);
					fields.push({
						name: identifier.value,
						typeName: typeInfo.typeName,
					});
					i = typeInfo.nextIndex;
					continue;
				}
			}
		}

		i += 1;
	}

	objects.set(typeName, dedupeObjectFields(fields));
}

function extractTopLevelMemberNames(body: string): string[] {
	const members: string[] = [];
	let depth = 0;
	let i = 0;

	while (i < body.length) {
		const char = body[i];

		if (char === "\"" || char === "'") {
			i = skipQuoted(body, i, char);
			continue;
		}

		if (char === "{") {
			depth += 1;
			i += 1;
			continue;
		}

		if (char === "}") {
			depth = Math.max(0, depth - 1);
			i += 1;
			continue;
		}

		if (depth === 0) {
			const identifier = readIdentifier(body, i);
			if (identifier) {
				members.push(identifier.value);
				i = identifier.end;
				continue;
			}
		}

		i += 1;
	}

	return dedupe(members);
}

function findBraceDeclarations(text: string, keywordName: "enum" | "obj"): Array<{ body: string; name: string }> {
	const declarations: Array<{ body: string; name: string }> = [];
	const regex = new RegExp(`\\b${keywordName}\\s+("(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)`, "g");

	for (const match of text.matchAll(regex)) {
		const name = match[1];
		const nameEnd = (match.index ?? 0) + match[0].length;
		const braceIndex = text.indexOf("{", nameEnd);

		if (braceIndex < 0) {
			continue;
		}

		const endIndex = findMatchingBrace(text, braceIndex);
		if (endIndex < 0) {
			continue;
		}

		declarations.push({
			body: text.slice(braceIndex + 1, endIndex),
			name,
		});
	}

	return declarations;
}

function findMatchingBrace(text: string, openBraceIndex: number): number {
	let depth = 0;

	for (let i = openBraceIndex; i < text.length; i += 1) {
		const char = text[i];

		if (char === "\"" || char === "'") {
			i = skipQuoted(text, i, char) - 1;
			continue;
		}

		if (char === "{") {
			depth += 1;
		}
		else if (char === "}") {
			depth -= 1;
			if (depth === 0) {
				return i;
			}
		}
	}

	return -1;
}

function keyword(label: string): vscode.CompletionItem {
	const item = new vscode.CompletionItem(label, vscode.CompletionItemKind.Keyword);
	item.detail = "Keyword";
	return item;
}

function literal(label: string): vscode.CompletionItem {
	const item = new vscode.CompletionItem(label, vscode.CompletionItemKind.Value);
	item.detail = "Literal";
	return item;
}

function provideMemberCompletions(
	document: vscode.TextDocument,
	position: vscode.Position,
	context: CompletionContextData,
): vscode.CompletionItem[] | undefined {
	const linePrefix = document.lineAt(position.line).text.slice(0, position.character);
	const visibleBindings = collectVisibleBindings(document.getText(), document.offsetAt(position), context);
	const match = linePrefix.match(QUALIFIED_MEMBER_PATTERN);

	let memberType: string | undefined;

	if (match) {
		const chain = splitQualifiedChain(match[1]);
		memberType = resolveMemberType(chain, context, visibleBindings);
	}
	else {
		const baseExpression = extractParenthesizedMemberBaseExpression(linePrefix);
		if (!baseExpression) {
			return undefined;
		}

		memberType = inferExpressionType(baseExpression, context, visibleBindings);
	}

	if (!memberType) {
		return undefined;
	}

	if (context.enums.has(memberType)) {
		return context.enums.get(memberType)?.map((variant) => {
			const item = new vscode.CompletionItem(variant, vscode.CompletionItemKind.EnumMember);
			item.detail = `Variant of ${memberType}`;
			return item;
		});
	}

	if (context.objects.has(memberType)) {
		return context.objects.get(memberType)?.map((field) => {
			const item = new vscode.CompletionItem(field.name, vscode.CompletionItemKind.Field);
			item.detail = `Field of ${memberType}`;
			return item;
		});
	}

	const schemaTable = context.schemaTables.find((table) => table.typeName === memberType);
	if (schemaTable) {
		return schemaTable.fields.map((field) => {
			const item = new vscode.CompletionItem(field.name, vscode.CompletionItemKind.Field);
			item.detail = `Field of ${schemaTable.tableName}`;
			return item;
		});
	}

	return undefined;
}

function readIdentifier(text: string, start: number): { end: number; value: string } | undefined {
	IDENTIFIER_PATTERN.lastIndex = start;
	const match = IDENTIFIER_PATTERN.exec(text);

	if (!match || match.index !== start) {
		return undefined;
	}

	return {
		end: IDENTIFIER_PATTERN.lastIndex,
		value: decodeIdentifier(match[0]),
	};
}

function skipQuoted(text: string, start: number, quote: "\"" | "'"): number {
	let i = start + 1;

	while (i < text.length) {
		if (quote === "\"" && text[i] === "\"" && text[i + 1] === "\"") {
			i += 2;
			continue;
		}

		if (quote === "'" && text[i] === "\\") {
			i += 2;
			continue;
		}

		if (text[i] === quote) {
			return i + 1;
		}

		i += 1;
	}

	return text.length;
}

function skipWhitespace(text: string, start: number): number {
	let i = start;
	while (i < text.length && /\s/u.test(text[i])) {
		i += 1;
	}
	return i;
}

function snippet(
	label: string,
	detail: string,
	value: string,
	documentation: string,
): vscode.CompletionItem {
	const item = new vscode.CompletionItem(label, vscode.CompletionItemKind.Snippet);
	item.detail = detail;
	item.documentation = new vscode.MarkdownString(documentation);
	item.insertText = new vscode.SnippetString(value);
	return item;
}

function typeItem(label: string): vscode.CompletionItem {
	const item = new vscode.CompletionItem(label, vscode.CompletionItemKind.TypeParameter);
	item.detail = "Primitive type";
	return item;
}

function dedupe(values: string[]): string[] {
	return [...new Set(values)];
}

function dedupeObjectFields(fields: ObjectFieldInfo[]): ObjectFieldInfo[] {
	const seen = new Set<string>();
	const result: ObjectFieldInfo[] = [];

	for (const field of fields) {
		if (seen.has(field.name)) {
			continue;
		}

		seen.add(field.name);
		result.push(field);
	}

	return result;
}

function readObjectFieldType(
	body: string,
	start: number,
	parentTypeName: string,
	fieldName: string,
	objects: Map<string, ObjectFieldInfo[]>,
): { nextIndex: number; typeName?: string } {
	let i = skipWhitespace(body, start);

	if (body[i] === "{") {
		const endIndex = findMatchingBrace(body, i);
		if (endIndex < 0) {
			return { nextIndex: body.length };
		}

		const inlineTypeName = `${parentTypeName}.${fieldName}`;
		collectObjectFields(inlineTypeName, body.slice(i + 1, endIndex), objects);
		return {
			nextIndex: skipObjectFieldTail(body, endIndex + 1),
			typeName: inlineTypeName,
		};
	}

	const typeStart = i;
	let parenDepth = 0;
	let bracketDepth = 0;

	while (i < body.length) {
		const char = body[i];

		if (char === "\"" || char === "'") {
			i = skipQuoted(body, i, char);
			continue;
		}

		if (char === "(") {
			parenDepth += 1;
			i += 1;
			continue;
		}

		if (char === ")") {
			parenDepth = Math.max(0, parenDepth - 1);
			i += 1;
			continue;
		}

		if (char === "[") {
			bracketDepth += 1;
			i += 1;
			continue;
		}

		if (char === "]") {
			bracketDepth = Math.max(0, bracketDepth - 1);
			i += 1;
			continue;
		}

		if (parenDepth === 0 && bracketDepth === 0 && (char === "," || char === "=" || char === "\n" || char === "\r")) {
			break;
		}

		i += 1;
	}

	const typeName = extractReferenceTypeName(body.slice(typeStart, i));
	return {
		nextIndex: skipObjectFieldTail(body, i),
		typeName,
	};
}

function skipObjectFieldTail(body: string, start: number): number {
	let i = start;
	let braceDepth = 0;
	let bracketDepth = 0;
	let parenDepth = 0;

	while (i < body.length) {
		const char = body[i];

		if (char === "\"" || char === "'") {
			i = skipQuoted(body, i, char);
			continue;
		}

		if (char === "{") {
			braceDepth += 1;
			i += 1;
			continue;
		}

		if (char === "}") {
			if (braceDepth === 0) {
				return i;
			}
			braceDepth -= 1;
			i += 1;
			continue;
		}

		if (char === "[") {
			bracketDepth += 1;
			i += 1;
			continue;
		}

		if (char === "]") {
			bracketDepth = Math.max(0, bracketDepth - 1);
			i += 1;
			continue;
		}

		if (char === "(") {
			parenDepth += 1;
			i += 1;
			continue;
		}

		if (char === ")") {
			parenDepth = Math.max(0, parenDepth - 1);
			i += 1;
			continue;
		}

		if (braceDepth === 0 && bracketDepth === 0 && parenDepth === 0 && char === ",") {
			return i;
		}

		i += 1;
	}

	return i;
}

function resolveMemberType(
	chain: string[],
	context: CompletionContextData,
	visibleBindings: VisibleBindings,
): string | undefined {
	if (chain.length === 0) {
		return undefined;
	}

	let currentType = visibleBindings.get(chain[0]) ?? chain[0];

	for (const fieldName of chain.slice(1)) {
		const objectFields = context.objects.get(currentType);
		if (!objectFields) {
			return undefined;
		}

		const field = objectFields.find((candidate) => candidate.name === fieldName);
		if (!field?.typeName) {
			return undefined;
		}

		currentType = field.typeName;
	}

	return currentType;
}

function splitQualifiedChain(value: string): string[] {
	const parts: string[] = [];
	let index = 0;

	while (index < value.length) {
		const identifier = readIdentifier(value, index);
		if (!identifier) {
			break;
		}

		parts.push(identifier.value);
		index = identifier.end;

		if (value[index] === ".") {
			index += 1;
		}
	}

	return parts;
}
