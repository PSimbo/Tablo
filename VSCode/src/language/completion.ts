import * as vscode from "vscode";

type CompletionContextData = {
	enums: Map<string, string[]>;
	objects: Map<string, ObjectFieldInfo[]>;
	symbols: vscode.CompletionItem[];
	variableTypes: Map<string, string>;
};

type ObjectFieldInfo = {
	name: string;
	typeName?: string;
};

const IDENTIFIER_PATTERN = /"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*/y;
const QUALIFIED_MEMBER_PATTERN = /((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)(?:\.(?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*))*)\.((?:"(?:[^"]|"")*"|[A-Za-z_][A-Za-z0-9_]*)?)$/;

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

export function registerCompletionProvider(context: vscode.ExtensionContext): void {
	const provider = vscode.languages.registerCompletionItemProvider(
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

	context.subscriptions.push(provider);
}

function analyzeDocument(document: vscode.TextDocument): CompletionContextData {
	const text = document.getText();
	const enums = collectEnumDeclarations(text);
	const objects = collectObjectDeclarations(text);
	const variableTypes = collectVariableTypes(text);
	const symbols = [
		...collectDocumentSymbols(text, "fn", vscode.CompletionItemKind.Function, "Function"),
		...collectDocumentSymbols(text, "obj", vscode.CompletionItemKind.Struct, "Object"),
		...collectDocumentSymbols(text, "enum", vscode.CompletionItemKind.Enum, "Enum"),
		...collectVariableSymbols(text),
	];

	return {
		enums,
		objects,
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
	const match = linePrefix.match(QUALIFIED_MEMBER_PATTERN);

	if (!match) {
		return undefined;
	}

	const chain = splitQualifiedChain(match[1]);
	const memberType = resolveMemberType(chain, context);

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
): string | undefined {
	if (chain.length === 0) {
		return undefined;
	}

	let currentType = context.variableTypes.get(chain[0]) ?? chain[0];

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
