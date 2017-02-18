/**
 * Jazr. A tool to parse, analyse and transform javascript modules
 * Copyright (C) 2016  Sebastiaan Koppe
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
module es6.tokens;

enum Type {
	Identifier,
	StringLiteral,
	Error,
	BinaryLiteral,
	OctalLiteral,
	DecimalLiteral,
	HexLiteral,
	TemplateHead,
	Template,
	TemplateTail,
	EndOfFile,
	LineTerminator,
	OpenCurlyBrace,
	CloseCurlyBrace,
	OpenParenthesis,
	CloseParenthesis,
	OpenSquareBrackets,
	CloseSquareBrackets,
	Dot,
	SpreadOperator,
	Semicolon,
	Comma,
	LeftShiftAssignment,
	LeftShift,
	LessOrEqual,
	LessThan,
	TripleRightShiftAssignment,
	TripleRightSift,
	RightShiftAssignment,
	RightShift,
	GreaterOrEqual,
	GreaterThan,
	StrictEqual,
	Equal,
	Arrow,
	Assignment,
	StrictNotEqual,
	NotEqual,
	Negation,
	Increment,
	AdditiveAssignment,
	Add,
	Decrement,
	DecrementalAssignment,
	Minus,
	MultiplicativeAssignment,
	Multiply,
	DivisionAssignment,
	Division,
	ModAssignment,
	Mod,
	LogicalAnd,
	BitwiseAndAssignment,
	BitwiseAnd,
	LogicalOr,
	BitwiseOrAssignment,
	BitwiseOr,
	BitwiseXorAssignment,
	BitwiseXor,
	Tilde,
	QuestionMark,
	Colon,
	Regex,
	SingleLineComment,
	MultiLineComment,
	ExponentIndicator,
	UnicodeEscapeSequence,
	StartIdentifier,
	TailIdentifier,
	EndIdentifier
}

struct Token
{
	Type type;
	string match;
}

bool matches(Token tok, Type t) {
	return tok.type == t;
}
bool matches(Token tok, Type t, string m) {
	return tok.type == t && tok.match == m;
}