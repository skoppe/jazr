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
module es6.analyse;

version (unittest)
{
	import unit_threaded;
	import es6.testhelpers;
	import std.stdio;
	import es6.parser;
	import es6.emitter;
	import std.range : lockstep, walkLength;
	import std.algorithm : filter;
	Scope getScope(string js, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto node = parseNode!("parseModule",ModuleNode)(js,file,line);
		auto s = node.analyseNode().scp;
		node.assertTreeInternals(file,line);
		return s;
	}
	Branch getBranch(string js, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto node = parseNode!("parseModule",ModuleNode)(js,file,line);
		auto b = node.analyseNode().scp.branch;
		node.assertTreeInternals(file,line);
		return b;
	}
	Scope getFirstChildScope(string js, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto s = getScope(js,file,line);
		s.children.length.shouldBeGreaterThan(0,file,line);
		return s.children[0];
	}
	void assertIdentifier(Scope s, size_t idx, string identifier, in string file = __FILE__, in size_t line = __LINE__)
	{
		s.identifiers.length.shouldBeGreaterThan(idx,file,line);
		s.identifiers[idx].node.identifier.shouldEqual(identifier,file,line);
	}
	auto assertIdentifiers(Scope s, string[] identifiers, in string file = __FILE__, in size_t line = __LINE__)
	{
		s.identifiers.length.shouldEqual(identifiers.length,file,line);
		foreach(got,expected; lockstep(s.identifiers,identifiers))
			got.node.identifier.shouldEqual(expected,file,line);
		return s;
	}
	auto assertVariables(alias type)(Scope s, string[] variables, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto f = s.variables.filter!(v=>v.type == type);
		f.walkLength.shouldEqual(variables.length,file,line);
		foreach(got,expected; lockstep(f,variables))
			got.node.identifier.shouldEqual(expected,file,line);
		return s;
	}
}
import es6.nodes;
import es6.scopes;

struct AnalysisResult
{
	Scope scp;
	Branch trunk;
	//string[] unresolvedIdentifiers;
}
//enum Type { Normal, NewScope };

// NOTE: since the same types and the same content always results in the same hints, shouldn't this hinting be part of the Node?
//  Idea: put the return hints in the branch
// 
/++
	Sets the hints for this node and returns hints for parent node
+/
/*Hints processHints(Node node, Hints childHints = Hints.init)
{
	import std.algorithm : startsWith;
	Hints mask;
	if (node.isA!"ES6.FunctionStatementList")
		return mask;
	node.hint = childHints;
	if (node.isA!"ES6.VariableStatement")
		node.hint |= Hint.VariableDecl;
	else if (node.isA!"ES6.Parentheses")
	{
		node.hint &= ~(Hint.LogicalOr | Hint.Assignment);
	}
	else if (node.isA!"ES6.TryStatement")
		node.hint |= Hint.NonExpression;
	else if (node.isA!"ES6.ThrowStatement")
		node.hint |= Hint.NonExpression;
	else if (node.isA!"ES6.FunctionDeclaration")
		node.hint |= Hint.FunctionDecl;
	else if (node.isA!"ES6.BreakStatement")
		node.hint |= Hint.Break | Hint.NonExpression;
	else if (node.isA!"ES6.AssignmentOperator")
		node.hint |= Hint.Assignment;
	else if (node.isA!"ES6.LogicalOperator")
	{
		if (node.matches[0].startsWith("||"))
			node.hint |= Hint.LogicalOr;
	}
	else if (node.isA!"ES6.ReturnStatement")
	{
		node.hint |= Hint.ReturningBranch | Hint.EmitReturn;
		if (node.children.length == 0)
			node.hint |= Hint.EmptyReturn;
		else
			node.hint |= Hint.Return;
	} else if (node.isA!"ES6.IfStatement")
	{
		node.hint |= Hint.NonExpression;
		node.hint &= ~Hint.Assignment;
		import std.algorithm : all;

		if (node.children.length == 4 && node.children[1].hint & Hint.ReturningBranch && node.children[3].hint & Hint.ReturningBranch)
		{
			node.hint |= Hint.ReturningBranch | Hint.DeducedReturingBranch;
			node.hint &= ~Hint.EmitReturn;
		}
		else
			node.hint &= ~(Hint.EmptyReturn | Hint.Return | Hint.ReturningBranch | Hint.DeducedReturingBranch);
	} else if (node.isA!"ES6.CaseClauses" || node.isA!"ES6.CaseBlock")
	{
		node.hint = node.hint & ~(Hint.Break | Hint.ReturningBranch | Hint.Return | Hint.EmptyReturn | Hint.DeducedReturingBranch | Hint.EmitReturn);
	} else if (node.isA!"ES6.SwitchStatement" || node.isA!"ES6.ContinueStatement")
		node.hint |= Hint.NonExpression;
	else if (node.isA!"ES6.IterationStatement")
	{
		node.hint |= Hint.NonExpression;
		node.hint &= ~Hint.Break;
	}

	return node.hint & ~node.getHintMask;
}
Hints getHintMask(Node node)
{
	Hints mask;
	if (node.isA!"ES6.StatementListItem")
	{
		mask |= (Hint.VariableDecl | Hint.FunctionDecl);
	} else if (node.isA!"ES6.FunctionBody")
		mask |= ~Hint.None;
	return mask;
}*/
/++
	Walks the nodes and sets scopes and branches.

	Returns: AnalysisResult struct with global scope objects and main branch.
+/

//// **** Note: this might be done smarter by some form of a Visitor pattern, although that might be slower.
//// **** Another Note: we could also add the set of NodeTypes that a given Node might have as its children, at least then we could statically enforce the types of the children, but - also - know which types to switch on, and whether we have them all. the main question is, can we then reduce the 400 loc below??
private void analyseBindingElementNode(Node node, Scope s, Branch b, IdentifierType v)
{
	node.branch = b;
	if (node.children[0].type == NodeType.ArrayBindingPatternNode)
		analyseArrayBindingPatternNode(node.children[0],s,b,v);
	else if (node.children[0].type == NodeType.ObjectBindingPatternNode)
		analyseObjectBindingPatternNode(node.children[0],s,b,v);
	else{
		s.addVariableOrIdentifier(node.children[0].as!IdentifierNode,v);
		node.children[0].branch = b;
	}
	analyse(node.children[1],s,b);
}
private void analyseArrayBindingPatternNode(Node node, Scope s, Branch b, IdentifierType v)
{
	assert(b !is null);
	node.branch = b;
	foreach(item; node.children)
	{
		switch(item.type)
		{
			case NodeType.RestElementNode:
				item.children[0].branch = b;
				item.branch = b;
				s.addVariableOrIdentifier(item.children[0].as!IdentifierNode,v);
				break;
			case NodeType.BindingElementNode:
				analyseBindingElementNode(item,s,b,v);
				break;
			case NodeType.SingleNameBindingNode:
				item.children[0].branch = b;
				item.branch = b;
				s.addVariableOrIdentifier(item.children[0].as!IdentifierNode,v);
				analyse(item.children[1],s,b);
				break;
			case NodeType.IdentifierNode:
				s.addVariableOrIdentifier(item.as!IdentifierNode,v);
				item.branch = b;
				break;
			case NodeType.ObjectBindingPatternNode:
				analyseObjectBindingPatternNode(item,s,b,v);
				break;
			case NodeType.ArrayBindingPatternNode:
				analyseArrayBindingPatternNode(item,s,b,v);
				break;
			case NodeType.ElisionNode:
				item.branch = b;
				break;
			default: break;
		}
	}
}
private void analyseFormalParameterList(Node node, Scope s, Branch b = null)
{
	if (b is null)
		b = s.branch;
	assert(b !is null);
	node.branch = b;
	foreach(item; node.children)
	{
		switch(item.type)
		{
			case NodeType.RestElementNode:
				s.addVariableOrIdentifier(item.children[0].as!IdentifierNode,IdentifierType.Parameter);
				item.branch = b;
				item.children[0].branch = b;
				break;
			case NodeType.BindingElementNode:
				analyseBindingElementNode(item,s,b,IdentifierType.Parameter);
				break;
			case NodeType.ObjectBindingPatternNode:
				analyseObjectBindingPatternNode(item,s,b,IdentifierType.Parameter);
				break;
			case NodeType.ArrayBindingPatternNode:
				analyseArrayBindingPatternNode(item,s,b,IdentifierType.Parameter);
				break;
			case NodeType.IdentifierNode:
				s.addVariableOrIdentifier(item.as!IdentifierNode,IdentifierType.Parameter);
				item.branch = b;
				break;
			case NodeType.SingleNameBindingNode:
				item.children[0].branch = b;
				item.branch = b;
				s.addVariableOrIdentifier(item.children[0].as!IdentifierNode,IdentifierType.Parameter);
				analyse(item.children[1],s,b);
				break;
			default: break;
		}
	}
}
private void analyseClassSetterParam(Node node, Scope s)
{
	if (node.type == NodeType.BindingElementNode)
		analyseBindingElementNode(node,s,s.branch,IdentifierType.Parameter);
	else
	{
		node.branch = s.branch;
		s.addVariable(Variable(node.as!IdentifierNode,IdentifierType.Parameter));
	}
}
private void addVariableOrIdentifier(Scope s, IdentifierNode node, IdentifierType i)
{
	if (i == IdentifierType.Identifier)
		s.addIdentifier(Identifier(node));
	else
		s.addVariable(Variable(node,i));
}
private void analyseObjectBindingPatternNode(Node node, Scope s, Branch b, IdentifierType v)
{
	node.branch = b;
	foreach(item; node.children)
	{
		item.branch = b;
		switch(item.type)
		{
			case NodeType.SingleNameBindingNode:
				s.addVariableOrIdentifier(item.children[0].as!IdentifierNode,v);
				item.children[0].branch = b;
				analyse(item.children[1],s,b);
				break;
			case NodeType.BindingPropertyNode:
				item.children[0].assignBranch(b);
				switch(item.children[1].type)
				{
					case NodeType.ObjectBindingPatternNode:
						analyseObjectBindingPatternNode(item.children[1],s,b,v);
						break;
					case NodeType.ArrayBindingPatternNode:
						analyseArrayBindingPatternNode(item.children[1],s,b,v);
						break;
					case NodeType.BindingElementNode:
						analyseBindingElementNode(item.children[1],s,b,v);
						break;
					case NodeType.SingleNameBindingNode:
						item.children[1].branch = b;
						item.children[1].children[0].branch = b;
						s.addVariableOrIdentifier(item.children[1].children[0].as!IdentifierNode,v);
						analyse(item.children[1].children[1],s,b);
						break;
					case NodeType.IdentifierNode:
						item.children[1].branch = b;
						s.addVariableOrIdentifier(item.children[1].as!IdentifierNode,IdentifierType.Identifier);
						break;
					default: version(unittest) throw new UnitTestException([format("Didn't expect %s",item.children[1].type)]);assert(0);
				}
				break;
			case NodeType.IdentifierNode:
				s.addVariable(Variable(item.as!IdentifierNode,v));
				break;
			default: break;
		}
	}
}
private void analyseArrowFunctionParamsObjectLiteral(Node node, Scope s, IdentifierType i)
{
	Branch b = s.branch;
	node.branch = b;
	foreach(item; node.children)
	{
		switch(item.type)
		{
			case NodeType.IdentifierNode:
				s.addVariableOrIdentifier(item.as!IdentifierNode,i);
				item.branch = b;
				break;
			case NodeType.PropertyDefinitionNode:
				item.branch = b;
				item.children[0].branch = b;
				auto rhs = item.children[1];
				switch(rhs.type)
				{
					case NodeType.AssignmentExpressionNode:
						analyseArrowFunctionParamsAssignmentExpression(rhs,s,IdentifierType.Parameter);
						break;
					case NodeType.IdentifierNode:
						s.addVariableOrIdentifier(rhs.as!IdentifierNode,IdentifierType.Parameter);
						rhs.branch = b;
						break;
					case NodeType.ObjectLiteralNode:
						analyseArrowFunctionParamsObjectLiteral(rhs,s,IdentifierType.Parameter);
						break;
					case NodeType.ArrayLiteralNode:
						analyseArrowFunctionParamsArrayLiteral(rhs,s,IdentifierType.Parameter);
						break;
					default: assert(0);
				}
				break;
			case NodeType.CoverInitializedName:
				item.branch = b;
				item.children[0].branch = b;
				if (i == IdentifierType.Parameter)
					s.addVariable(Variable(item.children[0].as!IdentifierNode,IdentifierType.Parameter));
				auto rhs = item.children[1];
				switch(rhs.type)
				{
					case NodeType.AssignmentExpressionNode:
						analyseArrowFunctionParamsAssignmentExpression(rhs,s,IdentifierType.Identifier);
						break;
					case NodeType.IdentifierNode:
						s.addVariableOrIdentifier(rhs.as!IdentifierNode,IdentifierType.Identifier);
						rhs.branch = b;
						break;
					case NodeType.ObjectLiteralNode:
						analyseArrowFunctionParamsObjectLiteral(rhs,s,IdentifierType.Identifier);
						break;
					case NodeType.ArrayLiteralNode:
						analyseArrowFunctionParamsArrayLiteral(rhs,s,IdentifierType.Identifier);
						break;
					case NodeType.StringLiteralNode:
					case NodeType.BinaryLiteralNode:
					case NodeType.OctalLiteralNode:
					case NodeType.DecimalLiteralNode:
					case NodeType.HexLiteralNode:
						rhs.branch = b;
						break;
					default: version(unittest) throw new UnitTestException([format("Didn't expect %s",rhs.type)]);assert(0);
				}
				break;
			default: assert(0);
		}
	}
}
private void analyseArrowFunctionParamsArrayLiteral(Node node, Scope s, IdentifierType i)
{
	Branch b = s.branch;
	node.branch = b;
	foreach(item; node.children)
	{
		switch(item.type)
		{
			case NodeType.IdentifierNode:
				s.addVariableOrIdentifier(item.as!IdentifierNode,i);
				item.branch = b;
				break;
			case NodeType.ArrayLiteralNode:
				analyseArrowFunctionParamsArrayLiteral(item,s,i);
				break;
			case NodeType.ObjectLiteralNode:
				analyseArrowFunctionParamsObjectLiteral(item,s,i);
				break;
			case NodeType.AssignmentExpressionNode:
				analyseArrowFunctionParamsAssignmentExpression(item,s,i);
				break;
			case NodeType.SpreadElementNode:
				s.addVariableOrIdentifier(item.children[0].as!IdentifierNode,i);
				item.branch = b;
				item.children[0].branch = b;
				break;
			case NodeType.ElisionNode:
				item.branch = b;
				break;
			default: assert(0);
		}
	}
}
private void analyseArrowFunctionParamsAssignmentExpression(Node node, Scope s, IdentifierType i)
{
	Branch b = s.branch;
	node.branch = b;
	auto lhs = node.children[0];
	node.children[1].branch = b;
	switch (lhs.type)
	{
		case NodeType.ArrayLiteralNode:
			analyseArrowFunctionParamsArrayLiteral(lhs,s,i);
			break;
		case NodeType.ObjectLiteralNode:
			analyseArrowFunctionParamsObjectLiteral(lhs,s,i);
			break;
		case NodeType.IdentifierNode:
			s.addVariableOrIdentifier(lhs.as!IdentifierNode,i);
			lhs.branch = b;
			break;
		default: assert(0);
	}
	analyse(node.children[2],s);
}
private void analyseArrowFunctionParams(Node node, Scope s)
{
	Branch b = s.branch;
	node.branch = b;
	if (node.type == NodeType.IdentifierNode)
		s.addVariable(Variable(node.as!IdentifierNode,IdentifierType.Parameter));
	else
	{
		foreach(item; node.children)
		{
			switch(item.type)
			{
				case NodeType.SpreadElementNode:
					item.branch = b;
					s.addVariable(Variable(item.children[0].as!IdentifierNode,IdentifierType.Parameter));
					item.children[0].branch = b;
					break;
				case NodeType.ExpressionNode:
					item.branch = b;
					foreach(e; item.children)
					{
						switch(e.type)
						{
							case NodeType.IdentifierNode:
								s.addVariable(Variable(e.as!IdentifierNode,IdentifierType.Parameter));
								e.branch = b;
								break;
							case NodeType.ObjectLiteralNode:
								analyseArrowFunctionParamsObjectLiteral(e,s,IdentifierType.Parameter);
								break;
							case NodeType.ArrayLiteralNode:
								analyseArrowFunctionParamsArrayLiteral(e,s,IdentifierType.Parameter);
								break;
							case NodeType.AssignmentExpressionNode:
								analyseArrowFunctionParamsAssignmentExpression(e,s,IdentifierType.Parameter);
								break;
							default: assert(0);
						}
					}
					break;
				default: assert(0);
			}
		}
	}
}
private int calcHints(Node node)
{
	switch (node.type)
	{
		case NodeType.ErrorNode:
		case NodeType.ContinueStatementNode:
		case NodeType.BreakStatementNode:
		case NodeType.LabelledStatementNode:
		case NodeType.VariableStatementNode:
		case NodeType.IfStatementNode:
		case NodeType.SwitchStatementNode:
		case NodeType.DoWhileStatementNode:
		case NodeType.WhileStatementNode:
		case NodeType.ForStatementNode:
		case NodeType.WithStatementNode:
		case NodeType.CatchStatementNode:
		case NodeType.FinallyStatementNode:
		case NodeType.TryStatementNode:
		case NodeType.ThrowStatementNode:
		case NodeType.DebuggerStatementNode:
		case NodeType.ClassDeclarationNode:
		case NodeType.FunctionDeclarationNode:
		case NodeType.GeneratorDeclarationNode:
		case NodeType.LexicalDeclarationNode:
		case NodeType.ExportDeclarationNode:
		case NodeType.ImportDeclarationNode:
			return Hint.NonExpression;
		case NodeType.ReturnStatementNode:
			return node.children.length == 0 ? Hint.Return : Hint.ReturnValue;
		default:
			return Hint.None;
	}
}
private int getHintMask(Node n)
{
	switch (n.type)
	{
		case NodeType.FunctionBodyNode:
			return ~(Hint.Return | Hint.ReturnValue);
		default:
			return ~(Hint.None);
	}
}
private int analyse(Node node, Scope s = null, Branch b = null)
{
	if (s is null)
		s = new Scope(node);
	if (b is null)
		b = s.branch;
	node.branch = b;
	int hints = calcHints(node);
	int analyseChildren(Node[] ns, Scope s, Branch b = null)
	{
		int hints = Hint.None;
		foreach(idx, n; ns)
		{
			auto h = analyse(n,s,b);
			hints |= n.getHintMask() & h;
		}
		return hints;
	}
	if (node.type == NodeType.FunctionDeclarationNode || node.type == NodeType.FunctionExpressionNode || node.type == NodeType.GeneratorDeclarationNode || node.type == NodeType.GeneratorExpressionNode)
	{
		bool hasName = node.children.length == 3;
		if (hasName)
		{
			s.addVariable(Variable(node.children[0].as!IdentifierNode,IdentifierType.Function));
			node.children[0].branch = b;
		}
		auto funcBody = node.children[hasName ? 2 : 1];
		auto newScope = s.newScope(funcBody);

		analyseFormalParameterList(node.children[hasName ? 1 : 0],newScope);
		analyse(funcBody,newScope);
	} else if (node.type == NodeType.ArrowFunctionNode)
	{
		auto funcBodyStmt = node.children[1];
		auto newScope = s.newScope(funcBodyStmt);

		analyseArrowFunctionParams(node.children[0],newScope);
		analyse(funcBodyStmt,newScope);
	} 
	else if (node.type == NodeType.ClassGetterNode)
	{
		node.children[0].branch = b;
		auto funcBody = node.children[1];
		auto newScope = s.newScope(funcBody);
		analyse(funcBody,newScope);
	}
	else if (node.type == NodeType.ClassMethodNode || node.type == NodeType.ClassGeneratorMethodNode)
	{
		node.children[1].branch = b;
		auto funcBody = node.children[2];
		auto newScope = s.newScope(funcBody);
		analyseFormalParameterList(node.children[0],newScope);
		analyse(funcBody,newScope);
	}
	else if (node.type == NodeType.ClassSetterNode)
	{
		node.children[0].branch = b;
		node.children[1].branch = b;
		auto funcBody = node.children[2];
		auto newScope = s.newScope(funcBody);

		analyseClassSetterParam(node.children[1],newScope);
		analyse(funcBody,newScope);
	}
	else if (node.type == NodeType.VariableStatementNode)
	{
		foreach(decl; node.children)
		{
			decl.branch = b;
			switch(decl.children[0].type)
			{
				case NodeType.ArrayBindingPatternNode:
					analyseArrayBindingPatternNode(decl.children[0],s,b,IdentifierType.Variable);
					break;
				case NodeType.ObjectBindingPatternNode:
					analyseObjectBindingPatternNode(decl.children[0],s,b,IdentifierType.Variable);
					break;
				case NodeType.IdentifierNode:
					s.addVariable(Variable(decl.children[0].as!IdentifierNode,IdentifierType.Variable));
					decl.children[0].branch = b;
					break;
				default: break;
			}
			if (decl.children.length == 2)
				analyse(decl.children[1],s,b);
		}
	}
	else if (node.type == NodeType.IdentifierNode)
	{
		s.addIdentifier(Identifier(node.as!IdentifierNode));
	}
	else if (node.type == NodeType.IfStatementNode)
	{
		analyse(node.children[0],s,b);
		analyse(node.children[1],s,b.newBranch(node.children[1]));
		if (node.children.length == 3)
			analyse(node.children[2],s,b.newBranch(node.children[2]));
	} else if (node.type == NodeType.SwitchStatementNode)
	{
		analyse(node.children[0],s,b);
		foreach(c; node.children[1..$])
		{
			c.branch = b;
			if (c.type == NodeType.CaseNode)
			{
				c.children[0].branch = b;
				if (c.children.length > 1)
					analyse(c.children[1],s,b.newBranch(c.children[1]));
			} else if (c.children.length > 0)
				analyse(c.children[0],s,b.newBranch(c.children[0]));
		}
	} else if (node.type == NodeType.ForStatementNode)
	{
		analyseChildren(node.children[0..$-1],s,b);
		auto stmt = node.children[$-1];
		analyse(node.children[$-1],s,b.newBranch(stmt));
	} else if (node.type == NodeType.DoWhileStatementNode)
	{
		analyse(node.children[0],s,b.newBranch(node.children[0]));
		analyse(node.children[1],s,b);
	} else if (node.type == NodeType.WhileStatementNode)
	{
		analyse(node.children[1],s,b.newBranch(node.children[1]));
		analyse(node.children[0],s,b);
	} else if (node.type == NodeType.ArrayBindingPatternNode)
		analyseArrayBindingPatternNode(node,s,b,IdentifierType.Identifier);
	else if (node.type == NodeType.ObjectBindingPatternNode)
		analyseObjectBindingPatternNode(node,s,b,IdentifierType.Identifier);
	else if (node.type == NodeType.ReturnStatementNode)
	{
		b.addHint(node.children.length == 0 ? Hint.Return : Hint.ReturnValue);
		hints |= analyseChildren(node.children,s,b);
	}
	else
	{
		hints |= analyseChildren(node.children,s,b);
	}
	node.hints = hints;
	return node.getHintMask() & hints;
}
AnalysisResult analyseNode(Node root)
{
	Scope s = new Scope(root);
	Branch b = s.branch;
	Scope all = s;
	Branch trunk = b;
	analyse(root,s,b);
	findGlobals(s);
	auto ar = AnalysisResult(all,trunk);
	//ar.fetchUnresolvedIdentifiers();
	return ar;
}
@("Scopes")
unittest
{
	void assertIsLeafScope(Scope s, Scope parent = null, in string file = __FILE__, in size_t line = __LINE__)
	{
		if (parent is null)
			s.parent.shouldBeNull(file,line);
		else
			s.parent.shouldBeObject(parent,file,line);
		s.children.length.shouldEqual(0,file,line);
		s.branch.shouldNotBeNull(file,line);
		s.entry.shouldNotBeNull(file,line);
		s.branch.scp.shouldBeObject(s,file,line);
		s.branch.parent.shouldBeNull(file,line);
		s.branch.entry.shouldBeObject(s.entry,file,line);
	}
	{auto s = getScope(`var a;`);
	assertIsLeafScope(s);
	s.branch.children.length.shouldEqual(0);
	s.entry.emit.shouldEqual(`var a;`);}

	void testFirstChildScopeToContainReturn1(string js, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto s = getScope(js,file,line);
		s.parent.shouldBeNull(file,line);
		s.children.length.shouldEqual(1,file,line);
		s.children[0].parent.shouldBeObject(s,file,line);
		assertIsLeafScope(s.children[0],s);
		s.children[0].entry.emit.shouldEqual(`{return 1}`,file,line);
	}

	testFirstChildScopeToContainReturn1(`function abc(){return 1}`);
	testFirstChildScopeToContainReturn1(`function*abc(){return 1}`);
	testFirstChildScopeToContainReturn1(`a=function(){return 1}`);
	testFirstChildScopeToContainReturn1(`a=function*(){return 1}`);
	testFirstChildScopeToContainReturn1(`a=()=>{return 1}`);
	testFirstChildScopeToContainReturn1("class abc{m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{*m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{*m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{set m(abc){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{set m(abc){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{get m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{get m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static *m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static *m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static set m(abc){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static set m(abc){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static get m(){return 1}}");
	testFirstChildScopeToContainReturn1("class abc{static get m(){return 1}}");
}
@("Branches")
unittest
{
	auto b = getBranch(`if(a)b;else c;`);
	b.children.length.shouldEqual(2);
	b.children[0].entry.emit.shouldEqual(`b`);
	b.children[1].entry.emit.shouldEqual(`c`);

	b = getBranch(`if(a){b}else{c}`);
	b.children.length.shouldEqual(2);
	b.children[0].entry.emit.shouldEqual(`{b}`);
	b.children[1].entry.emit.shouldEqual(`{c}`);

	b = getBranch(`if(a)b;`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`b`);

	b = getBranch(`if(a){b}`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`{b}`);

	b = getBranch(`switch(a){case 1:b;case 2:{c};case 3:case 4:break;default:d}`);
	b.children.length.shouldEqual(4);
	b.children[0].entry.emit.shouldEqual(`b`);
	b.children[1].entry.emit.shouldEqual(`{c}`);
	b.children[2].entry.emit.shouldEqual(`break`);
	b.children[3].entry.emit.shouldEqual(`d`);

	b = getBranch(`switch(a){case 1:b;default:}`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`b`);

	b = getBranch(`for(;;){a}`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`{a}`);

	b = getBranch(`for(;;)a;`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`a`);

	b = getBranch(`do a;while(1);`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`a`);

	b = getBranch(`do{a}while(1);`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`{a}`);

	b = getBranch(`while(1)a;`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`a`);

	b = getBranch(`while(1){a}`);
	b.children.length.shouldEqual(1);
	b.children[0].entry.emit.shouldEqual(`{a}`);
}
@("Variable Statements")
unittest
{
	getScope(`var a;`).assertVariables!(IdentifierType.Variable)(["a"]);
	getScope(`var a=t;`)
		.assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["t"]);
	getScope(`var a,b;`).assertVariables!(IdentifierType.Variable)(["a","b"]);
	getScope(`var [a]=b;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b"]);
	getScope(`var [a,b]=c;`).assertVariables!(IdentifierType.Variable)(["a","b"])
		.assertIdentifiers(["c"]);
	// no compiles
	/*s = getScope(`var [...rest]=b;`);
	s.variables.length.shouldEqual(1);
	s.variables[0].node.identifier.shouldEqual(`rest`);
	s.variables[0].type.shouldEqual(IdentifierType.Variable);*/

	getScope(`var [a=c]=b;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["c","b"]);
	getScope(`var [[a]]=b;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b"]);
	getScope(`var [{a}]=b;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b"]);
	getScope(`var [{a}=b]=c;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b","c"]);
	getScope(`var [[a]=b]=c;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b","c"]);
	getScope(`var [,,[a]=b,,c]=d;`).assertVariables!(IdentifierType.Variable)(["a","c"])
		.assertIdentifiers(["b","d"]);
	getScope(`var {a};`).assertVariables!(IdentifierType.Variable)(["a"]);
	getScope(`var {a}=b;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b"]);
	getScope(`var {a=b}=c;`).assertVariables!(IdentifierType.Variable)(["a"])
		.assertIdentifiers(["b","c"]);
	getScope(`var {a:b=c}=d;`).assertVariables!(IdentifierType.Variable)(["b"])
		.assertIdentifiers(["c","d"]);
	getScope(`var {a:{b}}=c;`).assertVariables!(IdentifierType.Variable)(["b"])
		.assertIdentifiers(["c"]);
	getScope(`var {a:[b]}=c;`).assertVariables!(IdentifierType.Variable)(["b"])
		.assertIdentifiers(["c"]);
	getScope(`var {a:{b}=c}=d;`).assertVariables!(IdentifierType.Variable)(["b"])
		.assertIdentifiers(["c","d"]);
	getScope(`var {a:[b]=c}=d;`).assertVariables!(IdentifierType.Variable)(["b"])
		.assertIdentifiers(["c","d"]);
	getScope(`var a,b=c,[d]=e,[f,g]=h,[i=j]=k,[[l]]=m,[{n}]=o,[{p}=q]=r,[[s]=t]=u,[,,[v]=w,,x]=y,{aa},{ab}=ac,{ad=ae}=af,{ag:ah=ai}=aj,{ak:{al}}=am,{an:[ao]}=ap,{aq:{ar}=as}=at,{au:[av]=aw}=ax;`)
		.assertVariables!(IdentifierType.Variable)([`a`,`b`,`d`,`f`,`g`,`i`,`l`,`n`,`p`,`s`,`v`,`x`,`aa`,`ab`,`ad`,`ah`,`al`,`ao`,`ar`,`av`])
		.assertIdentifiers(["c","e","h","j","k","m","o","q","r","t","u","w","y","ac","ae","af","ai","aj","am","ap","as","at","aw","ax"]);
}
@("ObjectLiteral")
unittest
{
    // todo: here we prepend the objectbindingpattern with an identifier to force parsing as an ObjectBindingPattern. Babel also has this problem. Don't know if bug or not.
    // // todo: WRONG!!
	getScope(`e,{a}=b;`).assertIdentifiers(["e","a","b"]);
	getScope(`e,{a,b}=c;`).assertIdentifiers(["e","a","b","c"]);
	getScope(`e,{a=b}=c;`).assertIdentifiers(["e","a","b","c"]);
	getScope(`e,{a:b}=c;`).assertIdentifiers(["e","a","b","c"]);
	getScope(`e,{a:b=c}=d;`).assertIdentifiers(["e","a","b","c","d"]);
	getScope(`e,{a:{b}}=c;`).assertIdentifiers(["e","a","b","c"]);
	getScope(`e,{a:[b]}=c;`).assertIdentifiers(["e","a","b","c"]);
	getScope(`e,{a:{b}=c}=d;`).assertIdentifiers(["e","a","b","c","d"]);
	getScope(`e,{a:[b]=c}=d;`).assertIdentifiers(["e","a","b","c","d"]);
}
@("ArrayLiteral")
unittest
{
	getScope(`[a]=b;`).assertIdentifiers(["a","b"]);
	getScope(`[a,b]=c;`).assertIdentifiers(["a","b","c"]);

	// TODO: no work
	//s = getScope(`[...rest]=b;`);
	//assertIdentifier(s,0,"rest");
	//assertIdentifier(s,1,"b");
	getScope(`[a=c]=b;`).assertIdentifiers(["a","c","b"]);
	getScope(`[[a]]=b;`).assertIdentifiers(["a","b"]);
	getScope(`[{a}]=b;`).assertIdentifiers(["a","b"]);
	getScope(`[{a}=b]=c;`).assertIdentifiers(["a","b","c"]);
	getScope(`[[a]=b]=c;`).assertIdentifiers(["a","b","c"]);
	getScope(`[,,[a]=b,,c]=d;`).assertIdentifiers(["a","b","c","d"]);
}
@("Functions")
unittest
{
	getScope(`function b(a){}`).assertVariables!(IdentifierType.Function)(["b"]);
	getFirstChildScope(`function b(a){}`).assertVariables!(IdentifierType.Parameter)(["a"]);
	getFirstChildScope(`function b(a,b){}`).assertVariables!(IdentifierType.Parameter)(["a","b"]);
	getFirstChildScope(`function b(...a){}`).assertVariables!(IdentifierType.Parameter)(["a"]);
	getFirstChildScope(`function b([a]){}`).assertVariables!(IdentifierType.Parameter)(["a"]);
	getFirstChildScope(`function b({a}){}`).assertVariables!(IdentifierType.Parameter)(["a"]);
	getFirstChildScope(`function b({a}=d){}`).assertVariables!(IdentifierType.Parameter)(["a"]);
	getFirstChildScope(`function b([a]=d){}`).assertVariables!(IdentifierType.Parameter)(["a"]);
}
@("ArrowFunction")
unittest
{
	getFirstChildScope(`(a)=>{}`).assertVariables!(IdentifierType.Parameter)(["a"]);
	getFirstChildScope(`(a,b)=>{}`).assertVariables!(IdentifierType.Parameter)(["a","b"]);
	getFirstChildScope(`(a=b)=>{}`).assertVariables!(IdentifierType.Parameter)(["a"])
		.assertIdentifiers(["b"]);
	getFirstChildScope(`({a})=>{c}`).assertVariables!(IdentifierType.Parameter)(["a"])
		.assertIdentifiers(["c"]);
	getFirstChildScope(`({a,b:c,d:e=f,g:{h},i:{j}=k,l:[m],n:[o]=p})=>{q}`)
		.assertVariables!(IdentifierType.Parameter)(["a","c","e","h","j","m","o"])
		.assertIdentifiers(["f","k","p","q"]);
	getFirstChildScope(`({a=b,c=d=e,f={g},h={i},j=[k]})=>{q}`)
		.assertVariables!(IdentifierType.Parameter)(["a","c","f","h","j"])
		.assertIdentifiers(["b","d","e","g","i","k","q"]);
	getFirstChildScope(`({a}=b)=>{c}`).assertVariables!(IdentifierType.Parameter)(["a"])
		.assertIdentifiers(["b","c"]);
	getFirstChildScope(`([a])=>{c}`).assertVariables!(IdentifierType.Parameter)(["a"])
		.assertIdentifiers(["c"]);
	getFirstChildScope(`([a,,[b],,{c},[d]=e,{f}=g,...rest])=>{n}`)
		.assertVariables!(IdentifierType.Parameter)(["a","b","c","d","f","rest"])
		.assertIdentifiers(["e","g","n"]);
	getFirstChildScope(`([a]=b)=>{c}`).assertVariables!(IdentifierType.Parameter)(["a"])
		.assertIdentifiers(["b","c"]);
	getFirstChildScope(`(...rest)=>{c}`).assertVariables!(IdentifierType.Parameter)(["rest"])
		.assertIdentifiers(["c"]);

}
@("Hints")
unittest
{
	getFirstChildScope(`function a() { return b }`).entry.hints.get.shouldEqual(Hint.ReturnValue);
	getFirstChildScope(`function a() { return }`).entry.hints.get.shouldEqual(Hint.Return);
	getScope(`function a() { return b }`).entry.hints.get.shouldEqual(Hint.NonExpression);
}
/*

//TODO: Also test this: `var [a,{b,f}={b:[,,b]={g:t},f},c,k={o:[,,l]}={op}]=d;`
// It should add a,b,f,c,k to the variable list
// and t, d, f and op to the identifier reference list
// in a objectbindingpattern a bindingelement 

/++
	Returns list of identifiers that do not refer to any
	declared variable (but to an global declared elsewhere).

	This is used in variable renaming. A variable cannot
	be renamed to a (used) global identifier.

	Common global identifiers include jQuery, or sanalysey any
	assignment to the window object.
+/
string[] fetchUnresolvedIdentifiers(ref AnalysisResult ar)
{
	ar.unresolvedIdentifiers = es5.node.getNonLocalIdentifiers(ar.scp);
	return ar.unresolvedIdentifiers;
}
unittest
{
	/// test scopes
	auto getScopes(string input)
	{
		auto p = ES6(input);
		auto nodes = createNode(p);
		return analyseNodes(nodes).scp;
	}
	auto s1 = getScopes(`var a = 6; function bla(){ huppa = 4; };`);
	assert(s1.entry.isA!("ES6"));
	assert(s1.scopes[0].entry.isA!"ES6.FunctionBody");
	assert(s1.scopes[0].entry.parent.parent.children[0].matches == ["bla"]);

	auto s2 = getScopes(`var a = (boo)=>{ huppa = 4};`);
	assert(s2.entry.isA!("ES6"));
	assert(s2.scopes[0].entry.isA!"ES6.FunctionBody");
	assert(s2.scopes[0].entry.parent.parent.parent.children[0].matches == ["(","boo",")"]);
	//assert(s2.scopes[0].identifiers == ["boo","huppa"]);

	auto s3 = getScopes(`var f = { a: function (b) { d = b; } };`);
	assert(s3.entry.isA!("ES6"));
	assert(s3.scopes[0].entry.isA!"ES6.FunctionBody");
	
	auto s4 = getScopes(`var f = { a: function name(b) { d = b; } };`);
	assert(s4.entry.isA!("ES6"));
	assert(s4.scopes[0].entry.isA!"ES6.FunctionBody");
	// TODO test MethodDefinition
}
unittest
{
	/// test branches
	auto getBranches(string input)
	{
		auto p = ES6(input);
		auto nodes = createNode(p);
		return analyseNodes(nodes).trunk;
	}
	auto getFirstFunctionsBranch(Branch b)
	{
		return b.entry.scp.scopes[0].entry.branch;
	}
	auto assertJs(Node node, string expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		import es5.tojs;
		import es5.testhelpers;
		string got = node.toJS();
		if (got == expected)
			return;
		Message m;
		m.writeln("Branch Failure");
		m.writeln("Expected:");
		m.writeln(expected);
		m.writeln("Got:");
		m.writeln(got);
		failed(m,file,line);
	}
	auto b1 = getBranches("if (c) d = 5; e = 5;");
	assert(b1.entry.isA!"ES6");
	assert(b1.children[0].entry.matches == ["d", "=", "5"]);

	auto b2 = getBranches("if (c) d = 5; else f = 5; e = 5;");
	assert(b2.entry.isA!"ES6");
	assert(b2.children[0].entry.matches == ["d", "=", "5"]);
	assert(b2.children[1].entry.matches == ["f", "=", "5"]);

	auto b3 = getBranches("if (c) d = 5; else if (g) f = 5; else h = 5; e = 5;");
	assert(b3.entry.isA!"ES6");
	assert(b3.children[0].entry.matches == ["d", "=", "5"]);
	assert(b3.children[1].entry.matches == ["(", "g", ")", "f", "=", "5", "else", "h", "=", "5"]);
	assert(b3.children[1].children[0].entry.matches == ["f", "=", "5"]);
	assert(b3.children[1].children[1].entry.matches == ["h", "=", "5"]);

	// if all branches are a ReturningBranch, the parent branch must also be
	auto b4 = getFirstFunctionsBranch(getBranches("function def (a) { if (c) if (d) return 4; else return 3; }"));
	assert(b4.children[0].children[0].entry.hint & Hint.ReturningBranch);
	assert(b4.children[0].children[1].entry.hint & Hint.ReturningBranch);
	assert(b4.children[0].entry.hint & Hint.ReturningBranch);
	assert(b4.children[0].entry.hint & Hint.DeducedReturingBranch);

	auto b5 = getFirstFunctionsBranch(getBranches("function def(a) { if (c) if (d) return 4; else if (c) { d = 3; return 3 }; else { e = 5; return 5; }}"));
	assert(b5.children[0].children[0].entry.hint & Hint.ReturningBranch);
	assert(b5.children[0].children[1].entry.hint & Hint.ReturningBranch);
	assert(b5.children[0].entry.hint & Hint.ReturningBranch);
	assert(b5.children[0].entry.hint & Hint.DeducedReturingBranch);
	//
	//	Todo: What about try-catch-finally, while, for??
	//
	auto b6 = getFirstFunctionsBranch(getBranches("function bla() { switch(a){ case 7: d=5; case 5: d=7; default: d=3; }; }"));
	assert(b6.entry.getNthChild(2).hint & Hint.NonExpression);
	assert(b6.children.length == 3);
	assert(b6.children[0].entry.matches == ["d", "=", "5"]);
	assert(b6.children[1].entry.matches == ["d", "=", "7"]);
	assert(b6.children[2].entry.matches == ["d", "=", "3"]);

	auto b7 = getFirstFunctionsBranch(getBranches("function bla() { switch(a){ case 7: if(c) return 7; return 5; case 5: break; default: return 5;}; }"));
	assert(b7.entry.getNthChild(2).hint & Hint.NonExpression);
	assert(b7.children.length == 3);
	assert(b7.children[0].entry.hint & Hint.ReturningBranch);
	assert(!(b7.children[1].entry.hint & Hint.ReturningBranch));
	assert(b7.children[2].entry.hint & Hint.ReturningBranch);
	assertJs(b7.children[2].entry,"return 55").shouldThrow();
	assertJs(b7.children[2].entry,"return 5");
	assertJs(b7.children[2].entry.parent,"default: return 5");
	assert(b7.children[0].entry.parent.isA!"ES6.CaseClause");
	assert(b7.children[1].entry.parent.isA!"ES6.CaseClause");
	assert(b7.children[2].entry.parent.parent.parent.isA!"ES6.CaseBlock");
}
unittest
{
	/// test hints
	auto getNodes(string js)
	{
		auto p = ES6(js);
		auto nodes = createNode(p);
		analyseNodes(nodes);
		return nodes;
	}
	void assertPredHint(alias errMsg, alias pred)(Node node, Hints h, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.traits : EnumMembers;
		import std.conv : to;
		import std.range : join;
		import std.stdio : writeln;
		string[] hints;
		foreach(M; EnumMembers!Hint)
		{
			if (h & M)
			{
				if (pred(node.hint,M))
					hints ~= M.to!string;
			}
		}
		if (hints.length == 0)
			return;
		Message m;
		m.writeln(node);
		m.writeln(errMsg~":\n"~hints.map!(h=>"\t"~h).join("\n"));
		failed(m,file,line);
	}
	void assertNoHint(Node node, Hints h2)
	{
		assertPredHint!("Node shouldn't have hints",(Hints s, Hint h)=>(s & h))(node,h2);
	}
	void assertHint(Node node, Hints h)
	{
		assertPredHint!("Node should have hints",(Hints s, Hint h)=>!(s & h))(node,h);
	}
	auto n0 = getNodes(`function f(){if (a) return d; if (c) return 7;}`);
	assertHint(n0.getChild("ES6.FunctionBody"),Hints(Hint.DeducedReturingBranch)).shouldThrow;
	assertNoHint(n0.getChild("ES6.ReturnStatement"),Hints(Hint.Return | Hint.ReturningBranch)).shouldThrow;


	auto n1 = getNodes(`function f(){if (a) return d; if (c) return 7;}`);
	assertNoHint(n1.getChild("ES6.FunctionBody"),Hints(Hint.DeducedReturingBranch));
	assertHint(n1.getChild("ES6.ReturnStatement"),Hints(Hint.Return | Hint.ReturningBranch));

	auto n2 = getNodes(`function f(){if (a) return d; else return 7;}`);
	assertHint(n2.getChild("ES6.StatementListReturn"),Hints(Hint.Return | Hint.ReturningBranch | Hint.DeducedReturingBranch));
	assertHint(n2.getChild("ES6.IfStatementReturn"),Hints(Hint.Return | Hint.ReturningBranch | Hint.DeducedReturingBranch));
	assertHint(n2.getChild("ES6.ReturnStatement"),Hints(Hint.Return | Hint.ReturningBranch));

	auto n3 = getNodes(`function f(){if (a) { if (c) return d; else return 5; } }`);
	assertNoHint(n1.getChild("ES6.StatementListReturn"),Hints(Hint.DeducedReturingBranch));
	
	auto n4 = getNodes(`for (var a in b) { if (!a) { continue; } }`);
	assertHint(n4.getChild("ES6.ContinueStatement"),Hints(Hint.NonExpression));
	assertHint(n4.getChild("ES6.IfStatement"),Hints(Hint.NonExpression));

	auto n5 = getNodes(`if (g) for (var a in b) g = 5;`);
	assertHint(n5.getChild("ES6.IfStatement"),Hints(Hint.NonExpression));

	auto n6 = getNodes(`if (g) for (var a in b) g = 5; else dd = 5;`);
	assertHint(n6.getChild("ES6.IfStatement"),Hints(Hint.NonExpression));

	auto n7 = getNodes(`for (var i = 0; i < e.length; i++) { if (!e[i]) { g = 6; break; } }`);
	assertHint(n7.getChild("ES6.IfStatement"),Hints(Hint.NonExpression));
	assertHint(n7.getChild("ES6.IterationStatement"),Hints(Hint.NonExpression));
}
@("assertUnresolvedIdentifiers")
unittest
{
	import unit_threaded;
	import es5.testhelpers;
	void assertUnresolvedIdentifiers(string js, string[] expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.stdio : writeln;
		auto p = ES6(js);
		auto nodes = createNode(p);
		auto a = analyseNodes(nodes);
		auto got = a.unresolvedIdentifiers;
		if (got == expected)
			return;
		Message m;
		m.writeln(p);
		m.writeln("Error in assertUnresolvedIdentifiers:");
		m.writeln("Expected:");
		m.writeln(expected);
		m.writeln("Got:");
		m.writeln(got);
		failed(m,file,line);
	}
	assertUnresolvedIdentifiers(
		"function hup() { var a; };",
		["a"]
	).shouldThrow;
	assertUnresolvedIdentifiers(
		"function hup() { var foo = 5; return foo.bla; }; var bla = hup()*p;",
		["p"]
	);
	assertUnresolvedIdentifiers(
		"function hup() { var foo = 5; k = function() { } return foo.bla*t; };",
		["k","t"]
	);
	assertUnresolvedIdentifiers(
		"var bla = (ops)=>{ return (bb,ol)=>{ bb.bla = ops*tt*pp*ol}; }",
		["tt","pp","pp"] // TODO: don't know why pp shows up twice
	);
}
void reanalyse(Node node)
{
	import std.algorithm : map, reduce;
	Hints childHints;
	if (node.children.length > 0)
		childHints = node.children.map!(n=>n.hint & ~n.getHintMask).reduce!((a,b)=>a | b);
	node.processHints(childHints);
	if (node.parent)
		reanalyse(node.parent);
}
@("reanalyse")
unittest {
	auto n = new Node(ParseTree("ES6.VariableStatement",true),[
		new Node(ParseTree("ES6.BreakStatement",true))
	]);

	n.reanalyse();
	n.hint.shouldEqual(Hints(Hint.VariableDecl));
	n.children[0].reanalyse();
	n.hint.shouldEqual(Hints(Hint.VariableDecl | Hint.NonExpression | Hint.Break));
	n.children[0].hint.shouldEqual(Hints(Hint.Break | Hint.NonExpression));
}
@("analyse")
unittest
{

}*/