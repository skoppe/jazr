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
module es6.transforms.returns;

import es6.nodes;
import es6.scopes;
import es6.transforms.conditionals;
import option;

version(unittest)
{
	import es6.parser;
	import es6.analyse;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
}

bool negateReturningIf(Scope s)
{
	// TODO: test this stuff
	foreach(branch; s.branch.children)
	{
		if (branch.entry.parent.type != NodeType.IfStatementNode ||
			!branch.hasHint!(Hint.Return))
			continue;
		auto ifStmt = branch.entry.parent.as!IfStatementNode;

		auto transfers = ifStmt.parent.opt!(BlockStatementNode).detachStatementsAfter(ifStmt);
		if (transfers.length == Some(0))
		{
			if (ifStmt.truthPath.isSingleStatement())
				ifStmt.replaceWith(ifStmt.condition);
			else
				ifStmt.truthPath.as!(BlockStatementNode).dropAllAfter(NodeType.ReturnStatementNode);
			return true;
		}
		IfPath target;
		if (ifStmt.truthPath.isSingleStatement())
		{
			ifStmt.negateCondition();
			if (ifStmt.hasElsePath)
			{
				ifStmt.truthPath.node = ifStmt.elsePath.node;
				ifStmt.removeElsePath();
			} else
			{
				ifStmt.truthPath.clearStatements();
			}
			target = ifStmt.truthPath;
		} else
		{
			ifStmt.truthPath.as!(BlockStatementNode).dropAllAfter(NodeType.ReturnStatementNode);
			ifStmt.forceElsePath();
			target = ifStmt.elsePath();
		}
		target.addStatements(transfers);
	}
	return false;
}