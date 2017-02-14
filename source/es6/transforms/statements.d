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
module es6.transforms.statements;

import es6.nodes;
import es6.scopes;
import es6.analyse;
import es6.eval;
import es6.transforms.expressions;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
}

void removeRedundantBlockStatements(BlockStatementNode node, out Node replacedWith)
{
	if (node.children.length == 1)
	{
		if (node.parent.type == NodeType.IfStatementNode &&
			node.parent.getIndexOfChild(node) == 1 &&
			node.parent.as!(IfStatementNode).hasElsePath)
			return;
		if (node.parent.type == NodeType.TryStatementNode ||
			node.parent.type == NodeType.CatchStatementNode ||
			node.parent.type == NodeType.FinallyStatementNode)
			return;
		replacedWith = node.replaceWith(node.children[0]);
	}
}

@("removeRedundantBlockStatements")
unittest
{
	alias assertRemoveRedundantBlockStatements = assertTransformations!(removeRedundantBlockStatements);

	assertRemoveRedundantBlockStatements(
		`if (a) {b = 6;}`,
		`if (a) b = 6;`
	);
	assertRemoveRedundantBlockStatements(
		`for (;;) {b = 6;}`,
		`for (;;) b = 6;`,
	);
	assertRemoveRedundantBlockStatements(
		`while (a) { b = 6; }`,
		`while (a) b = 6;`,
	);
	assertRemoveRedundantBlockStatements(
		`if (a) { if (b) for(;;); } else { d = 6; }`,
		`if (a) { if (b) for(;;); } else d = 6;`
	);
	assertRemoveRedundantBlockStatements(
		`if (a) { if (b) { if (c) for(;;); } } else { d = 6; }`,
		`if (a) { if (b) if (c) for(;;); } else d = 6;`
	);
	assertRemoveRedundantBlockStatements(
		`if(a) { d=5, p=6 }`,
		`if(a) d=5, p=6;`
	);
	assertRemoveRedundantBlockStatements(
		`try { bla(); } catch (e) { }`,
		`try { bla(); } catch (e) { }`
	);
	assertRemoveRedundantBlockStatements(
		`for (c in a) {
		    b = a[c], S(b) ? this[c] = b : this['_' + c] = b
		}`,
		`for (c in a)
		    b = a[c], S(b) ? this[c] = b : this['_' + c] = b`
	);
	// TODO: these dont work yet due to low ROI
	/*assertRemoveRedundantBlockStatements(
		`if (a) { if (b) { if (c) for(;;); } else i = 7; } else d = 6;`,
		`if (a) if (b) { if (c) for(;;); } else i = 7; else d = 6;`
	);
	assertRemoveRedundantBlockStatements(
		`if (a) { if (b) { if (c) for(;;); } else if (b) k = 7; } else d = 6;`,
		`if (a) { if (b) { if (c) for(;;); } else if (b) k = 7; } else d = 6;`
	);
	assertRemoveRedundantBlockStatements(
		`if (a) { if (b) { if (c) for(;;); } else for(;;); if (b) k = 7; } else d = 6;`,
		`if (a) { if (b) { if (c) for(;;); } else for(;;); if (b) k = 7; } else d = 6;`
	);*/
}