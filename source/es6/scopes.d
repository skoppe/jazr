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
module es6.scopes;

import es6.nodes;

version (unittest)
{
	import unit_threaded;
	import std.stdio;
}

enum IdentifierType
{
	Parameter,
	Variable,
	Function,
	Identifier
}
struct Variable
{
	IdentifierType type;
	IdentifierNode node;
	this(IdentifierNode n, IdentifierType t, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.format;
		assert(t != IdentifierType.Identifier,format("At %s:%s",file,line));
		type = t;
		node = n;
	}
}
struct Identifier
{
	IdentifierNode node;
	this(IdentifierNode n)
	{
		node = n;
	}
}
class Scope
{
	Scope parent;
	Scope[] children;
	Branch branch;
	Node entry;
	/*private*/ Variable[] variables;
	/*private*/ Identifier[] identifiers;
	private int nextFreeIdentifier = -1;
	this(Node e, Scope p = null)
	{
		parent = p;
		entry = e;
		branch = new Branch(this,null,e);
	}
	Node getRoot()
	{
		if (parent)
			return parent.getRoot();
		return entry;
	}
	Scope newScope(Node entry)
	{
		auto s = new Scope(entry,this);
		this.children ~= s;
		return s;
	}
	void addVariable(Variable v)
	{
		variables ~= v;
	}
	void addIdentifier(Identifier i)
	{
		identifiers ~= i;
	}
	Variable[] getVariables()
	{
		return variables;
	}
}
class Branch
{
	Scope scp;
	Branch parent; 		/// The parent branch (can be null)
	Branch[] children;	/// Any nested branches
	Node entry;			/// The node that starts this branch
	this(Scope s, Branch p = null, Node e = null)
	{
		scp = s;
		parent = p;
		entry = e;
	}
	Branch newBranch(Node entry)
	{
		auto b = new Branch(scp,this,entry);
		this.children ~= b;
		return b;
	}
}