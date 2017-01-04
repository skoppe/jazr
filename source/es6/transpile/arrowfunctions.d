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
module es6.transpile.arrowfunctions;

import es6.nodes;
import es6.scopes;
import std.algorithm : map, each;
import std.array : array;
import es6.analyse;

version (unittest)
{
	import unit_threaded;
	import es6.parser;
	import std.stdio;
	ArrowFunctionNode parseArrowFunction(string input, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto node = parseNode!("parseAssignmentExpression",ArrowFunctionNode)(input,file,line);
		node.analyseNode();
		return node;
	}
	FunctionExpressionNode parseFunctionExpression(string input)
	{
		auto node = parseNode!("parseFunctionExpression",FunctionExpressionNode)(input);
		node.analyseNode();
		return node;
	}
}
ObjectBindingPatternNode objectLiteralToObjectBindingPattern(Node obj)
{
	return new ObjectBindingPatternNode(
		obj.children.map!((child){
			if (child.type == NodeType.IdentifierNode)
				return child;
			if (child.type == NodeType.PropertyDefinitionNode)
				return cast(Node)(new BindingPropertyNode(child.children[0],child.children[1]));
			if (child.type == NodeType.CoverInitializedName)
				return cast(Node)(new SingleNameBindingNode(child.children[0],child.children[1]));
			return cast(Node)(new ErrorNode("not supported",0,0));
		}).array
	);
}
ArrayBindingPatternNode arrayLiteralToArrayBindingPattern(Node arr)
{
	return new ArrayBindingPatternNode(
		arr.children.map!((child){
			if (child.type == NodeType.IdentifierNode)
				return cast(Node)child;
			if (child.type == NodeType.ElisionNode)
				return cast(Node)child;
			if (child.type == NodeType.ArrayLiteralNode)
				return cast(Node)child.arrayLiteralToArrayBindingPattern();
			if (child.type == NodeType.ObjectLiteralNode)
				return cast(Node)child.objectLiteralToObjectBindingPattern();
			if (child.type == NodeType.AssignmentExpressionNode)
			{
				if (child.children.length == 3)
					return cast(Node)(new SingleNameBindingNode(child.children[0],child.children[2]));
				auto a = new AssignmentExpressionNode(child.children[2..$]);
				return cast(Node)new SingleNameBindingNode(child.children[0],a);
			}
			if (child.type == NodeType.SpreadElementNode)
				return cast(Node)new RestElementNode(child.children[0]);
			return cast(Node)new ErrorNode("not supported",0,0);
		}).array
	);
}
FormalParameterListNode arrowFunctionArgumentsToFormalParameterListNode(Node args)
{
	if (args.type == NodeType.ParenthesisNode)
	{
		if (args.children.length == 0)
			return new FormalParameterListNode([]);
		Node[] children;
		auto first = args.children[0];
		if (first.type == NodeType.ExpressionNode)
		{
			first.children.map!((arg){
				switch(arg.type)
				{
					case NodeType.IdentifierNode:
						return arg;
					case NodeType.ObjectLiteralNode:
						return arg.objectLiteralToObjectBindingPattern();
					case NodeType.ArrayLiteralNode:
						return arg.arrayLiteralToArrayBindingPattern();
					case NodeType.AssignmentExpressionNode:
						auto lhs = arg.children[0];
						switch (lhs.type)
						{
							case NodeType.ArrayLiteralNode:
								lhs = lhs.arrayLiteralToArrayBindingPattern();
								break;
							case NodeType.ObjectLiteralNode:
								lhs = lhs.objectLiteralToObjectBindingPattern();
								break;
							case NodeType.IdentifierNode:
								if (arg.children.length == 3)
									return new SingleNameBindingNode(lhs,arg.children[2]);
								auto a = new AssignmentExpressionNode(arg.children[2..$]);
								return new SingleNameBindingNode(lhs,a);
							default:
								return new ErrorNode("not supported",0,0);
						}
						return new BindingElementNode(lhs,arg.children[2]);
					default:
						return new ErrorNode("not supported",0,0);
				}
			}).each!(c => children~=c);
		} else if (first.type == NodeType.SpreadElementNode)
		{
			return new FormalParameterListNode([new RestElementNode(first.children[0])]);
		}
		if (args.children.length == 2)
		{
			auto second = args.children[1];
			children ~= new RestElementNode(second.children[0]);
		}
		return new FormalParameterListNode(children);
	} else
	{
		assert(args.type == NodeType.IdentifierNode);
		return new FormalParameterListNode([args]);
	}
}

FunctionExpressionNode arrowFunctionToFunctionExpression(ArrowFunctionNode a)
{
	return new FunctionExpressionNode(null,
		arrowFunctionArgumentsToFormalParameterListNode(a.children[0]).withBranch(a.children[0].branch),
		//arrowFunctionBodyToFunctionBody(a.children[1])
		a.children[1]
	);
}

@("arrowfunctions")
unittest
{
	void assertArrowFunctionParameterTranspile(string arrow, string funcExpr, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto formalParamList1 = parseArrowFunction(arrow,file,line).arrowFunctionToFunctionExpression.children[0];
		auto formalParamList2 = parseFunctionExpression(funcExpr).children[0];
		assertTreeInternals(formalParamList1);
		auto diff = diffTree(formalParamList1,formalParamList2,file,line);
		assertNoDiff(diff,file,line);
	}
	// TODO until we write the arrowFunctionBodyToFunctionBody still fails the unittests
	/*assertArrowFunctionParameterTranspile(
		`()=>a`,
		`function(){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`a=>a`,
		`function(a){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`(a)=>a`,
		`function(a){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`(a,b,c)=>a`,
		`function(a,b,c){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`(...rest)=>a`,
		`function(...rest){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`(a,...rest)=>a`,
		`function(a,...rest){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`([a,b],{c,d},...rest)=>a`,
		`function([a,b],{c,d},...rest){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`([a,b]=k,{c,d},e=2)=>a`,
		`function([a,b]=k,{c,d},e=2){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`(a=b=c?d:e,f=g&&d)=>{b}`,
		`function(a=b=c?d:e,f=g&&d){return b}`
	);
	assertArrowFunctionParameterTranspile(
		`({h,o:bla,j=4,[p+"54"]:g})=>a`,
		`function({h,o:bla,j=4,[p+"54"]:g}){return a}`
	);
	assertArrowFunctionParameterTranspile(
		`([,,[a,b],l,,,m,k=5,o=n=7,{h:p},...rest])=>{b}`,
		`function([,,[a,b],l,,,m,k=5,o=n=7,{h:p},...rest]){return b}`
	);*/
	// TODO: this still needs to pass...
	/*assertArrowFunctionParameterTranspile(
		`([a,{b,f}={b:[,,b]={g:t},f},c,k={o:[,,l]}={op}])=>{b}`,
		`function([a,{b,f}={b:[,,b]={g:t},f},c,k={o:[,,l]}={op}]){return b}`
	);*/
}