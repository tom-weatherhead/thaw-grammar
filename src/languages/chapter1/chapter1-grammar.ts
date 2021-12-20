// tom-weatherhead/thaw-grammar/src/languages/chapter1/chapter1-grammar.ts

import {
	GrammarSymbol,
	IToken,
	/* LexicalState, */ SemanticStackType
} from 'thaw-interpreter-types';

import { createProduction, Name } from 'thaw-interpreter-core';

import { IExpression } from '../../common/domain-object-model/iexpression';
import { IVariable, Variable } from '../../common/domain-object-model/variable';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';
import { FunctionDefinition } from '../../common/domain-object-model/function-definition';
import { IfUsage } from '../../common/domain-object-model/if-usage';
import { SetUsage } from '../../common/domain-object-model/set-usage';
import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { IntegerLiteral } from './domain-object-model/integer-literal';
import { Chapter1OperatorUsage } from './domain-object-model/operator-usage';

export class Chapter1Grammar extends GrammarBase {
	// The grammar from Chapter 1 of Kamin's book: "Programming Languages: An Interpreter-Based Approach" (?)

	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalDefine);
		this.terminals.push(GrammarSymbol.terminalIf);
		this.terminals.push(GrammarSymbol.terminalWhile);
		this.terminals.push(GrammarSymbol.terminalSet);
		this.terminals.push(GrammarSymbol.terminalBegin);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMinus);
		this.terminals.push(GrammarSymbol.terminalMultiply);
		this.terminals.push(GrammarSymbol.terminalDivide);
		this.terminals.push(GrammarSymbol.terminalEquals);
		this.terminals.push(GrammarSymbol.terminalLessThan);
		this.terminals.push(GrammarSymbol.terminalGreaterThan);
		this.terminals.push(GrammarSymbol.terminalPrint);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		this.terminals.push(GrammarSymbol.terminalRandom);
		this.terminals.push(GrammarSymbol.terminalThrow);
		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunDef);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunction);
		this.nonTerminals.push(GrammarSymbol.nonterminalArgList);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariableList);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalValue);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpressionList);
		this.nonTerminals.push(GrammarSymbol.nonterminalOptr);
		this.nonTerminals.push(GrammarSymbol.nonterminalValueOp);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedEntity);

		// From Chapter 1 of Kamin:
		// Input -> Expression
		// Input -> FunDef
		// FunDef -> ( define Function ArgList Expression )
		// ArgList -> ( VariableList )
		// VariableList -> Variable VariableList
		// VariableList -> Lambda
		// Expression -> Value
		// Expression -> Variable
		// Expression -> ( BracketedExpression )
		// BracketedExpression -> if Expression Expression Expression
		// BracketedExpression -> while Expression Expression
		// BracketedExpression -> set Variable Expression
		// BracketedExpression -> begin Expression ExpressionList
		// BracketedExpression -> Optr ExpressionList
		// ExpressionList -> Expression ExpressionList
		// ExpressionList -> Lambda
		// Optr -> Function
		// Optr -> Value-Op
		// Value -> Integer
		// Value-Op -> +
		// Value-Op -> -
		// Value-Op -> *
		// Value-Op -> /
		// Value-Op -> =
		// Value-Op -> <
		// Value-Op -> >
		// Value-Op -> print
		// Function -> Name
		// Variable -> Name
		// Integer -> ...
		// Name -> ...

		// We prevent function definitions from being considered as expressions.

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalStart,
				[GrammarSymbol.nonterminalInput, GrammarSymbol.terminalEOF],
				1
			)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalInput, [GrammarSymbol.nonterminalValue], 2)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalInput,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalBracketedEntity,
					GrammarSymbol.terminalRightBracket
				],
				3
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedEntity,
				[GrammarSymbol.nonterminalBracketedExpression],
				4
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedEntity,
				[GrammarSymbol.nonterminalFunDef],
				5
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalFunDef,
				[
					GrammarSymbol.terminalDefine,
					GrammarSymbol.nonterminalFunction,
					GrammarSymbol.nonterminalArgList,
					GrammarSymbol.nonterminalExpression,
					'#functionDefinition'
				],
				6
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArgList,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalVariableList,
					GrammarSymbol.terminalRightBracket
				],
				7
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariableList,
				[
					GrammarSymbol.nonterminalVariable,
					GrammarSymbol.nonterminalVariableList,
					'#variableList'
				],
				8
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariableList,
				[GrammarSymbol.Lambda, '#emptyVariableList'],
				9
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.nonterminalValue],
				10
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.nonterminalVariable],
				11
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalBracketedExpression,
					GrammarSymbol.terminalRightBracket
				],
				12
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalIf,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpression,
					'#if'
				],
				13
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalWhile,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpression,
					'#while'
				],
				14
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalSet,
					GrammarSymbol.nonterminalVariable,
					GrammarSymbol.nonterminalExpression,
					'#set'
				],
				15
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalBegin,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpressionList,
					'#begin'
				],
				16
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.nonterminalOptr,
					GrammarSymbol.nonterminalExpressionList,
					'#operatorUsage'
				],
				17
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpressionList,
					'#expressionList'
				],
				18
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[GrammarSymbol.Lambda, '#emptyExpressionList'],
				19
			)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalFunction], 20)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalValueOp], 21)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValue,
				[GrammarSymbol.terminalIntegerLiteral],
				22
			)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPlus], 23)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMinus], 24)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMultiply], 25)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalDivide], 26)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalEquals], 27)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLessThan], 28)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalGreaterThan],
				29
			)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPrint], 30)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalFunction, [GrammarSymbol.terminalID], 31)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariable,
				[GrammarSymbol.terminalID, '#variable'],
				32
			)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalRandom], 33)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalThrow], 34)
		);
	}

	public get languageName(): string {
		return 'Chapter 1';
	}

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let variable: IVariable<number>;
		let variableList: IVariable<number>[];
		let expression: IExpression<number>;
		let expression2: IExpression<number>;
		let expression3: IExpression<number>;
		let expressionList: IExpression<number>[];

		switch (action) {
			case '#functionDefinition':
				expression = semanticStack.pop() as IExpression<number>; // The function's body
				variableList = semanticStack.pop() as IVariable<number>[]; // The function's formal argument list
				name = semanticStack.pop() as Name; // The function name
				semanticStack.push(new FunctionDefinition<number>(name, variableList, expression)); // Add line and column?
				break;

			case '#variableList':
				variableList = semanticStack.pop() as IVariable<number>[];
				variable = semanticStack.pop() as Variable<number>;
				variableList.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push([] as IVariable<number>[]); // Add line and column?
				break;

			case '#if':
				expression3 = semanticStack.pop() as IExpression<number>;
				expression2 = semanticStack.pop() as IExpression<number>;
				expression = semanticStack.pop() as IExpression<number>;
				semanticStack.push(new IfUsage<number>(expression, expression2, expression3)); // Add line and column?
				break;

			case '#while':
				expression2 = semanticStack.pop() as IExpression<number>;
				expression = semanticStack.pop() as IExpression<number>;
				semanticStack.push(new WhileUsage<number>(expression, expression2)); // Add line and column?
				break;

			case '#set':
				expression = semanticStack.pop() as IExpression<number>;
				variable = semanticStack.pop() as Variable<number>;
				semanticStack.push(new SetUsage<number>(variable, expression)); // Add line and column?
				break;

			case '#begin':
				expressionList = semanticStack.pop() as IExpression<number>[];
				expression = semanticStack.pop() as IExpression<number>;
				semanticStack.push(new BeginUsage<number>(expression, expressionList)); // Add line and column?
				break;

			case '#operatorUsage':
				expressionList = semanticStack.pop() as IExpression<number>[];
				name = semanticStack.pop() as Name;
				semanticStack.push(new Chapter1OperatorUsage(name, expressionList)); // Add line and column?
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as IExpression<number>[];
				expression = semanticStack.pop() as IExpression<number>;
				expressionList.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push([] as IExpression<number>[]);
				break;

			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(new Variable<number>(name.value, name.line, name.column));
				break;

			default:
				throw new GrammarException(`Unrecognized semantic action: '${action}'`);
		}
	}

	// public override tokenToSymbol(token: IToken): GrammarSymbol {
	// 	// const tokenValueAsString: string = token.tokenValue as string;
	//
	// 	switch (token.tokenType) {
	// 		// case LexicalState.tokenEOF:
	// 		// 	return GrammarSymbol.terminalEOF;
	// 		// case LexicalState.tokenIntLit:
	// 		// 	return GrammarSymbol.terminalIntegerLiteral;
	// 		// case LexicalState.tokenFltLit:
	// 		// 	return GrammarSymbol.terminalFloatLiteral;
	// 		// case LexicalState.tokenStrLit:
	// 		// 	return GrammarSymbol.terminalStringLiteral;
	// 		// case LexicalState.tokenLeftBracket:
	// 		// 	return GrammarSymbol.terminalLeftBracket;
	// 		// case LexicalState.tokenRightBracket:
	// 		// 	return GrammarSymbol.terminalRightBracket;
	// 		// case LexicalState.tokenPlus:
	// 		// 	return GrammarSymbol.terminalPlus;
	// 		// case LexicalState.tokenMinus:
	// 		// 	return GrammarSymbol.terminalMinus;
	// 		// case LexicalState.tokenMult:
	// 		// 	return GrammarSymbol.terminalMultiply;
	// 		// case LexicalState.tokenDiv:
	// 		// 	return GrammarSymbol.terminalDivide;
	// 		// case LexicalState.tokenEqual:
	// 		// 	return GrammarSymbol.terminalEquals;
	// 		// case LexicalState.tokenLess:
	// 		// 	return GrammarSymbol.terminalLessThan;
	// 		// case LexicalState.tokenGreater:
	// 		// 	return GrammarSymbol.terminalGreaterThan;
	//
	// 		case LexicalState.tokenIdent:
	// 			switch (token.tokenValue as string) {
	// 				case 'define':
	// 					return GrammarSymbol.terminalDefine;
	// 				case 'if':
	// 					return GrammarSymbol.terminalIf;
	// 				case 'while':
	// 					return GrammarSymbol.terminalWhile;
	// 				case 'set':
	// 					return GrammarSymbol.terminalSet;
	// 				case 'begin':
	// 					return GrammarSymbol.terminalBegin;
	// 				case 'print':
	// 					return GrammarSymbol.terminalPrint;
	// 				// case 'random': return GrammarSymbol.terminalRandom;
	// 				// case 'throw': return GrammarSymbol.terminalThrow;
	// 				default:
	// 					return GrammarSymbol.terminalID;
	// 			}
	//
	// 		// break;
	//
	// 		default:
	// 			break;
	// 	}
	//
	// 	throw new GrammarException(
	// 		`No grammar symbol matches token ${token.tokenType} ${token.tokenValue}`,
	// 		token.line,
	// 		token.column
	// 	);
	// }

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalThrow: // TODO: Comment this out later.
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(new IntegerLiteral(value, token.line, token.column));
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
				break;
		}
	}
}
