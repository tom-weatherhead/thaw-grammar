// tom-weatherhead/thaw-grammar/src/languages/chapter1/chapter1-grammar.ts

'use strict';

import { Stack } from 'thaw-common-utilities.ts';

import { LexicalState, Token } from 'thaw-lexical-analyzer';

import { ExpressionList } from '../../common/domain-object-model/expression-list';
import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';
import { Variable } from '../../common/domain-object-model/variable';
import { VariableList } from '../../common/domain-object-model/variable-list';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';
import { FunctionDefinition } from '../../common/domain-object-model/function-definition';
import { IfUsage } from '../../common/domain-object-model/if-usage';
import { OperatorUsage } from '../../common/domain-object-model/operator-usage';
import { SetUsage } from '../../common/domain-object-model/set-usage';
import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Production } from '../../common/production';

import { Symbol } from '../../common/symbol';

import { IntegerLiteral } from './domain-object-model/integer-literal';
import { Chapter1OperatorUsage } from './domain-object-model/operator-usage';

export class Chapter1Grammar extends GrammarBase {
	// The grammar from Chapter 1 of Kamin's book: "Programming Languages: An Interpreter-Based Approach" (?)

	constructor() {
		super(Symbol.nonterminalStart);

		this.terminals.push(Symbol.terminalLeftBracket);
		this.terminals.push(Symbol.terminalRightBracket);
		this.terminals.push(Symbol.terminalDefine);
		this.terminals.push(Symbol.terminalIf);
		this.terminals.push(Symbol.terminalWhile);
		this.terminals.push(Symbol.terminalSet);
		this.terminals.push(Symbol.terminalBegin);
		this.terminals.push(Symbol.terminalPlus);
		this.terminals.push(Symbol.terminalMinus);
		this.terminals.push(Symbol.terminalMultiply);
		this.terminals.push(Symbol.terminalDivide);
		this.terminals.push(Symbol.terminalEquals);
		this.terminals.push(Symbol.terminalLessThan);
		this.terminals.push(Symbol.terminalGreaterThan);
		this.terminals.push(Symbol.terminalPrint);
		this.terminals.push(Symbol.terminalID);
		this.terminals.push(Symbol.terminalIntegerLiteral);
		this.terminals.push(Symbol.terminalRandom);
		this.terminals.push(Symbol.terminalThrow);
		this.terminals.push(Symbol.terminalEOF);

		this.nonTerminals.push(Symbol.nonterminalStart);
		this.nonTerminals.push(Symbol.nonterminalInput);
		this.nonTerminals.push(Symbol.nonterminalExpression);
		this.nonTerminals.push(Symbol.nonterminalFunDef);
		this.nonTerminals.push(Symbol.nonterminalFunction);
		this.nonTerminals.push(Symbol.nonterminalArgList);
		this.nonTerminals.push(Symbol.nonterminalVariableList);
		this.nonTerminals.push(Symbol.nonterminalVariable);
		this.nonTerminals.push(Symbol.nonterminalValue);
		this.nonTerminals.push(Symbol.nonterminalBracketedExpression);
		this.nonTerminals.push(Symbol.nonterminalExpressionList);
		this.nonTerminals.push(Symbol.nonterminalOptr);
		this.nonTerminals.push(Symbol.nonterminalValueOp);
		this.nonTerminals.push(Symbol.nonterminalBracketedEntity);

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
			new Production(
				Symbol.nonterminalStart,
				[Symbol.nonterminalInput, Symbol.terminalEOF],
				1
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalInput,
				[Symbol.nonterminalValue],
				2
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalInput,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalBracketedEntity,
					Symbol.terminalRightBracket
				],
				3
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedEntity,
				[Symbol.nonterminalBracketedExpression],
				4
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedEntity,
				[Symbol.nonterminalFunDef],
				5
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalFunDef,
				[
					Symbol.terminalDefine,
					Symbol.nonterminalFunction,
					Symbol.nonterminalArgList,
					Symbol.nonterminalExpression,
					'#functionDefinition'
				],
				6
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalArgList,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalVariableList,
					Symbol.terminalRightBracket
				],
				7
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalVariableList,
				[
					Symbol.nonterminalVariable,
					Symbol.nonterminalVariableList,
					'#variableList'
				],
				8
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalVariableList,
				[Symbol.Lambda, '#emptyVariableList'],
				9
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpression,
				[Symbol.nonterminalValue],
				10
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpression,
				[Symbol.nonterminalVariable],
				11
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpression,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalBracketedExpression,
					Symbol.terminalRightBracket
				],
				12
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.terminalIf,
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpression,
					'#if'
				],
				13
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.terminalWhile,
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpression,
					'#while'
				],
				14
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.terminalSet,
					Symbol.nonterminalVariable,
					Symbol.nonterminalExpression,
					'#set'
				],
				15
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.terminalBegin,
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpressionList,
					'#begin'
				],
				16
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.nonterminalOptr,
					Symbol.nonterminalExpressionList,
					'#operatorUsage'
				],
				17
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpressionList,
				[
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpressionList,
					'#expressionList'
				],
				18
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpressionList,
				[Symbol.Lambda, '#emptyExpressionList'],
				19
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalOptr,
				[Symbol.nonterminalFunction],
				20
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalOptr,
				[Symbol.nonterminalValueOp],
				21
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValue,
				[Symbol.terminalIntegerLiteral],
				22
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalPlus],
				23
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalMinus],
				24
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalMultiply],
				25
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalDivide],
				26
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalEquals],
				27
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalLessThan],
				28
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalGreaterThan],
				29
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalPrint],
				30
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalFunction,
				[Symbol.terminalID],
				31
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalVariable,
				[Symbol.terminalID, '#variable'],
				32
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalRandom],
				33
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalValueOp,
				[Symbol.terminalThrow],
				34
			)
		);
	}

	public get languageName(): string {
		return 'Chapter 1';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	public executeSemanticAction(
		semanticStack: Stack<any>,
		action: string
	): void {
		// console.log(`Chapter1Grammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		let name: Name;
		let variable: Variable<number>;
		let variableList: VariableList<number>;
		let expression: IExpression<number>;
		let expression2: IExpression<number>;
		let expression3: IExpression<number>;
		let expressionList: ExpressionList<number>;

		switch (action) {
			case '#functionDefinition':
				expression = semanticStack.pop() as IExpression<number>; // The function's body
				variableList = semanticStack.pop() as VariableList<number>; // The function's formal argument list
				name = semanticStack.pop() as Name; // The function name
				semanticStack.push(
					new FunctionDefinition<number>(
						name,
						variableList,
						expression
					)
				); // Add line and column?
				break;

			case '#variableList':
				variableList = semanticStack.pop() as VariableList<number>;
				variable = semanticStack.pop() as Variable<number>;
				variableList.value.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push(new VariableList<number>()); // Add line and column?
				break;

			case '#if':
				expression3 = semanticStack.pop() as IExpression<number>;
				expression2 = semanticStack.pop() as IExpression<number>;
				expression = semanticStack.pop() as IExpression<number>;
				semanticStack.push(
					new IfUsage<number>(expression, expression2, expression3)
				); // Add line and column?
				break;

			case '#while':
				expression2 = semanticStack.pop() as IExpression<number>;
				expression = semanticStack.pop() as IExpression<number>;
				semanticStack.push(
					new WhileUsage<number>(expression, expression2)
				); // Add line and column?
				break;

			case '#set':
				expression = semanticStack.pop() as IExpression<number>;
				variable = semanticStack.pop() as Variable<number>;
				semanticStack.push(
					new SetUsage<number>(variable, expression)
				); // Add line and column?
				break;

			case '#begin':
				expressionList = semanticStack.pop() as ExpressionList<
					number
				>;
				expression = semanticStack.pop() as IExpression<number>;
				semanticStack.push(
					new BeginUsage<number>(expression, expressionList)
				); // Add line and column?
				break;

			case '#operatorUsage':
				expressionList = semanticStack.pop() as ExpressionList<
					number
				>;
				name = semanticStack.pop() as Name;
				semanticStack.push(
					new Chapter1OperatorUsage(name, expressionList)
				); // Add line and column?
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as ExpressionList<
					number
				>;
				expression = semanticStack.pop() as IExpression<number>;
				expressionList.value.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push(new ExpressionList<number>());
				break;

			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(
					new Variable<number>(name.value, name.line, name.column)
				);
				break;

			default:
				throw new GrammarException(
					`Unrecognized semantic action: ${action}`
				);
		}
	}

	public tokenToSymbol(token: Token): number {
		// Returns Symbol
		// const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return Symbol.terminalEOF;
			case LexicalState.tokenIntLit:
				return Symbol.terminalIntegerLiteral;
			case LexicalState.tokenFltLit:
				return Symbol.terminalFloatLiteral;
			case LexicalState.tokenStrLit:
				return Symbol.terminalStringLiteral;
			case LexicalState.tokenLeftBracket:
				return Symbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return Symbol.terminalRightBracket;
			case LexicalState.tokenPlus:
				return Symbol.terminalPlus;
			case LexicalState.tokenMinus:
				return Symbol.terminalMinus;
			case LexicalState.tokenMult:
				return Symbol.terminalMultiply;
			case LexicalState.tokenDiv:
				return Symbol.terminalDivide;
			case LexicalState.tokenEqual:
				return Symbol.terminalEquals;
			case LexicalState.tokenLess:
				return Symbol.terminalLessThan;
			case LexicalState.tokenGreater:
				return Symbol.terminalGreaterThan;

			case LexicalState.tokenIdent:
				switch (token.tokenValue as string) {
					case 'define':
						return Symbol.terminalDefine;
					case 'if':
						return Symbol.terminalIf;
					case 'while':
						return Symbol.terminalWhile;
					case 'set':
						return Symbol.terminalSet;
					case 'begin':
						return Symbol.terminalBegin;
					case 'print':
						return Symbol.terminalPrint;
					// case 'random': return Symbol.terminalRandom;
					// case 'throw': return Symbol.terminalThrow;
					default:
						return Symbol.terminalID;
				}

				break;

			default:
				break;
		}

		throw new GrammarException(
			`No grammar symbol matches token ${token.tokenType} ${token.tokenValue}`,
			token.line,
			token.column
		);
	}

	public pushTokenOntoSemanticStack(
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case Symbol.terminalID:
			case Symbol.terminalPrint:
			case Symbol.terminalPlus:
			case Symbol.terminalMinus:
			case Symbol.terminalMultiply:
			case Symbol.terminalDivide:
			case Symbol.terminalEquals:
			case Symbol.terminalLessThan:
			case Symbol.terminalGreaterThan:
			case Symbol.terminalRandom:
			case Symbol.terminalThrow:
				semanticStack.push(
					new Name(value as string, token.line, token.column)
				);
				break;

			case Symbol.terminalIntegerLiteral:
				semanticStack.push(
					new IntegerLiteral(value, token.line, token.column)
				);
				break;

			case Symbol.terminalLeftBracket:
			case Symbol.terminalRightBracket:
			case Symbol.terminalDefine:
			case Symbol.terminalIf:
			case Symbol.terminalWhile:
			case Symbol.terminalSet:
			case Symbol.terminalBegin:
			case Symbol.terminalEOF:
				break;

			default:
				throw new GrammarException(
					`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${Symbol[tokenAsSymbol]} ${tokenAsSymbol}`,
					token.line,
					token.column
				);
		}
	}
}
