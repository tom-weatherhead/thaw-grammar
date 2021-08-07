// tom-weatherhead/thaw-grammar/src/languages/minimal/minimal-language-grammar.ts

// A minimal grammar that supports the input: (+ 2 3)
// I.e. Tokens: LeftBracket, Plus, IntegerLiteral_2, IntegerLiteral_3, RightBracket, EOF.

import { Stack } from 'thaw-common-utilities.ts';

import { LexicalState, Token } from 'thaw-lexical-analyzer';

import { ExpressionList } from '../../common/domain-object-model/expression-list';
import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';

import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Production } from '../../common/production';

import { Symbol } from '../../common/symbol';

import { IntegerLiteral } from './domain-object-model/integer-literal';
import { OperatorUsage } from './domain-object-model/operator-usage';

export class MinimalLanguageGrammar extends GrammarBase {
	constructor() {
		super(Symbol.nonterminalStart);

		this.terminals.push(Symbol.terminalLeftBracket);
		this.terminals.push(Symbol.terminalRightBracket);
		this.terminals.push(Symbol.terminalPlus);
		this.terminals.push(Symbol.terminalIntegerLiteral);
		this.terminals.push(Symbol.terminalEOF);

		this.nonTerminals.push(Symbol.nonterminalStart);
		this.nonTerminals.push(Symbol.nonterminalExpression);
		this.nonTerminals.push(Symbol.nonterminalBracketedExpression);
		this.nonTerminals.push(Symbol.nonterminalExpressionList);

		this.productions.push(
			new Production(
				Symbol.nonterminalStart,
				[Symbol.nonterminalExpression, Symbol.terminalEOF],
				1
			)
		);
		this.productions.push(
			new Production(Symbol.nonterminalExpression, [Symbol.terminalIntegerLiteral], 2)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpression,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalBracketedExpression,
					Symbol.terminalRightBracket
				],
				3
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[Symbol.terminalPlus, Symbol.nonterminalExpressionList, '#operatorUsage'],
				4
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpressionList,
				[Symbol.nonterminalExpression, Symbol.nonterminalExpressionList, '#expressionList'],
				5
			)
		);
		this.productions.push(
			new Production(
				Symbol.nonterminalExpressionList,
				[Symbol.Lambda, '#emptyExpressionList'],
				6
			)
		);
	}

	public get languageName(): string {
		// This is a 'get' accessor.
		return 'The minimal language';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	public executeSemanticAction(semanticStack: Stack<any>, action: string): void {
		// console.log(`MinimalLanguageGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		let name: Name;
		let expression: IExpression<number>;
		let expressionList: ExpressionList<number>;

		switch (action) {
			case '#operatorUsage':
				expressionList = semanticStack.pop() as ExpressionList<number>;
				name = semanticStack.pop() as Name;
				semanticStack.push(new OperatorUsage(name, expressionList));
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as ExpressionList<number>;
				expression = semanticStack.pop() as IExpression<number>;
				expressionList.value.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push(new ExpressionList<number>());
				break;

			default:
				throw new GrammarException(`Unrecognized semantic action: ${action}`);
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
			case LexicalState.tokenLeftBracket:
				return Symbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return Symbol.terminalRightBracket;
			case LexicalState.tokenPlus:
				return Symbol.terminalPlus;

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
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {
		switch (tokenAsSymbol) {
			case Symbol.terminalIntegerLiteral:
				// console.log(`Pushing IntegerLiteral ${token.tokenValue as number} onto the semanticStack`);
				semanticStack.push(new IntegerLiteral(token.tokenValue));
				break;

			case Symbol.terminalPlus:
				// console.log(`Pushing Name '${token.tokenValue as string}' onto the semanticStack`);
				semanticStack.push(
					new Name(token.tokenValue as string, token.line, token.column /*, false */)
				);
				break;

			case Symbol.terminalLeftBracket:
			case Symbol.terminalRightBracket:
			case Symbol.terminalEOF:
				break;

			default:
				throw new GrammarException(
					`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${tokenAsSymbol}`,
					token.line,
					token.column
				);
		}
	}
}
