// tom-weatherhead/thaw-grammar/src/languages/minimal/minimal-language-grammar.ts

// A minimal grammar that supports the input: (+ 2 3)
// I.e. Tokens: LeftBracket, Plus, IntegerLiteral_2, IntegerLiteral_3, RightBracket, EOF.

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { createProduction, Name } from 'thaw-interpreter-core';

import { IExpression } from '../../common/domain-object-model/iexpression';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { IntegerLiteral } from './domain-object-model/integer-literal';
import { OperatorUsage } from './domain-object-model/operator-usage';

export class MinimalLanguageGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpressionList);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalStart,
				[GrammarSymbol.nonterminalExpression, GrammarSymbol.terminalEOF],
				1
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.terminalIntegerLiteral],
				2
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
				3
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalPlus,
					GrammarSymbol.nonterminalExpressionList,
					'#operatorUsage'
				],
				4
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
				5
			)
		);
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[GrammarSymbol.Lambda, '#emptyExpressionList'],
				6
			)
		);
	}

	public get languageName(): string {
		// This is a 'get' accessor.
		return 'The minimal language';
	}

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		// console.log(`MinimalLanguageGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		let name: Name;
		let expression: IExpression<number>;
		let expressionList: IExpression<number>[];

		switch (action) {
			case '#operatorUsage':
				expressionList = semanticStack.pop() as IExpression<number>[];
				name = semanticStack.pop() as Name;
				semanticStack.push(new OperatorUsage(name, expressionList));
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

			default:
				throw new GrammarException(`Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		// Returns Symbol
		// const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
			case LexicalState.tokenPlus:
				return GrammarSymbol.terminalPlus;

			default:
				break;
		}

		throw new GrammarException(
			`No grammar symbol matches token ${token.tokenType} ${token.tokenValue}`,
			token.line,
			token.column
		);
	}

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {
		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalIntegerLiteral:
				// console.log(`Pushing IntegerLiteral ${token.tokenValue as number} onto the semanticStack`);
				semanticStack.push(new IntegerLiteral(token.tokenValue));
				break;

			case GrammarSymbol.terminalPlus:
				// console.log(`Pushing Name '${token.tokenValue as string}' onto the semanticStack`);
				semanticStack.push(
					new Name(token.tokenValue as string, token.line, token.column /*, false */)
				);
				break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalEOF:
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
