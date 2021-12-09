// tom-weatherhead/thaw-grammar/src/languages/arithmetic/arithmetic-grammar.ts

// Use an LR parser.

// expr1 = expr1 + expr2 // To make + associate from left to right.
// expr1 = expr1 - expr2
// expr1 = expr2

// expr2 = expr2 * expr3
// expr2 = expr2 / expr3
// expr2 = expr3

// expr3 = expr4 ^ expr3 // To ensure that a ^ b ^ c = a ^ (b ^ c)
// expr3 = expr4

// expr4 = NumericLiteral
// expr4 = ( expr1 )

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { createProduction, Name } from 'thaw-interpreter-core';

import { IExpression } from '../../common/domain-object-model/iexpression';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { IntegerLiteral } from './domain-object-model/number';
import { OperatorUsage } from './domain-object-model/operator-usage';

export class ArithmeticGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMinus);
		this.terminals.push(GrammarSymbol.terminalMultiply);
		this.terminals.push(GrammarSymbol.terminalDivide);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalArithmeticExpression1);
		this.nonTerminals.push(GrammarSymbol.nonterminalArithmeticExpression2);
		// this.nonTerminals.push(GrammarSymbol.nonterminalArithmeticOperator1);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalStart,
				[GrammarSymbol.nonterminalArithmeticExpression1, GrammarSymbol.terminalEOF],
				1
			)
		);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArithmeticExpression1,
				[
					GrammarSymbol.nonterminalArithmeticExpression1,
					GrammarSymbol.terminalPlus,
					GrammarSymbol.nonterminalArithmeticExpression2,
					'#operatorUsage'
				],
				2
			)
		);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArithmeticExpression1,
				[
					GrammarSymbol.nonterminalArithmeticExpression1,
					GrammarSymbol.terminalMinus,
					GrammarSymbol.nonterminalArithmeticExpression2,
					'#operatorUsage'
				],
				3
			)
		);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArithmeticExpression1,
				[GrammarSymbol.nonterminalArithmeticExpression2],
				4
			)
		);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArithmeticExpression2,
				[GrammarSymbol.terminalIntegerLiteral],
				5
			)
		);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArithmeticExpression2,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalArithmeticExpression1,
					GrammarSymbol.terminalRightBracket
				],
				6
			)
		);
	}

	public get languageName(): string {
		return 'An arithmetic language';
	}

	public override get defaultParser(): ParserSelector {
		return ParserSelector.SLR1;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		// throw new Error('executeSemanticAction() : Not implemented');

		// console.log(`Grammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		let operatorName: Name;
		let expressionL: IExpression<number>;
		let expressionR: IExpression<number>;
		// let expressionList: IExpression<number>[];

		switch (action) {
			case '#operatorUsage':
				expressionR = semanticStack.pop() as IExpression<number>;
				operatorName = semanticStack.pop() as Name;
				expressionL = semanticStack.pop() as IExpression<number>;
				semanticStack.push(new OperatorUsage(operatorName, [expressionL, expressionR]));
				break;

			// case '#expressionList':
			// 	expressionList = semanticStack.pop() as IExpression<number>[];
			// 	expression = semanticStack.pop() as IExpression<number>;
			// 	expressionList.unshift(expression);
			// 	semanticStack.push(expressionList);
			// 	break;
			//
			// case '#emptyExpressionList':
			// 	semanticStack.push([] as IExpression<number>[]);
			// 	break;

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
			case LexicalState.tokenMinus:
				return GrammarSymbol.terminalMinus;
			case LexicalState.tokenMult:
				return GrammarSymbol.terminalMultiply;
			case LexicalState.tokenDiv:
				return GrammarSymbol.terminalDivide;

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
		// throw new Error('pushTokenOntoSemanticStack() : Not implemented');

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalIntegerLiteral:
				// console.log(`Pushing IntegerLiteral ${token.tokenValue as number} onto the semanticStack`);
				semanticStack.push(new IntegerLiteral(token.tokenValue));
				break;

			case GrammarSymbol.terminalPlus:
			case GrammarSymbol.terminalMinus:
			case GrammarSymbol.terminalMultiply:
			case GrammarSymbol.terminalDivide:
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
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
