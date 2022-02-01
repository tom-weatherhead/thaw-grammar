// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/lambda-calculus-grammar.ts

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/Syntax.html :
//
// 	A complete BNF (Backus-Naur form) grammar for the lambda calculus:
//
//  < λexp > ::= < var >
//           | λ < var > . < λexp >
//           | ( < λexp > < λexp > )

// Glossary:
// A 'redex' is a reducible expression

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { Name } from 'thaw-interpreter-core';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { ILCExpression } from './domain-object-model/interfaces/expression';

import { LCFunctionCall } from './domain-object-model/call';
import { LCLambdaExpression } from './domain-object-model/lambda-expression';
import { LCVariable } from './domain-object-model/variable';

export class LambdaCalculusGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.addProduction(GrammarSymbol.nonterminalStart, [
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalEOF
		]);

		// Expression -> Variable
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.nonterminalVariable
		]);

		// Expression -> Lambda Expression
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.nonterminalLambdaExpression
		]);

		// Expression -> Function Call
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.nonterminalFunctionCall
		]);

		// Variable -> Name
		this.addProduction(GrammarSymbol.nonterminalVariable, [
			GrammarSymbol.terminalID,
			'#variable'
		]);

		// Lambda Expression -> λ Variable . Expression
		this.addProduction(GrammarSymbol.nonterminalLambdaExpression, [
			GrammarSymbol.terminalFn,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.terminalDot,
			GrammarSymbol.nonterminalExpression,
			'#lambdaExpression'
		]);

		// Function Call -> ( Expression Expression )
		this.addProduction(GrammarSymbol.nonterminalFunctionCall, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalRightBracket,
			'#functionCall'
		]);
	}

	public get languageName(): string {
		return 'The Lambda Calculus';
	}

	// public get selectorsOfCompatibleParsers(): ParserSelector[] {
	// 	return [ParserSelector.LL1];
	// }

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let variable: LCVariable;
		let expression: ILCExpression;
		let expression2: ILCExpression;

		switch (action) {
			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(new LCVariable(name.value)); //, name.line, name.column
				break;

			case '#lambdaExpression':
				expression = semanticStack.pop() as ILCExpression; // The function's body
				variable = semanticStack.pop() as LCVariable; // The function's formal argument list
				semanticStack.push(new LCLambdaExpression(variable, expression)); // Add line and column?
				break;

			case '#functionCall':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(new LCFunctionCall(expression, expression2));
				break;

			default:
				throw new GrammarException(`Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
			case LexicalState.tokenLowercaseGreekLetterLambda:
				return GrammarSymbol.terminalFn;
			case LexicalState.tokenDot:
				return GrammarSymbol.terminalDot;
			case LexicalState.tokenIdent:
				return GrammarSymbol.terminalID;

			default:
				throw new GrammarException(
					`No grammar symbol matches token ${token.tokenType} ${
						LexicalState[token.tokenType]
					} (value '${token.tokenValue}')`,
					token.line,
					token.column
				);
		}
	}

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalID:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalFn:
			case GrammarSymbol.terminalDot:
			case GrammarSymbol.terminalEOF:
				break;

			default:
				throw new GrammarException(
					`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${GrammarSymbol[tokenAsSymbol]} (${tokenAsSymbol})`,
					token.line,
					token.column
				);
		}
	}
}
