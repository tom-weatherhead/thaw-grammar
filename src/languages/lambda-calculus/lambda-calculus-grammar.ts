// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/lambda-calculus-grammar.ts

// Glossary:
// A 'redex' is a reducible expression

// TODO:

// - Convert non-negative integers into Church numerals via integerToChurchNumeral()
// - Add operators that can be easily converted to Lambda calculus expressions:
//   - ++ (successor) : λn.λf.λx.(f ((n f) x))
//   - -- (predecessor) : λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)
//   - + : λm.λn.λf.λx.((n f) ((m f) x))
//   - * : λm.λn.λf.(m (n f))
//   - (z? or 0?) (isZero) : λn.((n λx.FALSE) TRUE)
//   - if : λb.λx.λy.((b x) y)
//   - && (and) : λp.λq.((p q) FALSE)
//   - || (or) : λp.λq.(((IF p) TRUE) q)
// - Add support for the constants 'true' (λx.λy.x) and 'false' (λx.λy.y).

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { Name } from '../../common/domain-object-model/name';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { ILCExpression } from './domain-object-model/interfaces/expression';

import { LCFunctionCall } from './domain-object-model/call';
import { LCLambdaExpression } from './domain-object-model/lambda-expression';
import { LCVariable } from './domain-object-model/variable';

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/Syntax.html :
//
// 	A complete BNF grammar for the lambda calculus:
//
// 	< λexp > ::= < var >
// 		| λ < var > . < λexp >
// 		| ( < λexp > < λexp > )

export class LambdaCalculusGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalFn); // === 'λ'
		this.terminals.push(GrammarSymbol.terminalDot);

		this.terminals.push(GrammarSymbol.terminalLet);
		this.terminals.push(GrammarSymbol.terminalEquals);
		this.terminals.push(GrammarSymbol.terminalIn);

		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalLambdaExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunctionCall);

		// This initial production needed to be added: Start -> Input EOF
		this.addProduction(GrammarSymbol.nonterminalStart, [
			// GrammarSymbol.nonterminalInput,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalEOF
		]);

		// Input -> Expression
		// this.addProduction(GrammarSymbol.nonterminalInput, [GrammarSymbol.nonterminalExpression]);

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

		// Expression -> let v = e in e2
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalLet,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.terminalEquals,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalIn,
			GrammarSymbol.nonterminalExpression,
			'#let'
		]);
	}

	public get languageName(): string {
		return 'The Lambda Calculus';
	}

	public get selectorsOfCompatibleParsers(): ParserSelector[] {
		return [ParserSelector.LL1];
	}

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

			case '#let': // I.e. let variable = expression in expression2
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression; // The function's body
				variable = semanticStack.pop() as LCVariable; // The function's formal argument
				semanticStack.push(
					new LCFunctionCall(new LCLambdaExpression(variable, expression2), expression)
				);
				break;

			default:
				throw new GrammarException(`Unrecognized semantic action: ${action}`);
		}
	}

	public tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
			case LexicalState.tokenGreekLetterLambda:
				return GrammarSymbol.terminalFn;
			case LexicalState.tokenDot:
				return GrammarSymbol.terminalDot;
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;
			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					case 'let':
						return GrammarSymbol.terminalLet;
					case 'in':
						return GrammarSymbol.terminalIn;
					default:
						return GrammarSymbol.terminalID;
				}

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

	public pushTokenOntoSemanticStack(
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
			case GrammarSymbol.terminalLet:
			case GrammarSymbol.terminalIn:
			case GrammarSymbol.terminalEquals:
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
