// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-augmented-syntax/lambda-calculus-augmented-syntax-grammar.ts

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

import { integerToChurchNumeral } from '../lambda-calculus/church-numerals';

import {
	createOperatorAdd,
	createOperatorMultiply,
	createOperatorIfUsage,
	createStatementLetUsage
} from '../lambda-calculus/operators';

import { ILCExpression } from '../lambda-calculus/domain-object-model/interfaces/expression';

import { LCFunctionCall } from '../lambda-calculus/domain-object-model/call';
import { LCLambdaExpression } from '../lambda-calculus/domain-object-model/lambda-expression';
import { LCVariable } from '../lambda-calculus/domain-object-model/variable';

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/Syntax.html :
//
// 	A complete BNF grammar for the lambda calculus:
//
// 	< λexp > ::= < var >
// 		| λ < var > . < λexp >
// 		| ( < λexp > < λexp > )

export class LambdaCalculusWithAugmentedSyntaxGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalFn); // === 'λ'
		this.terminals.push(GrammarSymbol.terminalDot);

		this.terminals.push(GrammarSymbol.terminalIf);

		this.terminals.push(GrammarSymbol.terminalLet);
		this.terminals.push(GrammarSymbol.terminalEquals);
		this.terminals.push(GrammarSymbol.terminalIn);

		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMultiply);

		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalLambdaExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunctionCall);

		this.addProduction(GrammarSymbol.nonterminalStart, [
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalEOF
		]);

		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalBracketedExpression,
			GrammarSymbol.terminalRightBracket
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
		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
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
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#functionCall'
		]);

		// Handle 'let':

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

		// Handle 'if':

		// 'if a then b else c' translates to '((a b) c)', where a is a fn that takes 2 args.
		// In fact, a is either TRUE or FALSE, where:
		// TRUE := λx.λy.x
		// FALSE := λx.λy.y
		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalIf,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#if'
		]);

		// Handle integer literals:

		// intexpr -> intlit
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalIntegerLiteral
		]);

		// binaryintop -> +
		this.addProduction(GrammarSymbol.nonterminalExpression, [GrammarSymbol.terminalPlus]);

		// binaryintop -> *
		this.addProduction(GrammarSymbol.nonterminalExpression, [GrammarSymbol.terminalMultiply]);
	}

	public get languageName(): string {
		return 'The Lambda Calculus with Augmented Syntax';
	}

	public get selectorsOfCompatibleParsers(): ParserSelector[] {
		return [ParserSelector.LL1];
	}

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let variable: LCVariable;
		let expression: ILCExpression;
		let expression2: ILCExpression;
		let expression3: ILCExpression;

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
				// semanticStack.push(
				// 	new LCFunctionCall(new LCLambdaExpression(variable, expression2), expression)
				// );
				semanticStack.push(createStatementLetUsage(variable, expression, expression2));
				break;

			case '#if':
				expression3 = semanticStack.pop() as ILCExpression;
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				// semanticStack.push(
				// 	new LCFunctionCall(new LCFunctionCall(expression, expression2), expression3)
				// );
				semanticStack.push(createOperatorIfUsage(expression, expression2, expression3));
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
			case LexicalState.tokenLowercaseGreekLetterLambda:
				return GrammarSymbol.terminalFn;
			case LexicalState.tokenDot:
				return GrammarSymbol.terminalDot;
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;
			case LexicalState.tokenPlus:
				return GrammarSymbol.terminalPlus;
			case LexicalState.tokenMult:
				return GrammarSymbol.terminalMultiply;
			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					case 'if':
						return GrammarSymbol.terminalIf;
					case 'in':
						return GrammarSymbol.terminalIn;
					case 'let':
						return GrammarSymbol.terminalLet;
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

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(integerToChurchNumeral(value));
				break;

			case GrammarSymbol.terminalPlus:
				semanticStack.push(createOperatorAdd());
				break;

			case GrammarSymbol.terminalMultiply:
				semanticStack.push(createOperatorMultiply());
				break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalFn:
			case GrammarSymbol.terminalDot:
			case GrammarSymbol.terminalLet:
			case GrammarSymbol.terminalIn:
			case GrammarSymbol.terminalIf:
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
