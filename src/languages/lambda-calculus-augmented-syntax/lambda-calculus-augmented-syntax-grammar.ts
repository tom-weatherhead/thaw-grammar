// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-augmented-syntax/lambda-calculus-augmented-syntax-grammar.ts

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/Syntax.html :
//
// 	A complete BNF grammar for the lambda calculus:
//
// 	< λexp > ::= < var >
// 		| λ < var > . < λexp >
// 		| ( < λexp > < λexp > )

// Glossary:
// A 'redex' is a reducible expression

// Tasks completed:

// - Convert non-negative integers into Church numerals via integerToChurchNumeral()
// - Add language support for the constants 'true' (λx.λy.x) and 'false' (λx.λy.y).
// - Add operators that can be easily converted to Lambda calculus expressions:
//   - let : 'let v = e1 in e2' -> (λv.e2 e1)
//   - if : λb.λx.λy.((b x) y)
//   - && (and) : λp.λq.((p q) FALSE)
//   - || (or) : λp.λq.(((IF p) TRUE) q)
//   - + : λm.λn.λf.λx.((n f) ((m f) x))
//   - * : λm.λn.λf.(m (n f))
//   - ++ (successor) : λn.λf.λx.(f ((n f) x))
//   - -- (predecessor) : λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)
//   - (z? or 0?) (isZero) : λn.((n λx.FALSE) TRUE)
//   - comb ; e.g. (comb Y) for the Y combinator

// Tasks TODO:
// - Add language support for pairs (?)
// - Add language support for lists (?)
//   - nil
//   - null?
//   - cons
//   - hd (car)
//   - tl (cdr)

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
	createCombinator,
	createOperatorAddUsage,
	createOperatorDecrementUsage,
	createOperatorIfUsage,
	createOperatorIncrementUsage,
	createOperatorIsZeroUsage,
	createOperatorMultiplyUsage,
	// createPredicateIsZeroUsage,
	createStatementLetUsage,
	createValueFalse,
	createValueTrue,
	lcaConsUsage,
	lcaCreateNil,
	lcaHeadUsage,
	lcaIsListUsage,
	lcaIsNullUsage,
	lcaTailUsage
} from '../lambda-calculus/operators';

import { ILCExpression } from '../lambda-calculus/domain-object-model/interfaces/expression';

import { LCFunctionCall } from '../lambda-calculus/domain-object-model/call';
import { LCLambdaExpression } from '../lambda-calculus/domain-object-model/lambda-expression';
import { LCVariable } from '../lambda-calculus/domain-object-model/variable';

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

		this.terminals.push(GrammarSymbol.terminalTrue);
		this.terminals.push(GrammarSymbol.terminalFalse);

		this.terminals.push(GrammarSymbol.terminalComb);

		this.terminals.push(GrammarSymbol.terminalInc);
		this.terminals.push(GrammarSymbol.terminalDec);
		this.terminals.push(GrammarSymbol.terminalIsZero);
		this.terminals.push(GrammarSymbol.terminalAnd);
		this.terminals.push(GrammarSymbol.terminalOr);

		this.terminals.push(GrammarSymbol.terminalNil);
		this.terminals.push(GrammarSymbol.terminalNullPred);
		this.terminals.push(GrammarSymbol.terminalCons);
		this.terminals.push(GrammarSymbol.terminalCar);
		this.terminals.push(GrammarSymbol.terminalCdr);
		this.terminals.push(GrammarSymbol.terminalListPred);

		this.terminals.push(GrammarSymbol.terminalThickArrow);

		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalLambdaExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunctionCall);
		this.nonTerminals.push(GrammarSymbol.nonterminalAfterVariable);

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
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalAfterVariable
		]);

		this.addProduction(GrammarSymbol.nonterminalAfterVariable, [GrammarSymbol.Lambda]);

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

		// Handle the constants 'true' and 'false':

		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalTrue,
			'#true'
		]);
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalFalse,
			'#false'
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
		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalPlus,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#plus'
		]);

		// binaryintop -> *
		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalMultiply,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#multiply'
		]);

		// Handle combinators:

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalComb,
			GrammarSymbol.terminalID,
			'#comb'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalInc,
			GrammarSymbol.nonterminalExpression,
			'#inc'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalDec,
			GrammarSymbol.nonterminalExpression,
			'#dec'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalIsZero,
			GrammarSymbol.nonterminalExpression,
			'#isZero'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalAnd,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#and'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalOr,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#or'
		]);

		// Handle lists:

		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalNil,
			'#nil'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalNullPred,
			GrammarSymbol.nonterminalExpression,
			'#null?'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalCons,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#cons'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalCar,
			GrammarSymbol.nonterminalExpression,
			'#car'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalCdr,
			GrammarSymbol.nonterminalExpression,
			'#cdr'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalListPred,
			GrammarSymbol.nonterminalExpression,
			'#list?'
		]);

		// Arrow syntax for lambda expressions

		this.addProduction(GrammarSymbol.nonterminalAfterVariable, [
			GrammarSymbol.terminalThickArrow,
			GrammarSymbol.nonterminalExpression,
			// '#arrow'
			'#lambdaExpression'
		]);
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
				semanticStack.push(createStatementLetUsage(variable, expression, expression2));
				break;

			case '#if':
				expression3 = semanticStack.pop() as ILCExpression;
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(createOperatorIfUsage(expression, expression2, expression3));
				break;

			case '#plus':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(createOperatorAddUsage(expression, expression2));
				break;

			case '#multiply':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(createOperatorMultiplyUsage(expression, expression2));
				break;

			case '#true':
				semanticStack.push(createValueTrue());
				break;

			case '#false':
				semanticStack.push(createValueFalse());
				break;

			case '#comb':
				name = semanticStack.pop() as Name;
				semanticStack.push(createCombinator(name.value));
				break;

			case '#inc':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(createOperatorIncrementUsage(expression));
				break;

			case '#dec':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(createOperatorDecrementUsage(expression));
				break;

			case '#isZero':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(createOperatorIsZeroUsage(expression));
				break;

			case '#and':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(
					createOperatorIfUsage(expression, expression2, createValueFalse())
				);
				break;

			case '#or':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(
					createOperatorIfUsage(expression, createValueTrue(), expression2)
				);
				break;

			case '#nil':
				semanticStack.push(lcaCreateNil());
				break;

			case '#null?':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(lcaIsNullUsage(expression));
				break;

			case '#cons':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(lcaConsUsage(expression, expression2));
				break;

			case '#car':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(lcaHeadUsage(expression));
				break;

			case '#cdr':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(lcaTailUsage(expression));
				break;

			case '#list?':
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(lcaIsListUsage(expression));
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
			case LexicalState.tokenThickArrow:
				return GrammarSymbol.terminalThickArrow;
			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					case 'inc':
						return GrammarSymbol.terminalInc;
					case 'dec':
						return GrammarSymbol.terminalDec;
					case 'zero?':
						return GrammarSymbol.terminalIsZero;
					case 'and':
						return GrammarSymbol.terminalAnd;
					case 'or':
						return GrammarSymbol.terminalOr;
					case 'comb':
						return GrammarSymbol.terminalComb;
					case 'false':
						return GrammarSymbol.terminalFalse;
					case 'if':
						return GrammarSymbol.terminalIf;
					case 'in':
						return GrammarSymbol.terminalIn;
					case 'let':
						return GrammarSymbol.terminalLet;
					case 'true':
						return GrammarSymbol.terminalTrue;
					case 'nil':
						return GrammarSymbol.terminalNil;
					case 'null?':
						return GrammarSymbol.terminalNullPred;
					case 'cons':
						return GrammarSymbol.terminalCons;
					case 'hd':
					case 'car':
						return GrammarSymbol.terminalCar;
					case 'tl':
					case 'cdr':
						return GrammarSymbol.terminalCdr;
					case 'list?':
						return GrammarSymbol.terminalListPred;
					// case '=>':
					// 	return GrammarSymbol.terminalThickArrow;
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

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalFn:
			case GrammarSymbol.terminalDot:
			case GrammarSymbol.terminalTrue:
			case GrammarSymbol.terminalFalse:
			case GrammarSymbol.terminalIf:
			case GrammarSymbol.terminalLet:
			case GrammarSymbol.terminalIn:
			case GrammarSymbol.terminalEquals:
			case GrammarSymbol.terminalPlus:
			case GrammarSymbol.terminalMultiply:
			case GrammarSymbol.terminalComb:
			case GrammarSymbol.terminalInc:
			case GrammarSymbol.terminalDec:
			case GrammarSymbol.terminalIsZero:
			case GrammarSymbol.terminalAnd:
			case GrammarSymbol.terminalOr:
			case GrammarSymbol.terminalNil:
			case GrammarSymbol.terminalNullPred:
			case GrammarSymbol.terminalCons:
			case GrammarSymbol.terminalCar:
			case GrammarSymbol.terminalCdr:
			case GrammarSymbol.terminalListPred:
			case GrammarSymbol.terminalThickArrow:
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
