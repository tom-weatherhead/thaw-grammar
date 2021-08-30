// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/lambda-calculus-integer-extension-grammar.ts

// import {
// 	// GrammarSymbol,
// 	// IToken,
// 	// LexicalState,
// 	SemanticStackType
// } from 'thaw-interpreter-types';
//
// import { Name } from '../../common/domain-object-model/name';

// import { GrammarException } from 'thaw-interpreter-core';

// import { LambdaCalculusGrammar } from '../lambda-calculus/lambda-calculus-grammar';

// import { ILCExpression } from '../lambda-calculus/domain-object-model/interfaces/expression';
//
// import { LCFunctionCall } from '../lambda-calculus/domain-object-model/call';
// import { LCLambdaExpression } from '../lambda-calculus/domain-object-model/lambda-expression';
// import { LCVariable } from '../lambda-calculus/domain-object-model/variable';

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { Name } from '../../common/domain-object-model/name';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { ILCExpression } from '../lambda-calculus/domain-object-model/interfaces/expression';

import { LCFunctionCall } from '../lambda-calculus/domain-object-model/call';
import { LCLambdaExpression } from '../lambda-calculus/domain-object-model/lambda-expression';
import { LCVariable } from '../lambda-calculus/domain-object-model/variable';

// import { ILCIntegerExpression /*, IntegerLiteral */ } from './domain-object-model/integer-literal';
import { LCIntegerLiteral } from './domain-object-model/integer-literal';
import { LCPrimitiveOperator } from './domain-object-model/primitive-operator';

// Inherited productions:

// Deleted: λexp -> var
// λexp -> λ var . λexp
// λexp -> ( λexp λexp )

// New productions:

// λexp -> intexpr
// intexpr -> intliteral
// intexpr -> var
// intexpr -> [ binaryintop intexpr intexpr ]
// binaryintop -> +
// binaryintop -> -
// binaryintop -> *
// binaryintop -> / (i.e. integer division: Math.floor(a / b))
// binaryintop -> %

export class LambdaCalculusIntegerExtensionGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalFn); // === 'λ'
		this.terminals.push(GrammarSymbol.terminalDot);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		this.terminals.push(GrammarSymbol.terminalLeftSquareBracket);
		this.terminals.push(GrammarSymbol.terminalRightSquareBracket);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMinus);
		this.terminals.push(GrammarSymbol.terminalMultiply);
		this.terminals.push(GrammarSymbol.terminalEquals);
		this.terminals.push(GrammarSymbol.terminalIf);
		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		// this.nonTerminals.push(GrammarSymbol.nonterminalInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalLambdaExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunctionCall);
		this.nonTerminals.push(GrammarSymbol.nonterminalOptr);

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

		// intexpr -> intlit
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalIntegerLiteral
		]);

		// intexpr -> [ binaryintop intexpr intexpr ]
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalLeftSquareBracket,
			GrammarSymbol.nonterminalOptr,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalRightSquareBracket,
			'#integerOperator'
		]);

		// binaryintop -> +
		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalPlus]);

		// binaryintop -> -
		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalMinus]);

		// binaryintop -> *
		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalMultiply]);

		// binaryintop -> / (i.e. integer division: Math.floor(a / b))
		// this.addProduction(GrammarSymbol.nonterminalOptr, [
		// 	GrammarSymbol.terminalDivide
		// ]);

		// binaryintop -> %
		// this.addProduction(GrammarSymbol.nonterminalOptr, [
		// 	GrammarSymbol.terminalMod
		// ]);

		// binaryintop -> =
		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalEquals]);

		// Handling 'if':
		// 'if a then b else c' translates to '((a b) c)', where a is a fn that takes 2 args.
		// In fact, a is either TRUE or FALSE, where:
		// TRUE := λx.λy.x
		// FALSE := λx.λy.y
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalIf,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#if'
		]);
	}

	public get languageName(): string {
		return 'The Lambda Calculus + Ints';
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

			case '#integerOperator':
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				name = semanticStack.pop() as Name;
				semanticStack.push(new LCPrimitiveOperator(name.value, expression, expression2)); //, name.line, name.column
				break;

			case '#if':
				// throw new GrammarException(`Unimplemented semantic action: ${action}`);
				expression3 = semanticStack.pop() as ILCExpression;
				expression2 = semanticStack.pop() as ILCExpression;
				expression = semanticStack.pop() as ILCExpression;
				semanticStack.push(
					new LCFunctionCall(new LCFunctionCall(expression, expression2), expression3)
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
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
			case LexicalState.tokenLeftSquareBracket:
				return GrammarSymbol.terminalLeftSquareBracket;
			case LexicalState.tokenRightSquareBracket:
				return GrammarSymbol.terminalRightSquareBracket;
			case LexicalState.tokenGreekLetterLambda:
				return GrammarSymbol.terminalFn;
			case LexicalState.tokenDot:
				return GrammarSymbol.terminalDot;
			case LexicalState.tokenPlus:
				return GrammarSymbol.terminalPlus;
			case LexicalState.tokenMinus:
				return GrammarSymbol.terminalMinus;
			case LexicalState.tokenMult:
				return GrammarSymbol.terminalMultiply;
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;

			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					case 'if':
						return GrammarSymbol.terminalIf;
					// 	case '.':
					// 		return GrammarSymbol.terminalDot; // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
					// 	case 'λ':
					// 		console.log(
					// 			'LexicalState.tokenIdent λ being converted to Symbol.terminalFn'
					// 		);
					// 		return GrammarSymbol.terminalFn;
					default:
						return GrammarSymbol.terminalID;
				}
			// return GrammarSymbol.terminalID;

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
			case GrammarSymbol.terminalPlus:
			case GrammarSymbol.terminalMinus:
			case GrammarSymbol.terminalMultiply:
			case GrammarSymbol.terminalEquals:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(new LCIntegerLiteral(value));
				break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalLeftSquareBracket:
			case GrammarSymbol.terminalRightSquareBracket:
			case GrammarSymbol.terminalFn:
			case GrammarSymbol.terminalDot:
			case GrammarSymbol.terminalIf:
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

// export class LambdaCalculusIntegerExtensionGrammar extends LambdaCalculusGrammar {
// 	constructor() {
// 		super();
//
// 		/*
// 		// this.terminals.push(GrammarSymbol.terminalLeftBracket);
// 		// this.terminals.push(GrammarSymbol.terminalRightBracket);
// 		// this.terminals.push(GrammarSymbol.terminalID);
// 		// this.terminals.push(GrammarSymbol.terminalFn); // === 'λ'
// 		// this.terminals.push(GrammarSymbol.terminalDot);
// 		// this.terminals.push(GrammarSymbol.terminalEOF);
// 		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
// 		this.terminals.push(GrammarSymbol.terminalLeftSquareBracket);
// 		this.terminals.push(GrammarSymbol.terminalRightSquareBracket);
// 		this.terminals.push(GrammarSymbol.terminalPlus);
// 		this.terminals.push(GrammarSymbol.terminalMinus);
// 		this.terminals.push(GrammarSymbol.terminalMultiply);
// 		// this.terminals.push(GrammarSymbol.terminalDivide);
// 		// this.terminals.push(GrammarSymbol.terminalMod);
// 		this.terminals.push(GrammarSymbol.terminalEquals);
// 		// this.terminals.push(GrammarSymbol.terminalLessThan);
// 		// this.terminals.push(GrammarSymbol.terminalGreaterThan);
//
// 		// this.nonTerminals.push(GrammarSymbol.nonterminalStart);
// 		// // this.nonTerminals.push(GrammarSymbol.nonterminalInput);
// 		// this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
// 		// this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
// 		// this.nonTerminals.push(GrammarSymbol.nonterminalLambdaExpression);
// 		// this.nonTerminals.push(GrammarSymbol.nonterminalFunctionCall);
// 		// this.nonTerminals.push(GrammarSymbol.nonterminalTerm);
// 		this.nonTerminals.push(GrammarSymbol.nonterminalOptr);
//
// 		// λexp -> intexpr
// 		// this.addProduction(GrammarSymbol.nonterminalExpression, [
// 		// 	GrammarSymbol.nonterminalTerm // We use nonterminalTerm to represent intexpr
// 		// ]);
//
// 		// intexpr -> intliteral
// 		this.addProduction(GrammarSymbol.nonterminalExpression, [
// 			GrammarSymbol.terminalIntegerLiteral
// 		]);
//
// 		// intexpr -> var
// 		// this.addProduction(GrammarSymbol.nonterminalTerm, [GrammarSymbol.nonterminalVariable]);
//
// 		/*
// 		// intexpr -> [ binaryintop intexpr intexpr ]
// 		this.addProduction(GrammarSymbol.nonterminalExpression, [
// 			GrammarSymbol.terminalLeftSquareBracket,
// 			GrammarSymbol.nonterminalOptr,
// 			GrammarSymbol.nonterminalExpression,
// 			GrammarSymbol.nonterminalExpression,
// 			GrammarSymbol.terminalRightSquareBracket,
// 			'#integerOperator'
// 		]);
//
// 		// binaryintop -> +
// 		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalPlus]);
//
// 		// binaryintop -> -
// 		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalMinus]);
//
// 		// binaryintop -> *
// 		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalMultiply]);
//
// 		// binaryintop -> / (i.e. integer division: Math.floor(a / b))
// 		// this.addProduction(GrammarSymbol.nonterminalOptr, [
// 		// 	GrammarSymbol.terminalDivide
// 		// ]);
//
// 		// binaryintop -> %
// 		// this.addProduction(GrammarSymbol.nonterminalOptr, [
// 		// 	GrammarSymbol.terminalMod
// 		// ]);
//
// 		// binaryintop -> =
// 		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.terminalEquals]);
// 		 */
// 	}
//
// 	public override get languageName(): string {
// 		return 'The Lambda Calculus with Integer Extensions';
// 	}
//
// 	// public get selectorsOfCompatibleParsers(): ParserSelector[] {
// 	// 	return [ParserSelector.LL1];
// 	// }
//
// 	public override executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
// 		let name: Name;
// 		// let variable: LCVariable;
// 		let integerExpression1: ILCIntegerExpression;
// 		let integerExpression2: ILCIntegerExpression;
//
// 		switch (action) {
// 			case '#integerOperator':
// 				integerExpression2 = semanticStack.pop() as ILCIntegerExpression;
// 				integerExpression1 = semanticStack.pop() as ILCIntegerExpression;
// 				name = semanticStack.pop() as Name;
// 				semanticStack.push(
// 					new PrimitiveOperator(name.value, integerExpression1, integerExpression2)
// 				); //, name.line, name.column
// 				break;
//
// 			// case '#variable':
// 			// 	name = semanticStack.pop() as Name;
// 			// 	semanticStack.push(new LCVariable(name.value)); //, name.line, name.column
// 			// 	break;
// 			//
// 			// case '#lambdaExpression':
// 			// 	expression = semanticStack.pop() as ILCExpression; // The function's body
// 			// 	variable = semanticStack.pop() as LCVariable; // The function's formal argument list
// 			// 	semanticStack.push(new LCLambdaExpression(variable, expression)); // Add line and column?
// 			// 	break;
// 			//
// 			// case '#functionCall':
// 			// 	expression2 = semanticStack.pop() as ILCExpression;
// 			// 	expression = semanticStack.pop() as ILCExpression;
// 			// 	semanticStack.push(new LCFunctionCall(expression, expression2));
// 			// 	break;
//
// 			default:
// 				// throw new GrammarException(`Unrecognized semantic action: ${action}`);
// 				super.executeSemanticAction(semanticStack, action);
// 		}
// 	}
//
// 	// public override tokenToSymbol(token: IToken): GrammarSymbol {
// 	// 	// const tokenValueAsString: string = token.tokenValue as string;
// 	//
// 	// 	switch (token.tokenType) {
// 	// 		case LexicalState.tokenIntLit:
// 	// 			return GrammarSymbol.terminalIntegerLiteral;
// 	// 		case LexicalState.tokenLeftSquareBracket:
// 	// 			return GrammarSymbol.terminalLeftSquareBracket;
// 	// 		case LexicalState.tokenRightSquareBracket:
// 	// 			return GrammarSymbol.terminalRightSquareBracket;
// 	// 		case LexicalState.tokenPlus:
// 	// 			return GrammarSymbol.terminalPlus;
// 	// 		case LexicalState.tokenMinus:
// 	// 			return GrammarSymbol.terminalMinus;
// 	// 		case LexicalState.tokenMult:
// 	// 			return GrammarSymbol.terminalMultiply;
// 	// 		// case LexicalState.tokenDiv:
// 	// 		// 	return GrammarSymbol.terminalDivide;
// 	// 		case LexicalState.tokenEqual:
// 	// 			return GrammarSymbol.terminalEquals;
// 	// 		// case LexicalState.tokenLess:
// 	// 		// 	return GrammarSymbol.terminalLessThan;
// 	// 		// case LexicalState.tokenGreater:
// 	// 		// 	return GrammarSymbol.terminalGreaterThan;
// 	//
// 	// 		default:
// 	// 			return super.tokenToSymbol(token);
// 	// 	}
// 	// }
// 	//
// 	// public override pushTokenOntoSemanticStack(
// 	// 	semanticStack: SemanticStackType,
// 	// 	tokenAsSymbol: GrammarSymbol,
// 	// 	token: IToken
// 	// ): void {
// 	// 	const value = token.tokenValue;
// 	//
// 	// 	switch (tokenAsSymbol) {
// 	// 		// case GrammarSymbol.terminalPrint:
// 	// 		case GrammarSymbol.terminalPlus:
// 	// 		case GrammarSymbol.terminalMinus:
// 	// 		case GrammarSymbol.terminalMultiply:
// 	// 		case GrammarSymbol.terminalDivide:
// 	// 		// case GrammarSymbol.terminalMod:
// 	// 		case GrammarSymbol.terminalEquals:
// 	// 			// case GrammarSymbol.terminalLessThan:
// 	// 			// case GrammarSymbol.terminalGreaterThan:
// 	// 			semanticStack.push(new Name(value as string, token.line, token.column));
// 	// 			break;
// 	//
// 	// 		case GrammarSymbol.terminalLeftSquareBracket:
// 	// 		case GrammarSymbol.terminalRightSquareBracket:
// 	// 			break;
// 	//
// 	// 		case GrammarSymbol.terminalIntegerLiteral:
// 	// 			semanticStack.push(new IntegerLiteral(value));
// 	// 			break;
// 	//
// 	// 		default:
// 	// 			// throw new GrammarException(
// 	// 			// 	`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${GrammarSymbol[tokenAsSymbol]} (${tokenAsSymbol})`,
// 	// 			// 	token.line,
// 	// 			// 	token.column
// 	// 			// );
// 	// 			super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
// 	// 	}
// 	// }
// }
