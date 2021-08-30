// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/lambda-calculus-integer-extension-grammar.ts

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	// ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { Name } from '../../common/domain-object-model/name';

// import { GrammarException } from 'thaw-interpreter-core';

import { LambdaCalculusGrammar } from '../lambda-calculus/lambda-calculus-grammar';

// import { ILCExpression } from '../lambda-calculus/domain-object-model/interfaces/expression';
//
// import { LCFunctionCall } from '../lambda-calculus/domain-object-model/call';
// import { LCLambdaExpression } from '../lambda-calculus/domain-object-model/lambda-expression';
// import { LCVariable } from '../lambda-calculus/domain-object-model/variable';

import { ILCIntegerExpression, IntegerLiteral } from './domain-object-model/integer-literal';
import { PrimitiveOperator } from './domain-object-model/primitive-operator';

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

export class LambdaCalculusIntegerExtensionGrammar extends LambdaCalculusGrammar {
	constructor() {
		super(true);

		// this.terminals.push(GrammarSymbol.terminalLeftBracket);
		// this.terminals.push(GrammarSymbol.terminalRightBracket);
		// this.terminals.push(GrammarSymbol.terminalID);
		// this.terminals.push(GrammarSymbol.terminalFn); // === 'λ'
		// this.terminals.push(GrammarSymbol.terminalDot);
		// this.terminals.push(GrammarSymbol.terminalEOF);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		this.terminals.push(GrammarSymbol.terminalLeftSquareBracket);
		this.terminals.push(GrammarSymbol.terminalRightSquareBracket);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMinus);
		this.terminals.push(GrammarSymbol.terminalMultiply);
		// this.terminals.push(GrammarSymbol.terminalDivide);
		// this.terminals.push(GrammarSymbol.terminalMod);
		this.terminals.push(GrammarSymbol.terminalEquals);
		// this.terminals.push(GrammarSymbol.terminalLessThan);
		// this.terminals.push(GrammarSymbol.terminalGreaterThan);

		// this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		// // this.nonTerminals.push(GrammarSymbol.nonterminalInput);
		// this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		// this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		// this.nonTerminals.push(GrammarSymbol.nonterminalLambdaExpression);
		// this.nonTerminals.push(GrammarSymbol.nonterminalFunctionCall);
		this.nonTerminals.push(GrammarSymbol.nonterminalTerm);
		this.nonTerminals.push(GrammarSymbol.nonterminalOptr);

		// λexp -> intexpr
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.nonterminalTerm // We use nonterminalTerm to represent intexpr
		]);

		// intexpr -> intliteral
		this.addProduction(GrammarSymbol.nonterminalTerm, [GrammarSymbol.terminalIntegerLiteral]);

		// intexpr -> var
		this.addProduction(GrammarSymbol.nonterminalTerm, [GrammarSymbol.nonterminalVariable]);

		// intexpr -> [ binaryintop intexpr intexpr ]
		this.addProduction(GrammarSymbol.nonterminalTerm, [
			GrammarSymbol.terminalLeftSquareBracket,
			GrammarSymbol.nonterminalOptr,
			GrammarSymbol.nonterminalTerm,
			GrammarSymbol.nonterminalTerm,
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
	}

	public override get languageName(): string {
		return 'The Lambda Calculus with Integer Extensions';
	}

	// public get selectorsOfCompatibleParsers(): ParserSelector[] {
	// 	return [ParserSelector.LL1];
	// }

	public override executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		// let variable: LCVariable;
		let integerExpression1: ILCIntegerExpression;
		let integerExpression2: ILCIntegerExpression;

		switch (action) {
			case '#integerOperator':
				integerExpression2 = semanticStack.pop() as ILCIntegerExpression;
				integerExpression1 = semanticStack.pop() as ILCIntegerExpression;
				name = semanticStack.pop() as Name;
				semanticStack.push(
					new PrimitiveOperator(name.value, integerExpression1, integerExpression2)
				); //, name.line, name.column
				break;

			// case '#variable':
			// 	name = semanticStack.pop() as Name;
			// 	semanticStack.push(new LCVariable(name.value)); //, name.line, name.column
			// 	break;
			//
			// case '#lambdaExpression':
			// 	expression = semanticStack.pop() as ILCExpression; // The function's body
			// 	variable = semanticStack.pop() as LCVariable; // The function's formal argument list
			// 	semanticStack.push(new LCLambdaExpression(variable, expression)); // Add line and column?
			// 	break;
			//
			// case '#functionCall':
			// 	expression2 = semanticStack.pop() as ILCExpression;
			// 	expression = semanticStack.pop() as ILCExpression;
			// 	semanticStack.push(new LCFunctionCall(expression, expression2));
			// 	break;

			default:
				// throw new GrammarException(`Unrecognized semantic action: ${action}`);
				super.executeSemanticAction(semanticStack, action);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		// const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;
			case LexicalState.tokenLeftSquareBracket:
				return GrammarSymbol.terminalLeftSquareBracket;
			case LexicalState.tokenRightSquareBracket:
				return GrammarSymbol.terminalRightSquareBracket;
			case LexicalState.tokenPlus:
				return GrammarSymbol.terminalPlus;
			case LexicalState.tokenMinus:
				return GrammarSymbol.terminalMinus;
			case LexicalState.tokenMult:
				return GrammarSymbol.terminalMultiply;
			// case LexicalState.tokenDiv:
			// 	return GrammarSymbol.terminalDivide;
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;
			// case LexicalState.tokenLess:
			// 	return GrammarSymbol.terminalLessThan;
			// case LexicalState.tokenGreater:
			// 	return GrammarSymbol.terminalGreaterThan;

			default:
				return super.tokenToSymbol(token);
		}
	}

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			// case GrammarSymbol.terminalPrint:
			case GrammarSymbol.terminalPlus:
			case GrammarSymbol.terminalMinus:
			case GrammarSymbol.terminalMultiply:
			case GrammarSymbol.terminalDivide:
			// case GrammarSymbol.terminalMod:
			case GrammarSymbol.terminalEquals:
				// case GrammarSymbol.terminalLessThan:
				// case GrammarSymbol.terminalGreaterThan:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case GrammarSymbol.terminalLeftSquareBracket:
			case GrammarSymbol.terminalRightSquareBracket:
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(new IntegerLiteral(value));
				break;

			default:
				// throw new GrammarException(
				// 	`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${GrammarSymbol[tokenAsSymbol]} (${tokenAsSymbol})`,
				// 	token.line,
				// 	token.column
				// );
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}
}
