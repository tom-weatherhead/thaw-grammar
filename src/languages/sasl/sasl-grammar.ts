// tom-weatherhead/thaw-grammar/src/languages/sasl/sasl-grammar.ts

// import { Stack } from 'thaw-common-utilities.ts';

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	// ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { createProduction } from 'thaw-interpreter-core';

import { ExpressionList } from '../../common/domain-object-model/expression-list';
import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';
// import { Variable }  from '../../common/domain-object-model/variable';
import { VariableList } from '../../common/domain-object-model/variable-list';

// import { ArgumentException } from '../../common/exceptions/argument-exception';
// import { GrammarException } from '../../common/exceptions/grammar-exception';

// import { ParserSelector } from '../../common/parser-selectors';
// import { createProduction } from '../../common/production';
// import { Symbol } from '../../common/symbol';

import { ISExpression } from '../../languages/lisp/domain-object-model/isexpression';

import { SchemeGrammar } from '../../languages/scheme/scheme-grammar';

import { SASLEvaluableExpression } from './domain-object-model/evaluable-expression';
import { SASLLambdaExpression } from './domain-object-model/lambda-expression';
import { SASLPrimOp } from './domain-object-model/prim-op';

export class SASLGrammar extends SchemeGrammar {
	// extends SchemeGrammar
	// The SASL grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		// super(Symbol.nonterminalStart);
		super(true);

		// this.terminals.remove(Symbol.terminalWhile);
		// Terminals.Remove(Symbol.T_Begin);
		// Terminals.Remove(Symbol.T_Print);
		// Terminals.Remove(Symbol.T_Rplaca);
		// Terminals.Remove(Symbol.T_Rplacd);
		// Terminals.Remove(Symbol.T_DefineMacro);
		// this.nonTerminals.remove(Symbol.nonterminalMacroDef);
		// this.removeProductionsContainingSymbol(Symbol.terminalWhile);
		// RemoveProductionsContainingSymbol(Symbol.T_Begin);
		// RemoveProductionsContainingSymbol(Symbol.T_Print);
		// RemoveProductionsContainingSymbol(Symbol.T_Set);
		// RemoveProductionsContainingSymbol(Symbol.T_Rplaca);
		// RemoveProductionsContainingSymbol(Symbol.T_Rplacd);
		// RemoveProductionsContainingSymbol(Symbol.T_If);
		// RemoveProductionsContainingSymbol(Symbol.N_MacroDef);
		// RemoveProductionsContainingSymbol(Symbol.N_ExprPairList);   // This removes the three productions related to cond.

		// this.productions.push(
		// 	new Production(
		// 		Symbol.nonterminalInput,
		// 		[
		// 			Symbol.terminalLeftBracket,
		// 			Symbol.terminalSet,
		// 			Symbol.nonterminalVariable,
		// 			Symbol.nonterminalExpression,
		// 			Symbol.terminalRightBracket,
		// 			'#set'
		// 		],
		// 		70
		// 	)
		// );
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalIf], 71)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCond], 72)
		);
		// ...
	}

	public override get languageName(): string {
		return 'SASL';
	}

	// public override get selectorsOfCompatibleParsers(): ParserSelector[] {
	// 	return [ParserSelector.LL1];
	// }

	public override executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let expression: IExpression<ISExpression>;

		switch (action) {
			case '#evaluableExpression':
				const expressionList = semanticStack.pop() as ExpressionList<ISExpression>;

				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new SASLEvaluableExpression(expression, expressionList));
				break;

			case '#lambdaExpression':
				const body = semanticStack.pop() as IExpression<ISExpression>;
				const argList = semanticStack.pop() as VariableList<ISExpression>;

				// name = semanticStack.pop() as Name;
				// semanticStack.push(new SASLLambdaExpression(argList, body, name.line, name.column));
				semanticStack.push(new SASLLambdaExpression(argList, body));
				break;

			case '#valueOp':
				name = semanticStack.pop() as Name;
				semanticStack.push(new SASLPrimOp(name));
				break;

			default:
				super.executeSemanticAction(semanticStack, action);
				break;
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString = `${token.tokenValue}`;

		switch (token.tokenType) {
			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					case 'while':
					case 'begin':
					case 'print':
					case 'rplaca':
					case 'rplacd':
					case 'define-macro':
						return GrammarSymbol.terminalID;

					default:
						break;
				}

				break;

			default:
				break;
		}

		return super.tokenToSymbol(token);
	}

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalIf:
			case GrammarSymbol.terminalCond:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
				break;
		}
	}
}
