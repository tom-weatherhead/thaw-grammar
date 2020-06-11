// tom-weatherhead/thaw-grammar/src/languages/apl/apl-grammar.ts

'use strict';

import { Stack } from 'thaw-common-utilities.ts';

import {
	// LexicalState,
	Token
} from 'thaw-lexical-analyzer';

// import { ExpressionList }  from '../../common/domain-object-model/expression-list';
// import { IExpression }  from '../../common/domain-object-model/iexpression';
// import { Name }  from '../../common/domain-object-model/name';
// import { Variable }  from '../../common/domain-object-model/variable';
// import { VariableList }  from '../../common/domain-object-model/variable-list';

// import { BeginUsage }  from '../../common/domain-object-model/begin-usage';
// import { CondUsage }  from '../../common/domain-object-model/cond-usage';
// import { FunctionDefinition }  from '../../common/domain-object-model/function-definition';
// import { IfUsage }  from '../../common/domain-object-model/if-usage';
// import { LetStarUsage }  from '../../common/domain-object-model/let-star-usage';
// import { LetUsage }  from '../../common/domain-object-model/let-usage';
// import { OperatorUsage }  from '../../common/domain-object-model/operator-usage';
// import { SetUsage }  from '../../common/domain-object-model/set-usage';
// import { WhileUsage }  from '../../common/domain-object-model/while-usage';

// import { ArgumentException } from '../../common/exceptions/argument-exception';
// import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
// import { Production }  from '../../common/production';
import { Symbol } from '../../common/symbol';

export class APLGrammar extends GrammarBase {
	// The Scheme grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		super(Symbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'APL';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	public executeSemanticAction(
		semanticStack: Stack<any>,
		action: string
	): void {}

	public tokenToSymbol(token: Token): number {
		// Returns Symbol
		return Symbol.UndefinedSymbol;
	}

	public pushTokenOntoSemanticStack(
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {}
}
