// tom-weatherhead/thaw-grammar/src/languages/smalltalk/smalltalk-grammar.ts

// import { Stack } from 'thaw-common-utilities.ts';

import {
	GrammarSymbol,
	IToken,
	// LanguageSelector,
	// LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

// import {
// 	// LexicalState,
// 	Token
// } from 'thaw-lexical-analyzer';

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
// import { ParserSelector } from '../../common/parser-selectors';
// import { createProduction }  from '../../common/production';
// import { Symbol } from '../../common/symbol';

export class SmalltalkGrammar extends GrammarBase {
	// The Scheme grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		super(GrammarSymbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'Smalltalk';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {}
	/* eslint-enable @typescript-eslint/no-unused-vars */

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public tokenToSymbol(token: IToken): number {
		// Returns Symbol
		return GrammarSymbol.UndefinedSymbol;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
